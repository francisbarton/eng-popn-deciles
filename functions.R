
# libraries ---------------------------------------------------------------

options(conflicts.policy = list(warn.conflicts = FALSE))

library(here, quietly = TRUE)
library(dplyr)
library(janitor)
library(nomisr)
library(purrr)
library(readr)
library(sf, quietly = TRUE)


# borrow some functions from elsewhere ------------------------------------
# including work I've done on other projects

get_lookup <- function() {

  # geo lookups to help get area codes
  drkane_url <- "https://github.com/drkane/geo-lookups/raw/master/msoa_la.csv"

  destfile <- basename(drkane_url)

  # check to see if the file already exists
  if(!file.exists(destfile)) {
    utils::download.file(drkane_url, destfile, quiet = TRUE)
  }

  readr::read_csv(destfile) %>%
    janitor::clean_names()
}


make_batched_list <- function(x, batch_size = 1000) {

  if(is.list(x)) {
    x <- unlist(x)
  }

  rep(1:ceiling(length(x)/batch_size), each = batch_size) %>%
    head(length(x)) %>%
    split(x, .)
}


retrieve_popn_data <- function(x) {
  nomisr::nomis_get_data(
    id = "NM_2010_1", # Nomis dataset code
    geography = x, # pass each area code from each batch of the split list to the query
    gender = 0, # all
    date = "latest",
    measures = 20100, # people not %s
    c_age = 200, # all ages (check!)
    select = c("GEOGRAPHY", "C_AGE_NAME", "C_AGE_CODE", "OBS_VALUE"),
    tidy = TRUE) %>%

    rename(msoa11cd = geography,
           population = obs_value)
}



# fixes invalid areas from an sf data frame and returns a valid whole data frame
fix_some_areas <- function(x) {

  # st_is_valid(x) %>% summary

  to_fix <- which(!sf::st_is_valid(x))
  y <- 1:nrow(x)
  valid <- y[!is.element(y, to_fix)]

  # map_df doesn't work. Needs rbind.sf (below) instead.
  fixed_areas <- map(to_fix, ~ slice(.data = x, .) %>% sf::st_make_valid())

  # st_is_valid(fixed_areas) %>% summary

  # thanks to https://github.com/r-spatial/sf/issues/798
  fixed_areas_df <- purrr::reduce(fixed_areas, sf:::rbind.sf)

  # valid_list <- which(sf::st_is_valid(x))
  # msoa_bounds_valid <- x[valid_list, ]
  valid_areas <- slice(x, valid)

  sf:::rbind.sf(valid_areas, fixed_areas_df)
}



save_it <- function(x, dir = "rds_data") {

  dirhere <- here::here(dir)
  filenm <- paste0(deparse(rlang::enexpr(x)), ".Rds") # phwoar

  if (!dir.exists(dirhere)) {
    dir.create(dirhere)
  }

  saveRDS(x, here::here(dir, filenm))
}



# use st_touches to pull adjacent areas out ---------------------------

landgrab <- function(seed) {

  remainder <- msoas_data %>%
    filter(!msoa11cd %in% seed$msoa11cd)


  new_index <- sf::st_touches(seed, remainder) %>%
    unlist() %>%
    unique() %>%
    sample() # randomise so that initial shape irregularities are not amplified

  if (length(new_index) == 0) {
    break
  }

  else {
    new_layer <- remainder %>%
      slice(new_index)

    sf:::rbind.sf(seed, new_layer)
  }
}


landgrab_slowly <- function(seed, topslice, max = TRUE, focus_var = density) {

  remainder <- msoas_data %>%
    filter(!msoa11cd %in% seed$msoa11cd)

  new_index <- sf::st_touches(seed, remainder) %>%
    unlist() %>%
    unique() # %>%
  # sample() # randomise so that initial shape irregularities are not amplified (irrelevant now doing it slowly (below))

  if (length(new_index) == 0) {
    break
  }

  else if (max) {
    new_layer <- remainder %>%
      slice(new_index) %>%
      slice_max(order_by = {{ focus_var }}, n = topslice)
  }

  else {
    new_layer <- remainder %>%
      slice(new_index) %>%
      slice_min(order_by = {{ focus_var }}, n = topslice)
  }

  sf:::rbind.sf(seed, new_layer)
}

# a function to build the block ---------------------------------------

build_block <- function(seed, fraction) {

  pop <- pop_report(seed)
  n <- 0

  while (pop < (total_population*fraction)) {

    seed <- landgrab(seed)
    pop <- pop_report(seed)
    n <- nrow(seed)
    area <- sum(pull(seed, st_areashape))

    if (n %% 100 == 0) {
      usethis::ui_info(
        glue::glue("{n} MSOAs ({round(n*100/nrow(msoas_data), 1)}%); area: {comma(round(area/1e6))} sq.km; popn.: {comma(pop)} ({round(pop*100/total_population, 2)}%)"))
    }
  }

  usethis::ui_info(
    glue::glue("FINAL: {n} MSOAs ({round(n*100/nrow(msoas_data), 1)}%); area: {comma(round(area/1e6))} sq.km; popn.: {comma(pop)} ({round(pop*100/total_population, 2)}%)"))

  seed

}

build_slowly <- function(seed, topslice, fraction, ...) {

  pop <- pop_report(seed)
  n <- 0

  while (pop < (total_population*fraction)) {

    seed <- landgrab_slowly(seed, topslice, ...)
    pop <- pop_report(seed)
    n <- nrow(seed)
    area <- sum(pull(seed, st_areashape))

    if (n %% 100 == 0) {
      usethis::ui_info(
        glue::glue("{n} MSOAs ({round(n*100/nrow(msoas_data), 1)}%); area: {comma(round(area/1e6))} sq.km; popn.: {comma(pop)} ({round(pop*100/total_population, 2)}%)"))
    }
  }

  usethis::ui_info(
    glue::glue("FINAL: {n} MSOAs ({round(n*100/nrow(msoas_data), 1)}%); area: {comma(round(area/1e6))} sq.km; popn.: {comma(pop)} ({round(pop*100/total_population, 2)}%)"))

  seed
}
