
# libraries ---------------------------------------------------------------

options(conflicts.policy = list(warn.conflicts = FALSE))

library(here, quietly = TRUE)
library(dplyr)
library(janitor)
# library(jsonlite)
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
  filenm <- paste0(deparse(enexpr(x)), ".Rds")

  if (!dir.exists(dirhere)) {
    dir.create(dirhere)
  }

  saveRDS(x, here::here(dir, filenm))
}



# pull together MSOA codes and population data ----------------------------

lookup <- get_lookup() %>%
  filter(ctrynm == "England") %>%
  select(msoa11cd, msoa11hclnm, lad20nm, rgnnm)


msoa_pops <- lookup %>%
  pull(msoa11cd) %>%
  make_batched_list() %>%
  map_df( ~ retrieve_popn_data(.)) %>%
  select(msoa11cd, population)


# get MSOA boundaries -----------------------------------------------------

# Generalised (BGC), not full resolution (BFC)
# BGC https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bgc
msoa_bounds_url <- "https://opendata.arcgis.com/datasets/29fdaa2efced40378ce8173b411aeb0e_2.geojson"

msoa_bounds <- sf::st_read(msoa_bounds_url) %>%
  dplyr::semi_join(lookup) %>%
  select(msoa11cd, st_areashape) %>%
  fix_some_areas()


msoas_data <- msoa_bounds %>%
  left_join(lookup) %>%
  left_join(msoa_pops) %>%
  relocate(where(is.character)) %>%
  relocate(msoa11cd, .after = msoa11hclnm) %>%
  mutate(density = population*1e6/st_areashape) %>%
  arrange(desc(density))


save_it(msoas_data)
