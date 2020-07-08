
# libraries ---------------------------------------------------------------

options(conflicts.policy = list(warn.conflicts = FALSE))

library(here, quietly = TRUE)
library(dplyr)
library(glue)
library(purrr)
library(scales)
library(sf, quietly = TRUE)
library(tmap)
library(viridis)


# created in functions.R
msoas_data <- readRDS(here("rds_data/msoas_data.Rds"))

msoas_data <- sf::st_transform(msoas_data, crs = 3857)


# density map just for interest -------------------------------------------

tmap_mode("plot")
density_map <- msoas_data %>%
  tm_shape(name = "MSOAs by density") +
  tm_fill(title = "MSOA population density",
          col = "density",
          alpha = 1,
          palette = "viridis",
          style = "fixed",
          breaks = c(floor(min(msoas_data$density)), 50, 300, 1500, 6000, 12000, ceiling(max(msoas_data$density))))
density_map

total_population <- sum(msoas_data$population)


pop_report <- function(x) {
  x %>%
    sf::st_drop_geometry() %>%
    summarise(sum(population)) %>%
    pull()
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


# start with a seed MSOA --------------------------------------------------


# Pimlico North (highest density)
density_seed <- msoas_data %>%
  slice(1)

# 1,598 MSOAs
densest_quarter_slowly <- build_slowly(density_seed, 1, 0.25)
save_it(densest_quarter_slowly)

tmap_mode("view")
tm_shape(densest_quarter_slowly) +
  tm_fill(col = "density", palette = "-viridis", style = "fixed",
          breaks = c(floor(min(msoas_data$density)), 50, 300, 1500, 6000, 12000, ceiling(max(msoas_data$density))))

tmap_mode("plot")
tm_shape(england) +
  tm_borders() +
  tm_shape(densest_quarter_slowly) +
  tm_fill(col = "density", palette = "-viridis", style = "fixed",
          breaks = c(floor(min(msoas_data$density)), 50, 300, 1500, 6000, 12000, ceiling(max(msoas_data$density))))


dover_seed <- msoas_data %>%
  filter(msoa11hclnm == "Kingsdown & St Margaret's-at-Cliffe")

greenwich_seed <- msoas_data %>%
  filter(msoa11hclnm == "Greenwich & Deptford Creekside")


# Olympic Park & Mill Meads, Newham
pop_seed <- msoas_data %>%
  arrange(desc(population)) %>%
  slice(1)

# Isles of Scilly is smallest population (2,242) but it's non-contiguous...
# second-smallest is Whiston (Rotherham) with 4,883
minpop_seed <- msoas_data %>%
  arrange(population) %>%
  slice(2)

# Bellingham, Otterburn & Redesdale, Northumberland (pop 6,327, 1,128 sq.km)
min_dense_seed <- msoas_data %>%
  arrange(density) %>%
  slice(1)

# this took AGES longer than the most dense version - shape sizes/edge lengths I suppose
# FINAL: 1770 MSOAs (26.1%); area: 115652sq.km; popn.: 13995182 (25%)
emptiest_tenth_slowly2 <- build_slowly(seed = minpop_seed, topslice = 5, fraction = 0.1, max = FALSE)
save_it(emptiest_tenth_slowly2)


emptiest_quarter <- build_block(min_dense_seed, 0.25)
save_it(emptiest_quarter)

tmap_mode("plot")
tm_shape(england) +
  tm_borders() +
  tm_shape(emptiest_tenth_slowly2) +
  tm_fill(col = "density", palette = "-viridis", style = "fixed",
          breaks = c(floor(min(msoas_data$density)), 50, 300, 1500, 6000, 12000, ceiling(max(msoas_data$density))))

  tm_shape(densest_quarter_slowly) +
  tm_fill(col = "density", palette = "-viridis", style = "fixed",
          breaks = c(floor(min(msoas_data$density)), 50, 300, 1500, 6000, 12000, ceiling(max(msoas_data$density))))


# ---------------------------------------------------------------

first_ten <- build_block(seed, 1)


# pop_report(first_ten) - total_population*1/10
# pop_report(head(first_ten, -13)) - total_population*1/10
first_ten <- head(first_ten, -13)

second_ten <- build_block(first_ten, 2)

# pop_report(second_ten) - total_population*2/10
# pop_report(head(second_ten, -103)) - total_population*2/10
second_ten <- head(second_ten, -103)


third_ten <- build_block(second_ten, 3)
pop_report(third_ten) - total_population*3/10
pop_report(head(third_ten, -143)) - total_population*3/10
third_ten <- head(third_ten, -143)


fourth_ten <- build_block(third_ten, 4)
pop_report(fourth_ten) - total_population*4/10
pop_report(head(fourth_ten, -247)) - total_population*4/10
fourth_ten <- head(fourth_ten, -247)

fifth_ten <- build_block(fourth_ten, 5)
pop_report(fifth_ten) - total_population*5/10
pop_report(head(fifth_ten, -88)) - total_population*5/10
fifth_ten <- head(fifth_ten, -88)


dover_half <- build_block(dover_seed, 5)
pop_report(head(dover_half, -49)) - total_population/2
dover_half <- head(dover_half, -49)

tm_shape(england) +
  tm_borders() +
  tm_shape(dover_half) +
  tm_fill(col = viridis(10)[8])

greenwich_half <- build_block(greenwich_seed, 5)
pop_report(head(greenwich_half, -14)) - total_population/2
greenwich_half <- head(greenwich_half, -14)


tm_shape(england) +
  tm_borders() +
  tm_shape(greenwich_half) +
  tm_fill(col = viridis(10)[6])




england <- sf::st_union(msoas_data)
tmap_mode("plot")

tm_shape(england) +
  tm_borders() +
tm_shape(fifth_ten) +
  tm_fill(col = viridis(10)[8])
tm_shape(fourth_ten) +
  tm_fill(col = viridis(10)[2]) +
tm_shape(third_ten) +
  tm_fill(col = viridis(10)[9]) +
tm_shape(second_ten) +
  tm_fill(col = viridis(10)[1]) +
tm_shape(first_ten) +
  tm_fill(col = viridis(10)[10])

tmap_mode("view")
tm_shape(new_layer) +
  tm_polygons(alpha = 0, border.col = "grey10", popup.vars = c("Population" = "population", "Population density" = "density"))







