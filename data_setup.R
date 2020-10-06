library(here)
source(here("functions.R"))

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
msoa_bounds_url <- "https://opendata.arcgis.com/datasets/1e6f162967de4f3da92040761e8464d8_0.geojson"

msoa_bounds <- sf::st_read(msoa_bounds_url) %>%
  fix_some_areas() %>%
  janitor::clean_names() %>%
  dplyr::semi_join(lookup)



msoas_data <- msoa_bounds %>%
  dplyr::select(msoa11cd, shape_area) %>%
  dplyr::left_join(lookup) %>%
  dplyr::left_join(msoa_pops) %>%
  dplyr::relocate(where(is.character)) %>%
  dplyr::relocate(msoa11cd, .after = msoa11hclnm) %>%
  dplyr::mutate(density = population*1e6/shape_area) %>%
  dplyr::arrange(desc(density)) %>%
  sf::st_transform(crs = 3857)


save_it(msoas_data)
