library(magrittr)
library(totalcensus)

source(here::here('R/data_processing.R'))

options(tigris_use_cache = TRUE)

data_path <- here::here("data/totalcensus")

totalcensus::set_path_to_census(data_path)
tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"))
tigris::tigris_cache_dir(here::here("data/tigris_cache"))
readRenviron('~/.Renviron')
dir.create(data_path, recursive = TRUE, showWarnings = FALSE)

state <- "GA"

# -------------------------------------------------------------------------

population <- 
  totalcensus::read_decennial(
    year = 2010, 
    states = state, 
    summary_level = "block group", 
  ) %>% 
  dplyr::mutate(GEOID = stringr::str_sub(GEOID, 8L)) %>% 
  dplyr::select(GEOID, population)

income <- tidycensus::get_acs("tract", "B19013_001", state = state) %>% 
  dplyr::select(GEOID, income = estimate, income_moe = moe)

tracts_sf <- tigris::tracts(state) %>% 
  sf::st_as_sf()

block_groups_sf <- tigris::block_groups(state) %>% 
  sf::st_as_sf()

income_tracts <- tracts_sf %>% 
  dplyr::left_join(income, by = "GEOID") %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -GEOID)

census_data <- block_groups_sf %>% 
  dplyr::left_join(population, by = "GEOID") %>% 
  dplyr::left_join(income_tracts, by = "TRACTCE") %>% 
  dplyr::select(GEOID, population, income, income_moe) %>% 
  dplyr::mutate(area = sf::st_area(geometry), density = population / area)

saveRDS(census_data, file.path(here::here("data"), paste0(state, "_censusdata.rds")))

rm(list = ls())
gc()
