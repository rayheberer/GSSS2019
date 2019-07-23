library(magrittr)
library(totalcensus)

source('~/sfi_project/R/data_processing.R')

tmpdir <- tempdir()
totalcensus::set_path_to_census(tmpdir)

city <- "santa fe, new mexico"
state <- "NM"
county <- 049
osm_tags <- c("motorway", "trunk", "primary", "secondary", "tertiary")

# Import/Process Graph Data -----------------------------------------------

data_sf <- osmdata::opq(bbox = city) %>% 
  osmdata::add_osm_feature(key = "highway", value = osm_tags) %>% 
  osmdata::osmdata_sf() %>% 
  `[[`("osm_lines")

graph <- dodgr::weight_streetnet(data_sf, wt_profile = "motorcar") %>% 
  dodgr::dodgr_contract_graph()

distances <- dodgr::dodgr_distances(graph) %>% 
  trim_disconnected_nodes()

# Import Census Data ------------------------------------------------------

population <- 
  totalcensus::read_decennial(
    year = 2010, 
    states = state, 
    summary_level = "block group", 
  ) %>% 
  dplyr::mutate(GEOID = stringr::str_sub(GEOID, 8L)) %>% 
  dplyr::select(GEOID, population)

income <- tidycensus::get_acs("tract", "B19013_001", state = state, county = county) %>% 
  dplyr::select(GEOID, income = estimate, income_moe = moe)


# Join Census Data to Shapefiles ------------------------------------------

tracts_sf <- tigris::tracts(state, county) %>% 
  sf::st_as_sf()

block_groups_sf <- tigris::block_groups(state, county) %>% 
  sf::st_as_sf()

income_tracts <- tracts_sf %>% 
  dplyr::left_join(income, by = "GEOID") %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -GEOID)

census_data_sf <- block_groups_sf %>% 
  dplyr::left_join(population, by = "GEOID") %>% 
  dplyr::left_join(income_tracts, by = "TRACTCE") %>% 
  dplyr::select(GEOID, population, income, income_moe) %>% 
  dplyr::mutate(area = sf::st_area(geometry), density = population / area)

# Distribute Population to Nodes ------------------------------------------

nodes <- get_nodes_with_census_data(graph, census_data_sf, distances)
