library(magrittr)
library(totalcensus)

source('~/sfi_project/R/data_processing.R')

tmpdir <- tempdir()
totalcensus::set_path_to_census(tmpdir)

city <- "santa fe, new mexico"
state_abbr <- "NM"
county_fips <- 049
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

population <- totalcensus::read_decennial(
    year = 2010, 
    states = state_abbr,
    summary_level = "block group"
  ) %>% 
  dplyr::mutate(GEOID = stringr::str_sub(GEOID, 8L)) %>% 
  dplyr::select(GEOID, population)

block_groups_sf <- tigris::block_groups(state_abbr, county_fips) %>% 
  sf::st_as_sf()

population_sf <- block_groups_sf %>% 
  dplyr::left_join(population) %>% 
  dplyr::select(GEOID, population) %>% 
  dplyr::mutate(area = sf::st_area(geometry), density = population / area)

# Distribute Population to Nodes ------------------------------------------
nodes <- dodgr::dodgr_vertices(graph) %>% 
  dplyr::filter(id %in% rownames(distances)) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(population_sf))

covers <- sf::st_covers(population_sf, nodes)


