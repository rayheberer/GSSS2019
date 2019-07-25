library(magrittr)

source('~/sfi_project/R/data_processing.R')
source('~/sfi_project/R/objective_functions.R')
source('~/sfi_project/R/placement_algorithms.R')
source('~/sfi_project/R/simulations_and_sampling.R')

state <- "NM"
county <- "049"

osm_bbox <- "santa fe, new mexico"
osm_tags <- c("motorway", "trunk", "primary", "secondary", 
              "tertiary", "unclassified", "residential",
              "motorway_link", "trunk_link", "primary_link",
              "secondary_link", "tertiary_link", "living_street",
              "service", "pedestrian", "track", "bus_guideway",
              "escape", "raceway", "road")

# Import Data (Streetnet, Census, Stations) -------------------------------

streetnet_sf <- osmdata::opq(bbox = osm_bbox) %>% 
  osmdata::add_osm_feature(key = "highway", value = osm_tags) %>% 
  osmdata::osmdata_sf() %>% 
  `[[`("osm_lines")

graph <- streetnet_sf %>% 
  dodgr::weight_streetnet(wt_profile = "motorcar") %>% 
  dodgr::dodgr_contract_graph()

distances <- dodgr::dodgr_distances(streetnet_graph) %>% 
  trim_disconnected_nodes()

census_data_sf <- readRDS(file.path("data", paste0(state, county, "_censusdata.rds")))

stations_sf <- get_charging_stations(state) %>% 
  sf::st_crop(osmdata::getbb(osm_bbox, format_out = "sf_polygon")) %>% 
  sf::st_transform(sf::st_crs(census_data_sf))

# Join Data, Get Assignments ----------------------------------------------

nodes_census_sf <- join_nodes_with_census_data(
  streetnet_graph, 
  streetnet_distances, 
  census_data_sf
)

placements <- assign_stations_nearest_nodes(nodes_census_sf, stations_sf)

placements_sf <- nodes_census_sf %>% 
  dplyr::filter(id %in% assignments)

# Get Population & Demand Weights -----------------------------------------


# Sample Performance Measures ---------------------------------------------

maximal_distance(distances, placements)

minimal_distance(distances, placements)

range(distances, placements)
