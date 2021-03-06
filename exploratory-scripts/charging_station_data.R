library(magrittr)
library(totalcensus)
library(mapview)

source('~/sfi_project/R/data_processing.R')

state <- "NM"
county <- 049

osm_bbox <- "santa fe, new mexico"
osm_tags <- c("motorway", "trunk", "primary", "secondary", 
              "tertiary", "unclassified", "residential",
              "motorway_link", "trunk_link", "primary_link",
              "secondary_link", "tertiary_link", "living_street",
              "service", "pedestrian", "track", "bus_guideway",
              "escape", "raceway", "road")

# Import Data (RoadNet, Census, Stations) ---------------------------------
streetnet_sf <- osmdata::opq(bbox = osm_bbox) %>% 
  osmdata::add_osm_feature(key = "highway", value = osm_tags) %>% 
  osmdata::osmdata_sf() %>% 
  `[[`("osm_lines")

streetnet_graph <- streetnet_sf %>% 
  dodgr::weight_streetnet(wt_profile = "motorcar") %>% 
  dodgr::dodgr_contract_graph()

streetnet_distances <- dodgr::dodgr_distances(streetnet_graph) %>% 
  trim_disconnected_nodes()

census_data_sf <- import_join_population_income_data(state, county, save_path = "data")

stations_sf <- get_charging_stations(state) %>% 
  sf::st_crop(osmdata::getbb(osm_bbox, format_out = "sf_polygon")) %>% 
  sf::st_transform(sf::st_crs(census_data_sf))

# Join StreetNet/Census Data ----------------------------------------------

nodes_census_sf <- join_nodes_with_census_data(
  streetnet_graph, 
  streetnet_distances, 
  census_data_sf
)

# Assign Stations to StreetNet Nodes --------------------------------------

assignments <- assign_stations_nearest_nodes(nodes_census_sf, stations_sf)

assignments_sf <- nodes_census_sf %>% 
  dplyr::filter(id %in% assignments)

mapview(assignments_sf, color = "red") + 
  mapview(stations_sf, color = "green") + 
  mapview(nodes_census_sf)

mapview(assignments_sf, color = "red") + 
  mapview(stations_sf, color = "green") + 
  mapview(streetnet_sf)
