library(magrittr)
library(sf)
library(mapview)
source('~/sfi_project/R/objective_functions.R')

p <- 3

# Import Data -------------------------------------------------------------

data_raw <- dodgr::hampi

graph <- dodgr::weight_streetnet(data_raw, wt_profile = "motorcar") %>% 
  dodgr::dodgr_sample(nverts = 500)

distances <- dodgr::dodgr_distances(graph)

disconnected_nodes <- get_disconnected_nodes(distances)

distances <- distances[!rownames(distances) %in% disconnected_nodes,
                       !colnames(distances) %in% disconnected_nodes]

# Accessibility Calculation -----------------------------------------------

# random placement
stations <- greedy_placement(distances, p = 6)

mean_min_distance(distances, stations)

# Visualizing Results -----------------------------------------------------

network_sf <- dodgr::dodgr_to_sf(graph)

station_sf <- graph %>% 
  dplyr::filter(from_id %in% stations) %>% 
  dplyr::distinct(from_id, .keep_all = TRUE) %>% 
  dplyr::select(from_lon, from_lat) %>% 
  sf::st_as_sf(coords = c("from_lon", "from_lat"), crs = sf::st_crs(network_sf))

map <- mapview(network_sf) + mapview(station_sf)
mapview::addStaticLabels(
  map, 
  station_sf[1, ],
  mean_min_distance(distances, stations)
)

# Examine Multiple Configurations -----------------------------------------

