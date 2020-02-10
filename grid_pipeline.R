library(magrittr)

source(here::here("R/data_processing.R"))
source(here::here("R/objective_functions.R"))

state <- "GA"
county <- "121"
osm_bbox <- "atlanta, georgia"
utm_epsg <- 32616

grid_res <- 250

data_path <- here::here("data")
routing_df_batches_path <- file.path(data_path, glue::glue("routing_df_batches_{state}{grid_res}"))


# Get Boundary Polygon ----------------------------------------------------

bbox <- osmdata::opq(bbox = osm_bbox)$bbox %>% 
  stringr::str_split(",") %>% 
  purrr::pluck(1) %>% 
  as.numeric()

bbox_lon <- bbox[c(2, 4)]

bbox_lat <- bbox[c(1, 3)]

boundary_points <- data.frame(x = bbox_lon, y = bbox_lat) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  sf::st_transform(utm_epsg)


# Lay Down Grid -----------------------------------------------------------

grid <- boundary_points %>% 
  sf::st_make_grid(cellsize = grid_res) %>% 
  sf::st_as_sf() %>% 
  dplyr::mutate(id = as.character(dplyr::row_number()))

grid_centers <- sf::st_centroid(grid) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs = 4326)

# Get Distance Matrix through Routing Engine ------------------------------

routing_df <- import_bind_routing_df_batches(routing_df_batches_path)

distances <- routing_df %>% 
  dplyr::mutate_all(as.numeric) %>% 
  dplyr::arrange(toPlace) %>% 
  tidyr::pivot_wider(names_from = toPlace, values_from = distance) %>% 
  dplyr::arrange(fromPlace)

# Weight Grid -------------------------------------------------------------

census_data_sf <- readRDS(file.path(data_path, paste0(state, "_censusdata.rds"))) %>% 
  sf::st_transform(sf::st_crs(grid_centers))

nodes <- grid_centers %>% 
  dplyr::filter(id %in% names(distances))

nodes_census_sf <- join_nodes_with_census_data(
  nodes, 
  census_data_sf
)

# TODO: figure out why GEOIDs have multiple incomes, aggregate or deduplicate as needed

nodes_census_sf <- nodes_census_sf %>% 
  dplyr::distinct(GEOID, .keep_all = TRUE)

weights <- nodes_census_sf %>% 
  dplyr::mutate(weights = population / num_nodes) %>% 
  dplyr::pull(weights) %>% 
  setNames(nodes_census_sf$id)


# Assign Placements -------------------------------------------------------

stations_sf <- get_charging_stations(state) %>% 
  sf::st_crop(osmdata::getbb(osm_bbox, format_out = "sf_polygon")) %>% 
  sf::st_transform(sf::st_crs(grid_centers))

placements <- assign_stations_nearest_nodes(nodes_census_sf, stations_sf)  
# TODO: aggregate somehow when multiple stations assigned to the same node


# Evaluate Placements -----------------------------------------------------


