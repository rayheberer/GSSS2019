library(magrittr)

source(here::here("R/data_processing.R"))

state <- "GA"
county <- "121"
osm_bbox <- "atlanta, georgia"
utm_epsg <- 32616

grid_res <- 500

otp_dir <- "~/Documents/otp"
otp_router <- "georgia"
otp_memory <- 10240

data_path <- here::here("data")


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

setwd(otp_dir)
opentripplanner::otp_setup("otp.jar", ".", router = otp_router, memory = otp_memory)

otp_con <- opentripplanner::otp_connect(router = otp_router)

valid_nodes <- get_valid_nodes(grid_centers, otp_con)

# saveRDS(valid_nodes, "~/Documents/R/GSSS2019/data/valid_nodes_georgia_500m.rds")
# valid_nodes <- readRDS("~/Documents/R/GSSS2019/data/valid_nodes_georgia_500m.rds")

routing_df <- get_routing_df(grid_centers, otp_con, valid_nodes[1:10], get_geometry = FALSE, ncores = 3)

distances <- routing_df %>% 
  dplyr::select(fromPlace, toPlace, distance) %>% 
  dplyr::mutate_all(as.numeric) %>% 
  dplyr::arrange(toPlace) %>% 
  tidyr::pivot_wider(names_from = toPlace, values_from = distance) %>% 
  dplyr::arrange(fromPlace)

# Weight Grid -------------------------------------------------------------

census_data_sf <- readRDS(file.path(data_path, paste0(state, county, "_censusdata.rds"))) %>% 
  sf::st_transform(sf::st_crs(grid_centers))

nodes <- grid_centers %>% 
  dplyr::filter(id %in% names(distances))

nodes_census_sf <- join_nodes_with_census_data(
  nodes, 
  census_data_sf
)


# Assign Placements -------------------------------------------------------

stations_sf <- get_charging_stations(state) %>% 
  sf::st_crop(osmdata::getbb(osm_bbox, format_out = "sf_polygon")) %>% 
  sf::st_transform(sf::st_crs(grid_centers))

placements <- assign_stations_nearest_nodes(nodes_census_sf, stations_sf)


# Evaluate Placements -----------------------------------------------------


