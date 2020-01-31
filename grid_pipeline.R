library(magrittr)

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
  sf::st_make_grid(cellsize = grid_res)

grid_centers <- sf::st_centroid(grid) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs = 4326)

# Get Distance Matrix through Routing Engine ------------------------------

setwd(otp_dir)
opentripplanner::otp_setup("otp.jar", ".", router = otp_router, memory = otp_memory)

otp_con <- opentripplanner::otp_connect()

distances <- build_distance_mat(grid_centers, otp_con)

# Weight Grid -------------------------------------------------------------


# Assign Placements -------------------------------------------------------


# Evaluate Placements -----------------------------------------------------


