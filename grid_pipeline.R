library(magrittr)
library(raster)

state <- "GA"
county <- "121"
osm_bbox <- "atlanta, georgia"

grid_res <- 100

data_path <- here::here("data")



# Get Boundary Polygon ----------------------------------------------------

bbox <- osmdata::opq(bbox = osm_bbox)$bbox %>% 
  stringr::str_split(",") %>% 
  purrr::pluck(1)


# Lay Down Grid -----------------------------------------------------------

extent <- osmdata::opq(bbox = osm_bbox)$bbox %>% 
  stringr::str_split(",") %>% 
  purrr::pluck(1) %>% 
  as.numeric() %>% 
  `[`(c(2, 4, 1, 3)) %>% 
  raster::extent()

grid <- raster::raster(
  x = extent, 
  crs = sf::st_crs(4326)$proj4string, 
  ) %>% 
  raster::setValues(runif(raster::ncell(.)))

# Get Distance Matrix through Routing Engine ------------------------------


# Weight Grid -------------------------------------------------------------


# Assign Placements -------------------------------------------------------


# Evaluate Placements -----------------------------------------------------


