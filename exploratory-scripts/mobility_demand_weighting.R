library(magrittr)

source(here::here("R/data_processing.R"))

state <- "GA"
county <- "121"
osm_bbox <- "atlanta, georgia"
utm_epsg <- 32616

grid_res <- 250

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


# Read in Survey Data -----------------------------------------------------

survey_place_table <- readr::read_csv(file.path(data_path, "arc_full_survey", "survey_place.csv"))

survey_household_table <- readr::read_csv(file.path(data_path, "arc_full_survey", "survey_households.csv"))


# !! TESTING: Generate Sample Data for Redacted Columns !! ----------------


# Filter for Non-Loop Trips above Minumum Distance ------------------------


# Distribute Demand into Time/Space Buckets -------------------------------


# Export Data -------------------------------------------------------------


