library(magrittr)

source(here::here("R/data_processing.R"))

state <- "GA"
county <- "121"
osm_bbox <- "atlanta, georgia"
utm_epsg <- 32616

grid_res <- 250

min_trip_distance_miles = 0.5

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

survey_place_raw <- readr::read_csv(file.path(data_path, "arc_full_survey", "survey_place.csv"))

survey_household_raw <- readr::read_csv(file.path(data_path, "arc_full_survey", "survey_households.csv"))


# Select Necessary Raw Columns --------------------------------------------

place_cols <- c("sampno", "perno", "plano", "tripno", "travel_date", "arr_time", "dep_time", 
                "lon", "lat", "trip_distance_miles", "origin_lon", "origin_lat")

survey_raw <- survey_place_raw %>% 
  dplyr::select(tidyselect::all_of(place_cols)) %>% 
  dplyr::left_join(
    dplyr::select(survey_household_raw, sampno, income),
    by = "sampno"
  ) %>% 
  dplyr::arrange(sampno, perno, plano, tripno)
  

# !! TESTING: Generate Sample Data for Redacted Columns !! ----------------

lon_min <- bbox[2]
lon_max <- bbox[4]
lat_min <- bbox[1]
lat_max <- bbox[3]

survey_rows <- nrow(survey_raw)

sample_lons <- runif(survey_rows, lon_min, lon_max)
sample_lats <- runif(survey_rows, lat_min, lat_max)

prob_loop <- 0.1

is_loop <- runif(survey_rows) > prob_loop

sample_origin_lons <- ifelse(is_loop, sample_lons, runif(survey_rows, lon_min, lon_max))
sample_origin_lats <- ifelse(is_loop, sample_lats, runif(survey_rows, lat_min, lat_max))

survey_raw <- survey_raw %>% 
  dplyr::mutate(lon = sample_lons, lat = sample_lats, origin_lon = sample_origin_lons, origin_lat = sample_origin_lats)

# Filter for Non-Loop Trips above Minumum Distance ------------------------

survey_filtered_sf <- survey_raw %>% 
  dplyr::filter(is.na(trip_distance_miles) | trip_distance_miles > min_trip_distance_miles) %>% 
  dplyr::filter(!(lon == origin_lon & lat == origin_lat)) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)


# Distribute Demand into Time/Space Buckets -------------------------------

survey_binned <- NULL


# Export Data -------------------------------------------------------------


