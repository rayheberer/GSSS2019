library(magrittr)

city <- "santa fe, new mexico"

osm_tags <- c("motorway", "trunk", "primary", "secondary", "tertiary")

# Import OSM Data ---------------------------------------------------------

query <- osmdata::opq(bbox = city) %>% 
  osmdata::add_osm_feature(key = "highway", value = osm_tags)

data_raw <- osmdata::osmdata_sf(query)


# Get Distance Matrix -----------------------------------------------------

lat_long <- data_raw$osm_points["geometry"] %>% 
  sf::st_coordinates() %>% 
  tibble::as_tibble() %>% 
  dplyr::transmute(LAT_LONG = paste(as.character(Y), as.character(X), sep = "+"))

distances <- lat_long$LAT_LONG %>% 
  gmapsdistance::gmapsdistance(origin = ., destination = ., mode = "driving")

# Convert to Graph Object -------------------------------------------------


