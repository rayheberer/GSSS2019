library(magrittr)

city <- "santa fe, new mexico"

#osm_tags <- c("motorway", "trunk", "primary", "secondary",
#              "tertiary", "unclassified", "residential")

osm_tags <- c("motorway", "trunk", "primary", "secondary", "tertiary")

# Import OSM Data ---------------------------------------------------------

query <- osmdata::opq(bbox = city) %>% 
  osmdata::add_osm_feature(key = "highway", value = osm_tags)

data_raw <- osmdata::osmdata_sf(query)


# Get Distance Matrix -----------------------------------------------------

distances <- sf::as_Spatial(data_raw$osm_points) %>% 
  osrm::osrmTable()

# Convert to Graph Object -------------------------------------------------


