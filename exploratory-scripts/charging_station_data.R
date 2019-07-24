api_key <- Sys.getenv("NREL_API_KEY")
url <- "https://developer.nrel.gov/api/alt-fuel-stations/v1.json?"

state <- "NM"


# Get Stations from API ---------------------------------------------------

response <- httr::GET(
  url, 
  query = list(
    api_key = api_key, 
    fuel_type = "ELEC",
    state = state
  )
)

content <- httr::content(response)


# Convert to Spatial Object -----------------------------------------------

longitudes <- content$fuel_stations %>% 
  purrr::map_dbl("longitude")

latitudes <- content$fuel_stations %>% 
  purrr::map_dbl("latitude")

stations_sf <- data.frame(x = longitudes, y = latitudes) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)
