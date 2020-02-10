library(magrittr)

trim_disconnected_nodes <- function(distances) {
  trimmed <- FALSE
  
  while (!trimmed) {
    sum_missing <- rowSums(is.na(distances))
    trim <- names(sum_missing[sum_missing == length(sum_missing) - 1])
    
    if (length(trim) > 0) {
      distances <- distances[!rownames(distances) %in% trim, !colnames(distances) %in% trim]
    } else {
      trimmed <- TRUE
    }
  }
  
  distances
}

join_nodes_with_census_data <- function(nodes, census_data_sf) {

  covers <- sf::st_covers(census_data_sf, nodes)
  names(covers) <- census_data_sf$GEOID
  
  nodes_in_polygon <- purrr::map_df(covers, length) %>% 
    tidyr::gather("GEOID", "num_nodes")
    
  covered_by <- sf::st_covered_by(nodes, census_data_sf) %>% 
    tibble::as_tibble() %>% 
    dplyr::distinct(row.id, .keep_all = TRUE) %>% 
    dplyr::transmute(id = nodes$id, GEOID = census_data_sf$GEOID[col.id])
  
  population_flat <- tibble::as_tibble(census_data_sf) %>% 
    dplyr::select(-geometry)
  
  nodes %>% 
    dplyr::left_join(covered_by, by = "id") %>% 
    dplyr::left_join(nodes_in_polygon, by = "GEOID") %>% 
    dplyr::left_join(data.frame(population_flat), by = "GEOID")
}

get_charging_stations <- function(state, crs = 4326) {
  api_key <- Sys.getenv("NREL_API_KEY")
  url <- "https://developer.nrel.gov/api/alt-fuel-stations/v1.json?"
  
  content <- url %>% 
    httr::GET(query = list(api_key = api_key, fuel_type = "ELEC", state = state)) %>% 
    httr::content()
  
  longitudes <- content$fuel_stations %>% 
    purrr::map_dbl("longitude")
  
  latitudes <- content$fuel_stations %>% 
    purrr::map_dbl("latitude")
  
  data.frame(x = longitudes, y = latitudes) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = crs)
}

assign_stations_nearest_nodes <- function(nodes_sf, stations_sf,
                                          node_id_col = "id") {
  
  station_nodes_dists <- sf::st_distance(stations_sf, nodes_census_sf)
  
  station_rows <- apply(station_nodes_dists, 1, which.min)
  
  nodes_sf[[node_id_col]][station_rows]
}

clear_tempdir <- function() {
  list.files(tempdir(), full.names = TRUE) %>% 
    unlink(recursive = TRUE)
}

import_bind_routing_df_batches <- function(batch_dir) {
  batch_files <- list.files(batch_dir, full.names = TRUE)
  

  batch_files %>% 
    purrr::map(readRDS) %>% 
    purrr::map(tibble::as_tibble) %>% 
    purrr::map(~dplyr::select(., fromPlace, toPlace, distance)) %>% 
    dplyr::bind_rows()
}
