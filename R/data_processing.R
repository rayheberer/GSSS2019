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

get_nodes_with_populations <- function(graph, population_sf, distances) {
  nodes <- dodgr::dodgr_vertices(graph) %>% 
    dplyr::filter(id %in% rownames(distances)) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(population_sf))
  
  covers <- sf::st_covers(population_sf, nodes)
  names(covers) <- population_sf$GEOID
  
  nodes_in_polygon <- purrr::map_df(covers, length) %>% 
    tidyr::gather("GEOID", "num_nodes")
    
  covered_by <- sf::st_covered_by(nodes, population_sf) %>% 
    tibble::as_tibble() %>% 
    dplyr::transmute(id = nodes$id, GEOID = population_sf$GEOID[col.id])
  
  population_flat <- tibble::as_tibble(population_sf) %>% 
    dplyr::select(-geometry)
  
  nodes %>% 
    dplyr::left_join(covered_by, by = "id") %>% 
    dplyr::left_join(nodes_in_polygon, by = "GEOID") %>% 
    dplyr::left_join(data.frame(population_flat), by = "GEOID")
}
