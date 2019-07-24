mean_min_distance <- function(distances, placements, demands = NULL) {
  
  distances_to_placements <- distances[, placements, drop = FALSE]
  
  if (!is.null(demands)) {
    distances_to_placements <- distances_to_placements * demands[placements]
  }
  
  min_distances <- apply(distances_to_placements, 1, min, na.rm = TRUE)
  
  mmd <- sum(min_distances[is.finite(min_distances)]) / 
    (nrow(distances) - length(placements))
  
  ifelse(mmd == 0, Inf, mmd)
}

maximal_distance <- function(distances, placements) {
  distances_to_placements <- distances[, placements, drop = FALSE]
  
  max_distances <- apply(distances_to_placements, 1, max, na.rm = TRUE)
  
  max(max_distances)
}
