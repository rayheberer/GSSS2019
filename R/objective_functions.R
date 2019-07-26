maximal_distance <- function(distances, placements) {
  dist_indexed <- distances[, placements, drop = FALSE]
  
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  max(min_distances[is.finite(min_distances)], na.rm = TRUE)
}

minimal_distance <- function(distances, placements) {
  dist_indexed <- distances[, placements, drop = FALSE]
  dist_indexed[dist_indexed == 0] <- Inf
  
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  min(min_distances)
}

range <- function(distances, placements) {
  maximal_distance(distances, placements) - minimal_distance(distances, placements)
}

mean_minimal_distance <- function(distances, placements, weights = NULL) {
  dist_indexed <- distances[, placements, drop = FALSE]
  
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  denom <- nrow(distances)
  
  if (!is.null(weights)) {
    min_distances <- min_distances * weights
    denom <- sum(weights)
  }
  
  sum(min_distances[is.finite(min_distances)]) / denom
}

variance_minimal_distance <- function(distances, placements, weights = NULL) {
  dist_indexed <- distances[, placements, drop = FALSE]
  
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  mean_min_distances <- mean_minimal_distance(distances, placements, weights)
  
  diffs <- (min_distances - mean_min_distances)^2
  denom <- nrow(distances) - 1
  
  if (!is.null(weights)) {
    diffs <- diffs * weights
    denom <- sum(weights) - 1
  }
  
  sum(diffs[is.finite(diffs)]) / denom
}

gini_coefficient <- function(distances, placements, weights) {
  dist_indexed <- distances[, placements, drop = FALSE]
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  weights <- weights[is.finite(min_distances)]
  min_distances <- min_distances[is.finite(min_distances)]
  
  result <- 0
  for (i in 1:length(min_distances)) {
    for (j in 1:length(min_distances)) {
      result <- result + (weights[i] * weights[j] * abs(min_distances[i] - min_distances[j]))
    }
  }
  
  result / (2 * sum(weights)^2 * mean_minimal_distance(distances, placements))
}

mean_deviation <- function(distances, placements, weights) {
  dist_indexed <- distances[, placements, drop = FALSE]
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  mean_min_distances <- mean_minimal_distance(distances, placements, weights)
  deviation <- weights * abs(min_distances - mean_min_distances)
  
  sum(deviation[is.finite(deviation)]) / sum(weights)
}

hoover_concentration_index <- function(distances, placements, weights) {
  dist_indexed <- distances[, placements, drop = FALSE]
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  node_aggregate_travel <- min_distances * weights
  graph_aggregate_travel <- sum(node_aggregate_travel)
  total_weight <- sum(weights)
  
  diffs <- (node_aggregate_travel / graph_aggregate_travel) - (weights / total_weight)
  
  sum(abs(diffs[is.finite(diffs)])) / 2
}

moran_i <- function(distances, placements, weights, neighborhood_radius) {
  dist_indexed <- distances[, placements, drop = FALSE]
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  min_distances <- min_distances[is.finite(min_distances)]
  
  mean_V <- mean_minimal_distance(distances, placements)
  
  num <- 0
  w_sum <- 0
  
  for (i in 1:length(min_distances)) {
    for (j in 1:length(min_distances)) {
      if (is.na(distances[i, j])) {
        next
      }
      
      if (distances[i, j] < neighborhood_radius) {
        num <- num + ((min_distances[i] - mean_V) * (min_distances[j] - mean_V))
        w_sum <- w_sum + 1
      }
    }
  }
  
  denom <- sum((min_distances - mean_V)^2)
  
  (length(min_distances) / w_sum) * (num / denom)
}

theil_entropy_index <- function(distances, placements, weights) {
  dist_indexed <- distances[, placements, drop = FALSE]
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  weights <- weights[is.finite(min_distances) & min_distances > 0]
  min_distances <- min_distances[is.finite(min_distances) & min_distances > 0]
  
  node_aggregate_travel <- min_distances * weights
  graph_aggregate_travel <- sum(node_aggregate_travel)
  
  entropy <- (min_distances / graph_aggregate_travel) * 
    log(min_distances / graph_aggregate_travel)
  
  log(length(min_distances)) + sum(weights * entropy)
}

gini_coefficient_vectorized <- function(distances, placements, weights) {
  dist_indexed <- distances[, placements, drop = FALSE]
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  weights <- weights[is.finite(min_distances)]
  min_distances <- min_distances[is.finite(min_distances)]
  
  min_distance_matrix <- as.matrix(dist(min_distances))
  weights_X <- matrix(rep(weights, length(weights)), nrow = length(weights))
  
  num <- sum(min_distance_matrix * weights_X * t(weights_X))
  
  num / (2 * sum(weights)^2 * mean_minimal_distance(distances, placements))
}

moran_i_vectorized <- function(distances, placements, weights, neighborhood_radius) {
  dist_indexed <- distances[, placements, drop = FALSE]
  min_distances <- suppressWarnings(apply(dist_indexed, 1, min, na.rm = TRUE))
  
  weight_matrix <- 
    distances[is.finite(min_distances), is.finite(min_distances)] < neighborhood_radius
  
  weight_matrix[is.na(weight_matrix)] <- 0

  min_distances <- min_distances[is.finite(min_distances)]
  
  mean_V <- mean_minimal_distance(distances, placements)
  
  diffs_X <- matrix(rep(min_distances - mean_V, length(min_distances)), 
                    nrow = length(min_distances))
  
  (length(min_distances) / sum(weight_matrix)) * 
    (sum(weight_matrix * diffs_X * t(diffs_X)) / sum((min_distances - mean_V)^2))
}
