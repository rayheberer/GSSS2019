random_placement <- function(distances, p = 1) {
  sample(rownames(distances), size = p, replace = FALSE) %>% 
    as.character()
}

greedy_placement <- function(distances, demands = NULL,
                             objective_fn = mean_minimal_distance, p = 1) {
  
  nodes <- rownames(distances)
  available_nodes <- nodes
  placements <- NULL
  
  for (i in 1:p) {
    
    message(paste("p:", i))
    
    best_placement <- available_nodes[1]
    best_score <- Inf
    
    j <- 0
    for (node in available_nodes) {
      temp_placements <- c(placements, node)
      temp_score <- objective_fn(distances, temp_placements, demands)
      
      j <- j + 1
      if (j %% 100 == 0) {
        message(j)
      }
      
      
      if (temp_score < best_score) {
        best_placement <- node
        best_score <- temp_score
      }
    }
    
    placements <- c(placements, best_placement)
    available_nodes <- available_nodes[available_nodes != best_placement]
  }
  
  placements
}

greedy_placement_vectorized <- function(distances, weights = NULL, p = 1) {
  placements <- NULL
  n <- nrow(distances)
  
  distances[is.na(distances)] <- mean(distances, na.rm = TRUE)
  
  if (!is.null(weights)) {
    distances <- distances * weights
  }
  
  for (i in 1:p) {
    message(i)
    min_col <- which.min(colSums(distances, na.rm = TRUE))
    placements <- c(placements, colnames(distances)[min_col])
    
    min_mat <- distances[, min_col, drop = FALSE]
    min_mat <- matrix(rep(min_mat, n - i), ncol = n - i)
    
    distances <- distances[, -min_col, drop = FALSE]
    is_less <- distances < min_mat
    
    distances <- (distances * is_less) + (min_mat * !is_less)
  }
  
  placements
}
