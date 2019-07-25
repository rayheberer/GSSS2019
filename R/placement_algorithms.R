random_placement <- function(distances, p = 1) {
  sample(rownames(distances), size = p, replace = FALSE) %>% 
    as.character()
}

greedy_placement <- function(distances, demands = NULL,
                             objective_fn = mean_minimum_distance, p = 1) {
  
  nodes <- rownames(distances)
  available_nodes <- nodes
  placements <- NULL
  
  for (i in 1:p) {
    best_placement <- available_nodes[1]
    best_score <- Inf
    
    for (node in available_nodes) {
      temp_placements <- c(placements, node)
      temp_score <- objective_fn(distances, temp_placements, demands)
      
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
