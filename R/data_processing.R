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
