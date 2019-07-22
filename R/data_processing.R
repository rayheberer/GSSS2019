get_disconnected_nodes <- function(distances) {
  sum_missing <- rowSums(is.na(distances))
  
  sum_missing[sum_missing == length(sum_missing) - 1] %>% 
    names()
}
