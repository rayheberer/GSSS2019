generate_random_demand_ranges <- function(distances, center_min = 1, 
                                          center_max = 2, spread_coef = 1) {
  
  nodes <- rownames(distances)
  
  n <- length(nodes)
  
  expected_demands <- runif(n, center_min, center_max)
  
  interval_range <- runif(n, max = spread_coef * center_min)
  
  lower_demands <- expected_demands - interval_range
  upper_demands <- expected_demands + interval_range

  names(lower_demands) <- nodes
  names(upper_demands) <- nodes
  
  list("lower" = lower_demands, "upper" = upper_demands)
}

sample_performance_measure <- function(distances, placements, lower_demands, upper_demands,
                                       performance_fn = mean_min_distance, n = 10000) {
  
  1:n %>% 
    purrr::map_dbl(
      function(x) {
        purrr::map2_dbl(lower_demands, upper_demands, ~runif(1, .x, .y)) %>% 
          performance_fn(distances, placements, .)
      }
    )

}
