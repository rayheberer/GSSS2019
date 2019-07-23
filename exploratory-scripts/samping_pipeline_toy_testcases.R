library(igraph)

# Construct Toy Test Graphs -----------------------------------------------
star <- igraph::make_star(8)

distances <- igraph::distances(star) %>% 
  `colnames<-`(as.character(1:8)) %>% 
  `rownames<-`(as.character(1:8))

# Run Placement Algorithm -------------------------------------------------
placements <- "1"

demands <- list("lower" = rep(1, 8), "upper" = rep(1, 8)) %>% 
  purrr::map(~`names<-`(., as.character(1:8)))

# Sample Performance Measure ----------------------------------------------
sample_performance_measure(distances, placements, demands$lower, demands$upper, n = 10)
