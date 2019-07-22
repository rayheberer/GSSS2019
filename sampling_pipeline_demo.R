library(magrittr)
library(ggplot2)

source('~/sfi_project/R/objective_functions.R')
source('~/sfi_project/R/placement_algorithms.R')

p <- 3

# Import Data -------------------------------------------------------------
data_raw <- dodgr::dodgr_streetnet("santa fe, new mexico")

graph <- dodgr::weight_streetnet(data_raw, wt_profile = "motorcar") %>% 
  dodgr::dodgr_sample(nverts = 1000)

distances <- dodgr::dodgr_distances(graph)

# Assign Demand Intervals -------------------------------------------------
demands <- generate_random_demand_ranges(distances)
expected_demands <- (demands$lower + demands$upper) / 2

# Run Placement Algorithm -------------------------------------------------
placements_random <- random_placement(distances, p)
placements_greedy <- greedy_placement(distances, expected_demands, p = p)

# Sample Performance Measure ----------------------------------------------
performance_samples <- 
  sample_performance_measure(distances, placements, demands$lower, demands$upper)

ggplot(data.frame(x = performance_samples), aes(x = x)) +
  geom_density()

ggplot(data.frame(x = performance_samples), aes(x = x)) +
  geom_histogram()
