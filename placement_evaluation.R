library(magrittr)

source(here::here('R/data_processing.R'))
source(here::here('R/objective_functions.R'))
source(here::here('R/placement_algorithms.R'))
source(here::here('R/simulations_and_sampling.R'))

state <- "NM"
county <- "049"

osm_bbox <- "santa fe, new mexico"
osm_tags <- c("motorway", "trunk", "primary", "secondary", 
              "tertiary", "unclassified", "residential",
              "motorway_link", "trunk_link", "primary_link",
              "secondary_link", "tertiary_link", "living_street",
              "service", "pedestrian", "track", "bus_guideway",
              "escape", "raceway", "road")

random_placement_trials <- 100

# Import Data (Streetnet, Census, Stations) -------------------------------

streetnet_sf <- osmdata::opq(bbox = osm_bbox) %>% 
  osmdata::add_osm_feature(key = "highway", value = osm_tags) %>% 
  osmdata::osmdata_sf() %>% 
  `[[`("osm_lines")

graph <- streetnet_sf %>% 
  dodgr::weight_streetnet(wt_profile = "motorcar") %>% 
  dodgr::dodgr_contract_graph()

distances <- dodgr::dodgr_distances(graph) %>% 
  trim_disconnected_nodes()

census_data_sf <- readRDS(file.path("data", paste0(state, county, "_censusdata.rds")))

stations_sf <- get_charging_stations(state) %>% 
  sf::st_crop(osmdata::getbb(osm_bbox, format_out = "sf_polygon")) %>% 
  sf::st_transform(sf::st_crs(census_data_sf))

# Join Data, Get Assignments ----------------------------------------------

nodes_census_sf <- join_nodes_with_census_data(
  graph, 
  distances, 
  census_data_sf
)

placements <- assign_stations_nearest_nodes(nodes_census_sf, stations_sf)

# Get Population & Demand Weights -----------------------------------------

weights <- nodes_census_sf %>% 
  dplyr::mutate(weights = population / num_nodes) %>% 
  `[[`("weights") %>% 
  `names<-`(nodes_census_sf$id)


# Calculate Performance Measures ------------------------------------------

unweighted_performance_measures <- c("maximal_distance", "minimal_distance", 
                                     "range", "mean_minimal_distance", 
                                     "variance_minimal_distance")

weighted_performance_measures <- c("mean_minimal_distance", "variance_minimal_distance",
                                   "mean_deviation", "hoover_concentration_index",
                                   "theil_entropy_index")

results <- NULL
for (fn in unweighted_performance_measures) {
  message(fn)
  results <- c(results, do.call(fn, list(distances, placements)))
}

for (fn in weighted_performance_measures) {
  message(fn)
  results <- c(results, do.call(fn, list(distances, placements, weights)))
}

names(results) <- c(unweighted_performance_measures, 
                    paste0("weighted_", weighted_performance_measures))

results["gini_coefficiet"] <- gini_coefficient_vectorized(distances, placements, weights)
results["moran_I"] <- moran_i_vectorized(distances, placements, weights, 100)

saveRDS(results, file.path("results", paste0(state, county, "_metrics.rds")))

# Calculate Performance Measures - Random Placement -----------------------

placements_random <- 1:random_placement_trials %>% 
  purrr::map(~random_placement(distances, p = length(placements)))

saveRDS(placements_random, file.path("results", "placements_random.rds"))

results_random <- list()
for (fn in unweighted_performance_measures) {
  message(fn)
  results_random[[length(results_random) + 1]] <- placements_random %>% 
    purrr::map_dbl(~do.call(fn, list(distances, .)))
}

for (fn in weighted_performance_measures) {
  message(fn)
  results_random[[length(results_random) + 1]] <- placements_random %>% 
    purrr::map_dbl(~do.call(fn, list(distances, ., weights)))
}

names(results_random) <- c(unweighted_performance_measures, 
                           paste0("weighted_", weighted_performance_measures))

results_random[["gini_coefficient"]] <- placements_random %>% 
  purrr::map_dbl(~gini_coefficient_vectorized(distances, ., weights))

results_random[["moran_I"]] <- placements_random %>% 
  purrr::map_dbl(~moran_i_vectorized(distances, ., weights, 100))

results_random <- tibble::as_tibble(results_random)

write.csv(results_random, file.path("results", "results_random.csv"))

results_random_summary <- results_random %>% 
  dplyr::summarize_all(list(mean = mean, sd = sd))


# Calculate Performance Measures - Greedy Placement -----------------------

placements_greedy <- greedy_placement_vectorized(distances, weights, p = length(placements))

saveRDS(placements_greedy, file.path("results", "placements_weighted_greedy.rds"))

results_greedy <- NULL
for (fn in unweighted_performance_measures) {
  message(fn)
  results_greedy <- c(results_greedy, do.call(fn, list(distances, placements_greedy)))
}

for (fn in weighted_performance_measures) {
  message(fn)
  results_greedy <- 
    c(results_greedy, do.call(fn, list(distances, placements_greedy, weights)))
}

names(results_greedy) <- c(unweighted_performance_measures, 
                           paste0("weighted_", weighted_performance_measures))

results_greedy["gini_coefficient"] <- 
  gini_coefficient_vectorized(distances, placements_greedy, weights)

results_greedy["moran_I"] <- moran_i_vectorized(distances, placements_greedy, weights, 100)

saveRDS(results_greedy, file.path("results", paste0(state, county, "_greedymetrics.rds")))
