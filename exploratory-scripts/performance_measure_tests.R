library(igraph)

source('~/sfi_project/R/objective_functions.R')

# Build Toy Graph ---------------------------------------------------------
edges <- c(1,2, 2,3, 2,4, 4,5, 3,5, 5,6)
edge_weights <- c(5, 4, 4, 3, 3, 10)
weights <- c(1, 2, 3, 4, 5, 6)

graph <- igraph::make_graph(edges, directed = FALSE)
igraph::E(graph)$weight <- edge_weights

distances <- igraph::distances(graph)

placements <- c(1, 6)

# Test Performance Measures -----------------------------------------------

maximal_distance(distances, placements) # 10

minimal_distance(distances, placements) # 5

range(distances, placements) # 5

mean_minimal_distance(distances, placements) # 5.5

variance_minimal_distance(distances, placements) # 21.1


# Test Performance Measures (with weights) --------------------------------

mean_minimal_distance(distances, placements, weights) # 5.85

variance_minimal_distance(distances, placements, weights) # 19.83

gini_coefficient(distances, placements, weights) # 0.413

mean_deviation(distances, placements, weights) # 4.07

hoover_concentration_index(distances, placements, weights) # 0.347

moran_i(distances, placements, weights, 10) # 0.26

x <- seq(0, 20)
y <- purrr::map_dbl(x, ~moran_i(distances, placements, weights, .))
