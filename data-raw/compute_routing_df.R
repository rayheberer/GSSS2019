library(magrittr)

source(here::here("R/data_processing.R"))

state <- "GA"
county <- "121"
osm_bbox <- "atlanta, georgia"
utm_epsg <- 32616

grid_res <- 250

otp_dir <- "~/Documents/otp"
otp_router <- "georgia"
otp_memory <- 10240
test_node <- 1
batch_size <- 10000

data_path <- here::here("data")
batches_path <- file.path(data_path, paste0("routing_df_batches_", state, grid_res))

dir.create(batches_path, showWarnings = FALSE)

# Get Boundary Polygon ----------------------------------------------------

bbox <- osmdata::opq(bbox = osm_bbox)$bbox %>% 
  stringr::str_split(",") %>% 
  purrr::pluck(1) %>% 
  as.numeric()

bbox_lon <- bbox[c(2, 4)]

bbox_lat <- bbox[c(1, 3)]

boundary_points <- data.frame(x = bbox_lon, y = bbox_lat) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  sf::st_transform(utm_epsg)


# Lay Down Grid -----------------------------------------------------------

grid <- boundary_points %>% 
  sf::st_make_grid(cellsize = grid_res) %>% 
  sf::st_as_sf() %>% 
  dplyr::mutate(id = as.character(dplyr::row_number()))

grid_centers <- sf::st_centroid(grid) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs = 4326)


# Compute Routing Df ------------------------------------------------------

setwd(otp_dir)

opentripplanner::otp_setup("otp.jar", ".", router = otp_router, memory = otp_memory)

otp_con <- opentripplanner::otp_connect(router = otp_router)

valid_nodes_rds <- paste0("valid_nodes_", state, grid_res, "m.rds")

if (valid_nodes_rds %in% list.files(data_path)) {
  valid_nodes <- readRDS(file.path(data_path, valid_nodes_rds))
} else {
  message("Computing valid nodes...")
  bad_nodes <- numeric(0)
  for (i in seq_len(nrow(grid_centers))) {
    if (i == test_node) {
      next()
    }
    
    plan <- opentripplanner::otp_plan(otp_con, grid_centers[test_node, 'x'], grid_centers[i, 'x'], get_geometry = FALSE)
    if (is.na(plan)) {
      bad_nodes <- append(bad_nodes, i)
    }
  }
  
  valid_nodes <- setdiff(1:nrow(grid_centers), bad_nodes)
  saveRDS(valid_nodes, file.path(data_path, valid_nodes_rds))
}

message("calculating origin-destination pairs...")
od_pairs <- purrr::cross2(valid_nodes, valid_nodes, .filter = `==`)
origins <- purrr::map_dbl(od_pairs, 1)
destinations <- purrr::map_dbl(od_pairs, 2)

assertthat::assert_that(!any(origins == destinations))

message("routing...")

origin_batches <- split(origins, ceiling(seq_along(origins) / batch_size))
destination_batches <- split(destinations, ceiling(seq_along(destinations) / batch_size))

for (i in sample(seq_along(origin_batches), size = length(origin_batches))) {
  batch_df_rds <- glue::glue("batch_{i}.rds")
  
  if (batch_df_rds %in% list.files(batches_path)) {
    message("Batch already computed, skipping...")
    next()
  }
  
  batch_df <- opentripplanner::otp_plan(
      otp_con,
      fromPlace = grid_centers[origin_batches[[i]], 'x'],
      toPlace = grid_centers[destination_batches[[i]], 'x'], 
      fromID = grid_centers$id[origin_batches[[i]]],
      toID = grid_centers$id[destination_batches[[i]]],
  )
  
  saveRDS(batch_df, file.path(batches_path, batch_df_rds))
  
}

# Cleanup -----------------------------------------------------------------

rm(list = ls())
