library(magrittr)

source(here::here("R/data_processing.R"))

state <- "GA"
county <- "121"
osm_bbox <- "atlanta, georgia"
utm_epsg <- 32616

grid_res <- 250

batch_size <- 50

data_path <- here::here("data")

routing_batch_path <- file.path(data_path, glue::glue("routing-batches-{state}"))
dir.create(routing_batch_path, showWarnings = FALSE)

valhalla_path <- here::here(glue::glue("valhalla-{state}"))
if (!valhalla_path %in% list.files(here::here(), full.names = TRUE)) {
  source(here::here("setup_valhalla.R"))
}

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

lat_lons <- do.call(rbind, sf::st_geometry(grid_centers)) %>% 
  tibble::as_tibble(.name_repair = "minimal") %>% 
  setNames(c("lon", "lat")) %>% 
  dplyr::select(lat, lon)

ids <- grid_centers$id

# Setup Router ------------------------------------------------------------

setwd(valhalla_path)
system("docker-compose up", wait = FALSE)
Sys.sleep(10)  # a bit of a hacky way to ensure the router is set up


# Build Routing Batches ---------------------------------------------------

null_to_na <- function(x) {
  x[vapply(x, is.null, logical(1))] <- NA
  return(x)
}

batches <- split(1:nrow(lat_lons), ceiling(seq_len(nrow(lat_lons)) / batch_size))

for (batch_ix in 1:length(batches)) {
  source_ix <- batches[[batch_ix]]
  sources <- jsonlite::toJSON(lat_lons[source_ix,])
  source_ids <- ids[source_ix]
  
  batch_df <- NULL
  for (target_ix in batches) {
    targets <- jsonlite::toJSON(lat_lons[target_ix,])
    target_ids <- ids[target_ix]

    query <- glue::glue('curl http://localhost:8002/sources_to_targets --data \'{"sources": {{sources}}, 
                        "targets": {{targets}}, "costing": "auto"}\' | jq \'.\'', .open = "{{", .close = "}}")
    
    response <- system(query, intern = TRUE) %>% 
      jsonlite::parse_json()
    
    sources_to_targets <- response$sources_to_targets %>% 
      purrr::flatten()
    
    batch_routing_mat <- do.call(rbind, sources_to_targets) %>% 
      null_to_na()
    
    routing_df <- tibble::tibble(
      from_id = source_ids[unlist(batch_routing_mat[, "from_index"]) + 1],
      to_id = target_ids[unlist(batch_routing_mat[, "to_index"]) + 1],
      distance = unlist(batch_routing_mat[, "distance"]),
      time = unlist(batch_routing_mat[, "time"])
    )
    
    batch_df <- dplyr::bind_rows(batch_df, routing_df)
  }
  
  saveRDS(batch_df, file.path(routing_batch_path, glue::glue("batch_{batch_ix}.rds")))
  
}
