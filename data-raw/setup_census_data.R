library(magrittr)
library(totalcensus)

source(here::here('R/data_processing.R'))

options(tigris_use_cache = TRUE)

data_path <- here::here("data/totalcensus")

totalcensus::set_path_to_census(data_path)
tigris::tigris_cache_dir(here::here("data/tigris_cache"))
dir.create(data_path, recursive = TRUE, showWarnings = FALSE)

state <- "GA"
county_fips <- "121"

# -------------------------------------------------------------------------

census_data <- import_join_population_income_data(state, county_fips)

saveRDS(census_data, file.path(here::here("data"), paste0(state, county_fips, "_censusdata.rds")))

gc()
