library(magrittr)
library(totalcensus)

source('~/sfi_project/R/data_processing.R')
totalcensus::set_path_to_census(tempdir())

state <- "MA"
county <- "025"

# -------------------------------------------------------------------------

census_data <- import_join_population_income_data(state, county)

saveRDS(census_data, file.path("data", paste0(state, county, "_censusdata.rds")))

unlink(tempdir(), TRUE)
gc()
