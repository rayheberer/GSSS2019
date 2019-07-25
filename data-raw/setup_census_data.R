library(magrittr)

source('~/sfi_project/R/data_processing.R')

state <- "IL"
county <- 031

# -------------------------------------------------------------------------

census_data <- import_join_population_income_data(state, county)

saveRDS(census_data, file.path("data", paste0(state, county, "_censusdata.rds")))
