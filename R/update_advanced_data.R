source("R/helpers.R")
year <- 2024
save_to_releases(get_advanced_data(year), paste0("advanced_data_", year, ".rds"), "advanced_data")
