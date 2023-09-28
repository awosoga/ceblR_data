suppressPackageStartupMessages(source("R/helpers.R"))
sapply(2021:2023, function(x) save_to_releases(
  get_advanced_data(x), paste0("advanced_data_", x, ".rds"), "advanced_data"))
