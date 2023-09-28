source("R/helpers.R")
year <- 2024
save_to_releases(get_league_factors(year), paste0("team_four_factors_", year, ".rds"), "four_factors")
