suppressPackageStartupMessages(source("R/helpers.R"))
sapply(2019:2023, function(x) save_to_releases(
  get_league_factors(x),
  paste0("team_four_factors_", x, ".rds"), "four_factors")
  )
