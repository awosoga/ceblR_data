library(ceblscrapeR)
library(dplyr)

update_boxscores <- function(year) {
  season_id <- get_season_id(year)
  boxscore_ids <- paste0("https://hosted.dcd.shared.geniussports.com/CEBL/en/competition/",
                     season_id, "/schedule?") %>%
    scrape_links(subset_path = "/boxscore") %>%
    sapply(extract_id, "boxscore") %>% clean_data()

  #See which ones already exists
  # Set the game type:

  # scrape boxscore data

  # playerbox:

  # game_id, team, player_id, opponent, everything else

}
