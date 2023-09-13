library(ceblscrapeR)
library(dplyr)
library(stringr)

player_boxscores_all_seasons <- data.frame()
team_boxscores_all_seasons <- data.frame()

unlist_modified <- function(x) {
  list_names <- names(x)
  for (i in seq(length(list_names))) {
    phase_name <- str_extract(list_names[i], "phaseName=.+&$") %>%
      str_remove_all("(phaseName=)|(\\&)")
  names(x[[list_names[i]]]) <- rep(phase_name, length(x[[list_names[i]]]))
  }
  x %>% unname() %>% unlist()
}

for(season in 2019:2020) {
  print(paste("scraping season:", season))

  # get boxscore ids
  season_id <- get_season_id(season)
  boxscore_ids <-
    paste0("https://hosted.dcd.shared.geniussports.com/CEBL/en/competition/",
                         season_id, "/schedule?") %>%
    scrape_links(subset_path = "phaseName.+\\&$") %>%
    sapply(str_detect, "matchType", negate = T) %>%
    which() %>% names() %>%
    sapply(URLencode) %>%
    sapply(scrape_links, subset_path = "/boxscore") %>%
    lapply(sapply, extract_id, "boxscore") %>%# tail(1) %>%
    unlist_modified()

  boxscores_current_season <- mapply(scrape_boxscore_data,
                                     year = season,
                                     match_id = boxscore_ids,
                                     phase = names(boxscore_ids),
                                     SIMPLIFY = F) %>% bind_rows()

  # save current season team and player boxscores
  team_ids <- get_team_info(season) %>% pull(CEBL_id)
  team_boxscores_current_season <- boxscores_current_season %>% filter(ID %in% team_ids)
  player_boxscores_current_season <- boxscores_current_season %>% filter(!(ID %in% team_ids))

  saveRDS(team_boxscores_current_season, paste0("team_boxscores_", season, ".rds"))
  saveRDS(player_boxscores_current_season, paste0("player_boxscores_", season, ".rds"))

  # update all time team and player boxscores
  player_boxscores_all_seasons <- bind_rows(player_boxscores_all_seasons,
                                            player_boxscores_current_season)

  team_boxscores_all_seasons <- bind_rows(team_boxscores_all_seasons,
                                            team_boxscores_current_season)
}

# save all time boxscores
saveRDS(team_boxscores_all_seasons, "team_boxscores_all_seasons.rds")
saveRDS(player_boxscores_all_seasons, "player_boxscores_all_seasons.rds")

