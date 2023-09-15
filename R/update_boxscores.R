library(ceblscrapeR)
library(dplyr)

update_boxscores <- function(year) {
  season_id <- get_season_id(year)
  phase = "Regular Season" #adjust this so that it isn't hardcoded?
  boxscore_ids <- paste0("https://hosted.dcd.shared.geniussports.com/CEBL/en/competition/",
                     season_id, "/schedule?") %>%
    scrape_links(subset_path = "/boxscore") %>%
    sapply(extract_id, "boxscore") %>%
    clean_data() %>% as.numeric()
  stopifnot(length(boxscore_ids) > 0)

  # see if any games have been recorded yet
  if(inherits(try(
    read_from_releases(paste0("team_boxscores_", year, ".rds"), "box_scores"),
    TRUE), "try-error") %>% suppressWarnings()
    #!file.exists(paste0("team_boxscores_", year, ".rds"))
     ) {
    boxscores_current_season <- mapply(scrape_boxscore_data,
                                       year = season,
                                       match_id = boxscore_ids,
                                       phase = phase,
                                       SIMPLIFY = F) %>% bind_rows()

    # save current season team and player boxscores
    team_ids <- get_team_info(season) %>% pull(CEBL_id)
    team_boxscores_current_season <- boxscores_current_season %>% filter(ID %in% team_ids)
    player_boxscores_current_season <- boxscores_current_season %>% filter(!(ID %in% team_ids))

    save_to_releases(team_boxscores_current_season,
                     paste0("team_boxscores_", year, ".rds"),
                     "box_scores")
    save_to_releases(player_boxscores_current_season,
                     paste0("player_boxscores_", season, ".rds"),
                     "box_scores")
  } else {

  #See which ones have already been scraped
  existing_ids <- read_from_releases(
    paste0("team_boxscores_", year, ".rds"), "box_scores") %>% pull(game_id)
  new_games <- setdiff(boxscore_ids, existing_ids)
  stopifnot(length(new_games) > 0)

  # scrape boxscore data
  new_boxscores <- mapply(scrape_boxscore_data,
                                     year = year,
                                     match_id = new_games,
                                     phase = phase,
                                     SIMPLIFY = F) %>% bind_rows()
  # add them to old boxscores
  team_ids <- get_team_info(year) %>% pull(CEBL_id)
  team_boxscores_new <- new_boxscores %>% filter(ID %in% team_ids)
  player_boxscores_new <- new_boxscores %>% filter(!(ID %in% team_ids))

  team_boxscores_current_season <- read_from_releases(
    paste0("team_boxscores_", year, ".rds"), "box_scores") %>% bind_rows(team_boxscores_new)
  player_boxscores_current_season <- read_from_releases(
    paste0("player_boxscores_", year, ".rds"), "box_scores") %>% bind_rows(player_boxscores_new)

  save_to_releases(team_boxscores_current_season,
                   paste0("team_boxscores_", year, ".rds"), "box_scores")
  save_to_releases(player_boxscores_current_season,
                   paste0("player_boxscores_", season, ".rds"), "box_scores")

  }

  # update all time stats
  team_boxscores_all_seasons <- read_from_releases("team_boxscores_all_seasons.rds", "box_scores") %>%
    bind_rows(team_boxscores_current_season)

  player_boxscores_all_seasons <- read_from_releases("player_boxscores_all_seasons.rds", "box_scores") %>%
    bind_rows(player_boxscores_current_season)

  save_to_releases(team_boxscores_all_seasons,
                   "team_boxscores_all_seasons.rds",
                   "box_scores")
  save_to_releases(player_boxscores_all_seasons,
                   "player_boxscores_all_seasons.rds",
                   "box_scores")
}

update_boxscores(2024)
