library(ceblscrapeR)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(data.table)
library(stringdist)
library(tibble)

get_team_factors <- function(df, team_name) {
  team_stats <- df %>% filter(Team == team_name) %>%
    mutate(MIN = as.numeric(MIN)) %>%
    select(phase, MIN:TO, -ends_with("PCT")) %>%
    summarise(across(everything(), sum), .by = "phase")

  opponent_stats <- df %>% filter(Opponent == team_name) %>%
    mutate(MIN = as.numeric(MIN)) %>%
    select(phase, MIN:TO, -ends_with("PCT")) %>%
    summarise(across(everything(), sum), .by = "phase") %>%
    rename_with(~paste0("opp", .x))

  team_stats %>% left_join(
      opponent_stats, by = c("phase" = "oppphase")
    ) %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'Total'))) %>% #Add Totals
    mutate(
      phase,
      poss = (FGA + 0.44*FTA - OFF + TO),
      opp_poss = (oppFGA + 0.44*oppFTA - oppOFF + oppTO),
      team = team_name,
      pace = 200 * (poss + opp_poss)/(2*MIN), # poss per game
      ortg = (100*PTS)/(FGA + 0.44*FTA - OFF + TO),
      efg = (FGM+0.5*`3PM`)/FGA,
      tov = TO/(FGA + 0.44*FTA - OFF + TO),
      orb = OFF/(OFF + oppDEF),
      drb = DEF/(DEF + oppOFF),
      ftf = FTM/FGA,
      drtg = (100*oppPTS)/(oppFGA + 0.44*oppFTA - oppOFF + oppTO),
      opp_efg = (oppFGM+0.5*opp3PM)/oppFGA,
      opp_tov = oppTO/(oppFGA + 0.44*oppFTA - oppOFF + oppTO),
      opp_orb = oppOFF/(oppOFF + DEF),
      opp_drb = oppDEF/(oppDEF + OFF),
      opp_ftf = oppFTM/oppFGA,
      .keep = "none")
}

get_league_factors <- function(year) {

  team_names <- get_team_info(year) %>% pull(Team)
  boxscores <- read_from_releases(
    paste0("team_boxscores_", year, ".rds"), "box_scores")

  lapply(team_names, get_team_factors, df = boxscores) %>%
    bind_rows() %>%
    mutate(year, .before = 1)
}

get_trim <- function(column) {
  return(column %>% sapply(pmin, 5) %>% sapply(pmax, 1))
}

remove_blanks <- function(x) {x[nzchar(x)]}

scrape_data1 <- function (url, css_selection, split_char) {
  url %>% rvest::read_html() %>% rvest::html_elements(css_selection) %>%
    rvest::html_text2() %>% str_split(split_char)
}

get_advanced_data <- function(year){

  league_factors <- read_from_releases(
    paste0("team_four_factors_", year, ".rds"), "four_factors") %>%
    filter(phase == "Total")

  team_boxscores <- read_from_releases(
    paste0("team_boxscores_", year, ".rds"), "box_scores") #aggregate team data

  player_boxscores <- read_from_releases(
    paste0("player_boxscores_", year, ".rds"), "box_scores")

  teams <- get_team_info(year)
  season_id <- get_season_id(year)

  # Set Global Variables
  Positions <- data.frame(2.130, 8.668, -2.486, 0.992, -3.536, 1.667, 50) %>%
    setnames(c("Intercept","% of TRB", "% of STL", "% of PF",
               "% of AST", "% of BLK", "Min Wt"))
  rownames(Positions) <- "Modern"
  OffensiveRole <- data.frame(6, -6.642, -8.544, -0.330) %>%
    setnames(c("Intercept", "% of ast", "% of Threshold", "Pt Threshold"))
  DefaultPos <- 4

  BPMCoeffs <- data.frame(0.860,	-0.560,	-0.246,	0.389,	0.580,	-0.964,
                          0.613,	0.116,	0.000,	1.369,	1.327,	-0.367) %>%
    rbind(c(0.860,	-0.780,	-0.343,	0.389,	1.034,	-0.964,	0.181,	0.181,
            0.000,	1.008,	0.703,	-0.367)) %>%
    setnames(c("Adj. Pt",	"FGA",	"FTA",	"3 Pt FG (bonus)",	"AST",
               "TO", "ORB",	"DRB",	"TRB",	"STL",	"BLK",	"PF")) %>%
    `rownames<-`(c("Pos 1", "Pos 5"))

  OBPMCoeffs <- data.frame(0.605,	-0.330,	-0.145,	0.477,	0.476,	-0.579,
                           0.606,	-0.112,	0.000,	0.177,	0.725,	-0.439) %>%
    rbind(c(0.605,	-0.472,	-0.208,	0.477,	0.476,	-0.882,	0.422,
            0.103,	0.000,	0.294,	0.097,	-0.439)) %>%
    setnames(c("Adj. Pt",	"FGA",	"FTA",	"3 Pt FG (bonus)",	"AST",	"TO",
               "ORB",	"DRB",	"TRB",	"STL",	"BLK",	"PF")) %>%
    `rownames<-` (c("Pos 1", "Pos 5"))

  # Get team offensive information
  url <- paste0("https://hosted.dcd.shared.geniussports.com/CEBL/en/competition/",season_id, "/statistics/team")
  team_data_full <- scrape_data(url, "h4, td") %>% clean_data()
  idx <- which(team_data_full == "Averages")
  team_totals <- team_data_full[2:(idx-1)]

  column_names_team_totals <- scrape_data1(url, "#BLOCK_STATISTICS_TEAM_1 > div > table > thead", "\\t") %>%
    unlist() %>% remove_blanks()

  team_data_scraped <-
    data.frame(
    matrix(unlist(team_totals), ncol = length(column_names_team_totals), byrow = TRUE),
    stringsAsFactors = F
  ) %>% data.table::setnames(column_names_team_totals) %>%
    select(Team, PF = `Total Fouls`) %>% # See if I can get away by just pulling Total Fouls
    mutate(
      Team = sapply(Team,gsub, pattern = "([a-z])([A-Z])", replacement = "\\1 \\2"),
      across(-1, as.numeric)) %>%
    filter(Team %in% teams$Team)

  team_data <-
    team_boxscores %>%
    select(Team, MIN, PTS, FGM, FGA, `3PM`, `3PA`, FTM, FTA, OFF, DEF, TOT,
           A, STL, BLK, TO) %>%
    group_by(Team) %>% mutate(
      MIN = period_to_seconds(ms(as.numeric(MIN))),
      GP = 1, .after = Team) %>%
    summarise(across(everything(), sum)) %>%
    mutate(
      MIN = (MIN/60),
      `FG%` = FGM/FGA,
      `FT%` = FTM/FTA,
      `3P%` = `3PM`/`3PA`,
      #Team = teamName
    ) %>%
    left_join(team_data_scraped, by = "Team")

  # Initialize variables
  lgPace <- 0
  lgThree <- 0
  lgFT <- 0
  lgPF   <- 0
  lgFTA  <- 0
  lgAST  <- 0
  lgFG   <- 0
  lgPTS  <- 0
  lgFGA  <- 0
  lgORB  <- 0
  lgTO   <- 0
  lgTRB  <- 0
  lgMin  <- 0
  lgBLK  <- 0
  lgSTL  <- 0
  lgPoss <- 0

  league <- data.frame()
  totals <- data.frame()
  lgRankings <- data.frame()

  LeagueAverageGP <- team_boxscores %>%
    count(Player, name = "GP") %>%
    summarise(mean(GP)) %>%
    round() %>% pull()

  LeagueAverageORtg <- league_factors %>% pull(ortg) %>% mean()
  # not exact since I separated regular season and playoffs, whoops

  #MASSIVE FOR LOOP

  for( i in seq(nrow(teams))) {
    teamName = teams$Team[i]
    cebl_id = teams$CEBL_id[i]
    url <- paste0("https://hosted.dcd.shared.geniussports.com/CEBL/en/competition/" ,
                  season_id, "/team/", cebl_id, "/statistics"
    )
    column_names_team_totals <-
      scrape_data1(url,
                   "#hs-container > div.hs-container.noresize.main-container > div > div > div > div.table-wrap.heading > table > thead > tr",
                   "\\t") %>% unlist() %>% remove_blanks()

    individual_team_data <- scrape_data(url, "td") %>% clean_data()
    path = "person"
    player_ids <- scrape_links(url, subset_path = path) %>%
      lapply(sapply, extract_id, path) %>%
      clean_data()

    team_data_df <- data.frame(
      matrix(unlist(individual_team_data), ncol = length(column_names_team_totals), byrow = TRUE),
      stringsAsFactors = F
    ) %>% data.table::setnames(column_names_team_totals) %>%
      select(Player, G, GS, PF =`Tot Fouls PG`) %>%
      mutate(ID = player_ids,
             across(-1, as.numeric),
             PF = round(G*PF),
             Name = sapply(Player,gsub, pattern = "([a-z])([A-Z])", replacement = "\\1 \\2")) %>%
      select(-c(G, Player))

    team_stats <- player_boxscores %>% filter(Team == teamName) %>%
      select(ID, MIN, PTS, FGM, FGA, `3PM`, `3PA`, FTM, FTA, OFF, DEF, TRB = TOT,
                          AST = A, STL, BLK, TOV = TO) %>%
    group_by(ID) %>% mutate(
      MIN = period_to_seconds(ms(MIN)),
      GP = 1, .after = ID) %>%
      summarise(across(everything(), sum)) %>%
      mutate(
        MIN = (MIN/60),
        `FG%` = FGM/FGA,
        `FT%` = FTM/FTA,
        `3P%` = `3PM`/`3PA`,
        Team = teamName
      ) %>% filter(MIN > 0, FGA > 0) %>%
      left_join(team_data_df, by = "ID")

    offense <- team_data %>% filter(Team == teamName)
    #ID would be better but I think things match here.

    #Prepare Advanced Team Stats

    OffRating <- league_factors %>% filter(team == teamName) %>% pull(ortg)
    DefRating <- league_factors %>% filter(team == teamName) %>% pull(drtg)
    NetRatingAdj <- OffRating - DefRating
    Pace <- league_factors %>% filter(team == teamName) %>% pull(pace)
    OffRatingAdj <- OffRating - LeagueAverageORtg
    DefRatingAdj <- DefRating - LeagueAverageORtg
    AverageLead <- NetRatingAdj * Pace / 100 / 2
    LeadBonus <- 0.35/2*AverageLead
    AdjTeamRating <- NetRatingAdj + LeadBonus
    AdjOffRating <- OffRatingAdj + LeadBonus/2

    #Conglomerate Team Stats

    TeamPoints <- offense$PTS
    TeamFGA <- offense$FGA
    TeamFTA <- offense$FTA
    S6 <- ((BPMCoeffs[1,]$FTA/BPMCoeffs[1,]$FGA))
    TmPtsPerTSA <- TeamPoints/(TeamFGA+TeamFTA*S6)
    BaselinePtsPerTSA <- 1
    TotalMinutes <- 200 * offense$GP
    TeamTRB <- offense$TOT
    TeamSTL <- offense$STL
    TeamPF <- offense$PF
    TeamAST <- offense$A
    TeamBLK <- offense$BLK
    TeamGames <- offense$GP

    #Gather Data for PER

    lgPoss <- lgPoss+ offense %>% transmute(FGA + 0.44*FTA - OFF + TO) %>% pull()
    lgThree <- lgThree + offense$`3PM`
    lgFT <- lgFT + offense$FTM
    lgPF   <- lgPF   + offense$PF
    lgFTA  <- lgFTA  + TeamFTA
    lgAST  <- lgAST  + TeamAST
    lgFG   <- lgFG   + offense$FGM
    lgPTS  <- lgPTS  + TeamPoints
    lgFGA  <- lgFGA  + TeamFGA
    lgORB  <- lgORB  + offense$OFF
    lgTO   <- lgTO   + offense$TO
    lgTRB  <- lgTRB  + TeamTRB
    lgMin  <- lgMin  + TotalMinutes
    lgBLK  <- lgBLK  + TeamBLK
    lgSTL  <- lgSTL  + TeamSTL

    # Start Calculations

    playerSeasonTotals <- team_stats %>% # aggreate here
      filter(MIN > 0, FGA > 0) %>%

      #Add Shooting Stats and Adjust for Team Shooting Context

      mutate(`2PM` = (FGM - `3PM`), `2PA` = (FGA - `3PA`), `2P%` = `2PM`/`2PA`,
             'eFG%' = (FGM + (0.5*`3PM`))/FGA, .before = 'FTM') %>%
      mutate(TSA = FGA + S6*FTA, "Pt/TSA" = PTS/TSA,
             "Adj. Pts" = (`Pt/TSA` - TmPtsPerTSA + BaselinePtsPerTSA)*TSA,
             Possessions = MIN*Pace/40,
             "ThreshPts" = (TSA * (`Pt/TSA`-(TmPtsPerTSA+OffensiveRole$`Pt Threshold`))),
             .after = "PF") %>%

      # Calculated Stats per 100 Possessions

      mutate("Adj Pt"=`Adj. Pts`/Possessions * 100, "FGA_p" = FGA /Possessions * 100,
             "FTA_p" = FTA /Possessions * 100, "3PM_p" =`3PM`/Possessions * 100,
             "AST_p" = AST /Possessions * 100, "TOV_p" = TOV /Possessions * 100,
             "ORB_p" = OFF /Possessions * 100, "DRB_p" = DEF /Possessions * 100,
             "TRB_p" = TRB /Possessions * 100, "STL_p" = STL /Possessions * 100,
             "BLK_p" = BLK /Possessions * 100, "PF_p" = PF /Possessions * 100,
             .after = "ThreshPts") %>%

      #% Min

      mutate("% of Min" = MIN /(TotalMinutes/5), .after = 'PF_p') %>%

      # % of Stats
      mutate("% of TRB" = TRB / TeamTRB / `% of Min`,
             "% of STL" = STL / TeamSTL / `% of Min`,
             "% of PF" = PF / TeamPF /   `% of Min`,
             "% of AST" = AST / TeamAST / `% of Min`,
             "% of BLK" = BLK / TeamBLK / `% of Min`,
             "% of ThreshPts" = ThreshPts / sum(ThreshPts, na.rm = TRUE) / `% of Min`,
             .before = '% of Min')

    #Estimate Positions

    #Get url page formatted

    pos <- str_locate_all(pattern = " ", teamName)
    urlname <- ""
    starting <- (length(pos[[1]])/2) %% 2
    for(idx in 1:(((length(pos[[1]])/2))+1)) {
      if(idx == (length(pos[[1]])/2)+1) {
        segment <- substr(teamName,starting+1,nchar(teamName))
      } else {
        segment<- paste0(substr(teamName,starting-(length(pos[[1]])/2) %% 2,pos[[1]][idx]-1), "-")
        #print(segment)
      }
      urlname <- paste0(urlname, segment)
      starting <- pos[[1]][idx]
    }
    for (x in 1:length(urlname)) {
      urlname[x] <-  str_replace_all(urlname[x], "[\\s]", "")
      #print(urlname)
    }

    #Scrape Team Information

    roster <- paste0("https://basketball.realgm.com/international/league/128/Canadian-Elite-Basketball-League/team/",
                     teams$REALGM_id[i], "/",urlname, "/rosters/",year)


    roster_data <- scrape_data(roster, "td") %>% clean_data()

    idx <- which(roster_data == "Starters")
    rosterTotals <- roster_data[1:(idx-1)]

    col.names = c("No.", "Player", "Pos", "Ht", "Wt", "Age", "Birth_City",
                  "NBA Draft Status", "Nationality")

    playerSeasonRoster <- data.frame(matrix(
      unlist(rosterTotals),
      nrow = (length(rosterTotals)/9),
      byrow = TRUE), stringsAsFactors = FALSE)  %>% setnames(col.names)

    position <- vector()
    for(nom in playerSeasonTotals$Name) {
      searchResults <-  afind(playerSeasonRoster$Player, nom)
      index <- which.min(searchResults$distance)

      convertedNum <- switch(playerSeasonRoster$Pos[index], "-" = 3, "PG" = 1, "SG" = 2, "SF" = 3, "PF" = 4, "C" = 5,
                             "GF" = 2.5, "FG" = 2.5, "G" = 1.5, "F" = 3.5, "FC" = 4.5)
      #print(convertedNum)
      position <- append(position,convertedNum)
    }

    # Continue building Season Totals

    playerSeasonTotals <- playerSeasonTotals %>%
      mutate("Pos Num" = position,
             "Est Pos 1" = Positions$Intercept +
               (`% of TRB` * Positions$`% of TRB`) +
               (`% of STL` * Positions$`% of STL`) +
               (`% of PF` * Positions$`% of PF`) +
               (`% of AST` * Positions$`% of AST`) +
               (`% of BLK` * Positions$`% of BLK`),
             "Min Adj 1" = (`Est Pos 1` * MIN + `Pos Num` * Positions$`Min Wt`)/(MIN+Positions$`Min Wt`),
             "Trim 1" = get_trim(`Min Adj 1`),
             "Tm Avg 1" = sum(`Trim 1` * MIN, na.rm = TRUE)/TotalMinutes,
             "Adj Pos 2" = `Min Adj 1` - (`Tm Avg 1` - 3),
             "Trim 2" = get_trim(`Adj Pos 2`),
             "Tm Avg 2" = sum(`Trim 2` * MIN, na.rm = TRUE)/TotalMinutes,
             "Adj Pos 3" = `Min Adj 1` - (`Tm Avg 1` - 3) - (`Tm Avg 2` - 3),
             "Trim 3" = get_trim(`Adj Pos 3`),
             "Tm Avg 3" = sum(`Trim 3` * MIN, na.rm = TRUE)/TotalMinutes,
             "Adj Pos 4" = `Min Adj 1` - (`Tm Avg 1` - 3) - (`Tm Avg 2` - 3) - (`Tm Avg 3` - 3),
             "Position" = get_trim(`Adj Pos 4`),  .before = '% of Min') %>%

      #Offensive Role

      mutate("Est Off. Role 1" = OffensiveRole$Intercept + # I might have messed up here
               OffensiveRole$`% of ast`*`% of AST` +
               OffensiveRole$`% of Threshold` * `% of ThreshPts`,
             "Min Adj 1_o" =(`Est Off. Role 1` * MIN + DefaultPos*Positions$`Min Wt`)/(MIN + Positions$`Min Wt`),
             "Trim 1_o" = get_trim(`Min Adj 1_o`),
             "Tm Avg 1_o" = sum(`Trim 1_o` * MIN, na.rm = TRUE)/TotalMinutes,
             "Adj Off. Role 2" = `Min Adj 1_o` - (`Tm Avg 1_o` - 3),
             "Trim 2_o" = get_trim(`Adj Off. Role 2`),
             "Tm Avg 2_o" = sum(`Trim 2_o` * MIN, na.rm = TRUE)/TotalMinutes,
             "Adj Off. Role 3" = `Min Adj 1_o` - (`Tm Avg 1_o` - 3) - (`Tm Avg 2_o` - 3),
             "Trim 3_o" = get_trim(`Adj Off. Role 3`),
             "Tm Avg 3_o" = sum(`Trim 3_o` * MIN, na.rm = TRUE)/TotalMinutes,
             "Adj Off. Role 4" = `Min Adj 1_o` - (`Tm Avg 1_o` - 3) - (`Tm Avg 2_o` - 3) - (`Tm Avg 3_o` - 3),
             "Offensive Role" = get_trim(`Adj Off. Role 4`),

             # BPM Coefficients to use, based on positions

             "Adj. Pt" = (5 - Position)/4*BPMCoeffs$`Adj. Pt`[1] + (Position -1) / 4*BPMCoeffs$`Adj. Pt`[2],
             "FGA_bpm" = (5 - `Offensive Role`)/4*BPMCoeffs$FGA[1] + (`Offensive Role` -1) / 4*BPMCoeffs$FGA[2],
             "FTA_bpm" = (5 - `Offensive Role`)/4*BPMCoeffs$FTA[1] + (`Offensive Role` -1) / 4*BPMCoeffs$FTA[2],
             .before = '% of Min')

    for (coef in BPMCoeffs[4:12]) {
      #Refactor later
      playerSeasonTotals <- add_column(
        playerSeasonTotals, colname = (5 - playerSeasonTotals$Position)/4*coef[1] +
          (playerSeasonTotals$Position -1) / 4*coef[2], .before = '% of Min',
        .name_repair = "unique") %>% suppressMessages()
    }

    #Rename the columns
    colnames(playerSeasonTotals)[c(79:87)] <- sapply(names(BPMCoeffs)[4:12],  paste0,"_bpm")

    # Raw BPM Calculation
    PositionConstantBPM <- c((-0.409*2),0,0,1.387)
    PositionConstantOffensive <- c((-0.849*2),0,0,0.43)

    playerSeasonTotals <- playerSeasonTotals %>%
      mutate("Scoring" = (`Adj Pt` * `Adj. Pt`) + (FGA_p * FGA_bpm) +
               (FTA_p * FTA_bpm ) + (`3PM_p` * `3 Pt FG (bonus)_bpm`),
             "Ballhandling" = (AST_p * AST_bpm) + (TOV_p * TO_bpm ),
             "Rebounding" = (ORB_p * ORB_bpm) + (DRB_p * DRB_bpm ) + (TRB_p * TRB_bpm ),
             "Defense" = (STL_p * STL_bpm ) + (BLK_p * BLK_bpm ) + (PF_p *  PF_bpm ),

             # Kind of Complicated, but I think it works
             "Pos Const" = (ifelse(Position < 3,
                                   ((Position - 1)/2*PositionConstantBPM[2]+(3-Position)/2*PositionConstantBPM[1]),
                                   (Position - 3)/2*PositionConstantBPM[3]+(5-Position)/2*PositionConstantBPM[2]) +
                              PositionConstantBPM[4] * (`Offensive Role`-3)),
             "Raw BPM" = Scoring + Ballhandling + Rebounding + Defense + `Pos Const`,
             "Contributions" = `Raw BPM`*`% of Min`, .before = '% of Min')

    Sum <- sum(playerSeasonTotals$Contributions, na.rm = TRUE)
    Tm_Adj <- (AdjTeamRating-Sum)/5

    #OBPM Coefficients to use, based on Positions:

    playerSeasonTotals <- playerSeasonTotals %>%
      mutate("Adj. Pt_obpm" = (5 - Position)/4*OBPMCoeffs$`Adj. Pt`[1] +
               (Position -1) / 4*OBPMCoeffs$`Adj. Pt`[2],
             "FGA_obpm" = (5 - `Offensive Role`)/4*OBPMCoeffs$FGA[1] +
               (`Offensive Role` -1) / 4*OBPMCoeffs$FGA[2],
             "FTA_obpm" = (5 - `Offensive Role`)/4*OBPMCoeffs$FTA[1] +
               (`Offensive Role` -1) / 4*OBPMCoeffs$FTA[2], .before = '% of Min')

    for (coef in OBPMCoeffs[4:12]) { #Refactor later
      playerSeasonTotals <- add_column(
        playerSeasonTotals, colname = (5 - playerSeasonTotals$Position)/4*coef[1] +
          (playerSeasonTotals$Position -1) / 4*coef[2], .before = '% of Min',
        .name_repair = "unique") %>% suppressMessages()
    }

    #Rename the columns
    colnames(playerSeasonTotals)[c(98:106)] <- sapply(
      names(OBPMCoeffs)[4:12],  paste0,"_obpm")

    PositionConstantOffensive <- c((-0.849*2),0,0,0.43)

    playerSeasonTotals <- playerSeasonTotals %>%
      mutate( "Scoring_o" = (`Adj Pt` * `Adj. Pt_obpm` ) + (FGA_p * FGA_obpm ) +
                (FTA_p * FTA_obpm ) + (`3PM_p` *  `3 Pt FG (bonus)_obpm` ),
              "Ballhandling_o" = (AST_p * AST_obpm ) + (TOV_p * TO_obpm ),
              "Rebounding_o" = (ORB_p * ORB_obpm )+ (DRB_p * DRB_obpm ) +
                (TRB_p * TRB_obpm ),
              "Defense_o" = (STL_p * STL_obpm )+  (BLK_p * BLK_obpm ) +
                (PF_p *  PF_obpm ),

              # Kind of Complicated, but I think it works
              "Pos Const_obpm" = (ifelse(Position < 3,
                                         ((Position - 1)/2*PositionConstantOffensive[2]+(3-Position)/2*PositionConstantOffensive[1]),
                                         (Position - 3)/2*PositionConstantOffensive[3]+(5-Position)/2*PositionConstantOffensive[2]) +
                                    PositionConstantOffensive[4] * (`Offensive Role`-3)),
              "Raw OBPM" = Scoring_o + Ballhandling_o + Rebounding_o + Defense_o + `Pos Const_obpm`,
              "Contrib" = `Raw OBPM`*`% of Min`, .before = '% of Min')

    Sum_o <- sum(playerSeasonTotals$Contrib, na.rm = TRUE)
    Tm_Adj_o <- (AdjOffRating-Sum_o)/5
    totals <- rbind(totals, playerSeasonTotals)

    #Finish Calculations on Separate Dataframe

    Advanced_Stats <- playerSeasonTotals %>%
      mutate("Minutes" = MIN, "MPG" = MIN/GP,
             "BPM" = `Raw BPM`+Tm_Adj, "OBPM" = `Raw OBPM`+Tm_Adj_o) %>%
      mutate("DBPM" = BPM - OBPM, "Contribution" = BPM*`% of Min`,
             "VORP" = (BPM + 2)*`% of Min`*TeamGames/LeagueAverageGP,
             "eFG" = ( FGM+(0.5* `3PM`))*100/FGA,
             "TOV" = ((TOV*100)/(TOV+(0.44*FTA)+FGA)),
             "FTF" = ( FTM*100/  FGA),
             "PPP" = round(PTS/Possessions*5,3)) %>%
      select(c("Name", "Team", "Position", "Offensive Role", "Minutes",
               "MPG", "BPM", "OBPM", "DBPM", "Contribution", "VORP", "eFG", "TOV", "FTF", "PPP")) %>%
      setnames(c("Name", "Team", "Position",
                 "Offensive Role", "Minutes", "MPG", "BPM", "OBPM",
                 "DBPM", "Contribution", "VORP", "eFG%", "TOV%", "FTF%", "PPP"))

    league <- rbind(league, Advanced_Stats)
  }

  qualifiedPlayers <- data.frame(row.names = FALSE)
  qualifiedPlayers <- league#[totals$`% of Min` > 0.381097561,]
  qualifiedTotals <- totals

  #Add PER

  lgPace <- (lgPoss * 5) / lgMin # New way
  lgVOP <- lgPTS / (lgFGA - lgORB + lgTO + (0.44*lgFTA))
  lgDRBP <- (lgTRB - lgORB) / lgTRB
  lgFactor <- ((2/3) - ((0.5* (lgAST/lgFG))/(2*(lgFG/lgFT))))
  lguPER <- (1/lgMin)*(lgThree+((2.0 * lgAST)/3.0)+((2-(lgFactor*(lgAST/lgFG)))*lgFG) + (0.5*lgFT*(2-(1.0/3.0)*(lgAST/lgFG))) - (lgVOP*lgTO) - (lgVOP*lgDRBP*(lgFGA - lgFG)) - (lgVOP*0.44*(0.44+(0.56*lgDRBP))*(lgFTA-lgFT))+(lgVOP*(1-lgDRBP)*(lgTRB-lgORB))+(lgVOP*lgDRBP*lgORB)+(lgVOP*lgSTL)+(lgVOP*lgDRBP*lgBLK)-(lgPF*((lgFT/lgPF)-0.44*(lgFTA/lgPF)*lgVOP)))

  TeamStats <- team_data %>%
    left_join(.,league_factors %>%
                select(team, pace) %>%
                mutate(Pace = pace/40), by = c("Team" = "team")) %>%
    mutate(across(-1, as.numeric))

  PERlist <- vector()
  ## Complete PER Calculation. Refactor Later
  for (l in 1:nrow(qualifiedPlayers)) {

    uPER <-  (1/qualifiedTotals$MIN[l])*(qualifiedTotals$`3PM`[l]+((2.0 * qualifiedTotals$AST[l])/3.0)
                                         +((2-(lgFactor*((TeamStats$A[which(qualifiedPlayers$Team[l] == TeamStats$Team)])/(TeamStats$FGM[which(qualifiedPlayers$Team[l] == TeamStats$Team)]))))*qualifiedTotals$FGM[l]) + (0.5*qualifiedTotals$FTM[l]*(2-(1.0/3.0)*((TeamStats$A[which(qualifiedPlayers$Team[l] == TeamStats$Team)])/(TeamStats$FGM[which(qualifiedPlayers$Team[l] == TeamStats$Team)]))))
                                         - (lgVOP*qualifiedTotals$TOV[l]) - (lgVOP*lgDRBP*(qualifiedTotals$FGA[l] - qualifiedTotals$FGM[l]))
                                         - (lgVOP*0.44*(0.44+(0.56*lgDRBP))*(qualifiedTotals$FTA[l]-qualifiedTotals$FTM[l]))
                                         +(lgVOP*(1-lgDRBP)*(qualifiedTotals$TRB[l]-qualifiedTotals$OFF[l]))
                                         +(lgVOP*lgDRBP*qualifiedTotals$OFF[l])+(lgVOP*qualifiedTotals$STL[l])
                                         +(lgVOP*lgDRBP*qualifiedTotals$BLK[l])
                                         -(qualifiedTotals$PF[l]*((lgFT/lgPF)-0.44*(lgFTA/lgPF)*lgVOP)))

    PER <- round((uPER * (lgPace/TeamStats$Pace[which(qualifiedPlayers$Team[l] == TeamStats$Team)]))*(15/lguPER),1)

    PERlist <- append(PERlist, PER)
  }

  qualifiedPlayers <- qualifiedPlayers %>% mutate("PER" = PERlist) %>% arrange(desc(VORP))
  rownames(qualifiedPlayers) <- NULL
  qualifiedPlayers
}
