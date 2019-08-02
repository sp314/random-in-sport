library(tidyverse)
library(lubridate)

# Function to extract how many days each team in a game had since they last played a game
# Measure for fatigue and difficult scheduling

# df: dataframe outputted by nhl/nhl/nfl_dataset() functions from sports_scraper repos
# RETURN: df with columns representing date gaps for home and away teams for each row
get_days_since_last_game <- function(df) {
  # Make 2 records for each game, one in the perspective of the home team and one in the perspective of the away team
  home <- df %>% ungroup() %>%
    mutate(team0 = home, 
           team1 = away,
           team0_score = home_score,
           team1_score = away_score,
           team0_hfa = TRUE,
           team1_hfa = FALSE
    ) %>%
    select(-c(home, away, home_score, away_score, home_win))
  
  away <- df %>% ungroup() %>%
    mutate(team0 = away, 
           team1 = home,
           team0_score = away_score,
           team1_score = home_score,
           team0_hfa = FALSE,
           team1_hfa = TRUE
    ) %>%
    select(-c(home, away, home_score, away_score, home_win))
  
  # Get the chronological set of games each team plays in a season whether they're home or away
  # Calculate the date gaps between the games in that set
  grouped <- bind_rows(home, away) %>%
    group_by(season, team0) %>%
    arrange(team0, game_date) %>%
    mutate(
      team0_last_game_date = lag(x = game_date, n = 1),
      team0_days_since_last_game = if_else(!is.na(team0_last_game_date), true = as.numeric(game_date - team0_last_game_date, units="days"), false = 0) # If NA (team0's first game of season) put 
    ) %>%
    ungroup() %>%
    group_by(season, team1) %>%
    arrange(team1, game_date) %>%
    mutate(
      team1_last_game_date = lag(x = game_date, n = 1),
      team1_days_since_last_game = if_else(!is.na(team1_last_game_date), true = as.numeric(game_date - team1_last_game_date, units="days"), false = 0),
      team0_win = team0_score > team1_score
    ) %>%
    select(season, game_id, game_date, team0, team1, team0_win, team0_score, team1_score,
           team0_hfa, team1_hfa, team0_days_since_last_game, team1_days_since_last_game) %>%
    ungroup()
  
  #With the feature now recorded, select the rows where team0 is home and revert to home away cols
  home_away <- grouped %>%
    filter(team0_hfa) %>%
    rename(
      'home' = team0,
      'away' = team1,
      'home_score' = team0_score,
      'away_score' = team1_score,
      'home_win' = team0_win,
      'home_days_since_last_game' = team0_days_since_last_game,
      'away_days_since_last_game' = team1_days_since_last_game
    ) %>%
    arrange(game_date, game_id) %>%
    select(-c(team0_hfa, team1_hfa))
  
  return(home_away)
}

# # RUN
# nba <- read_csv(file = "../../sports_scraper/nba_1999_2019.csv") %>% get_days_since_last_game()
# nhl <- read_csv(file = "../../sports_scraper/nhl_1999_2018.csv") %>% get_days_since_last_game()
# nfl <- read_csv(file = "../../sports_scraper/nfl_1999_2018.csv") %>% get_days_since_last_game()
# 
# write_csv(x = nba, path = "data/nba.csv")
# write_csv(x = nhl, path = "data/nhl.csv")
# write_csv(x = nfl, path = "data/nfl.csv")
# 
# # Visualize for fun!
# nba_date_gaps <- c(nba$home_days_since_last_game, nba$away_days_since_last_game)
# nba_date_gaps <- data.frame(days_since_last_game = nba_date_gaps[nba_date_gaps != 0], 
#                             league = "nba")
# 
# nhl_date_gaps <- c(nhl$home_days_since_last_game, nhl$away_days_since_last_game)
# nhl_date_gaps <- data.frame(days_since_last_game = nhl_date_gaps[nhl_date_gaps != 0],
#                             league = "nhl")
# 
# nfl_date_gaps <- c(nfl$home_days_since_last_game, nfl$away_days_since_last_game)
# nfl_date_gaps <- data.frame(days_since_last_game = nfl_date_gaps[nfl_date_gaps != 0],
#                        league = "nfl")
# 
# date_gaps <- data.frame(rbind(nba_date_gaps, nhl_date_gaps, nfl_date_gaps)) %>%
#   mutate(league = as.factor(league))
# 
# date_gaps %>%
#   ggplot() +
#   geom_bar(mapping = aes(x = days_since_last_game, y = ..prop.., group = league, fill = league)) +
#   facet_wrap(~league) +
#   ggtitle(label = "Distribution of date gaps between games 1999-2019")
# 
# # Matrix breakdown
# date_gaps_matrix <- xtabs( ~ days_since_last_game + league, data = date_gaps)

