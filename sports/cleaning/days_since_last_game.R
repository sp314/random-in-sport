library(tidyverse)
library(lubridate)

#Read in data
nba <- read_csv(file = "data/nba.csv")
nhl <- read_csv(file = "data/nhl.csv")
nfl <- read_csv(file = "data/nfl.csv")

### NBA  -----------

#Map each date to a season, remove duplicate rows
nba_clean <- nba %>%
  mutate(
    season = as.factor(case_when(
      game_date <= "2007-04-18" ~ "2006-07",
      "2007-10-30" <= game_date & game_date <= "2008-04-16" ~ "2007-08",
      "2008-10-28" <= game_date & game_date <= "2009-04-16" ~ "2008-09",
      "2009-10-27" <= game_date & game_date <= "2010-04-14" ~ "2009-10",
      "2010-10-26" <= game_date & game_date <= "2011-04-13" ~ "2010-11",
      "2011-12-25" <= game_date & game_date <= "2012-04-26" ~ "2011-12",
      "2012-10-30" <= game_date & game_date <= "2013-04-17" ~ "2012-13",
      "2013-10-29" <= game_date & game_date <= "2014-04-16" ~ "2013-14",
      "2014-10-28" <= game_date & game_date <= "2015-04-15" ~ "2014-15",
      "2015-10-27" <= game_date & game_date <= "2016-04-13" ~ "2015-16",
      "2016-10-25" <= game_date & game_date <= "2017-04-12" ~ "2016-17",
      "2017-10-17" <= game_date & game_date <= "2018-04-11" ~ "2017-18",
      "2018-10-16" <= game_date & game_date <= "2019-04-10" ~ "2018-19"
    ))
  ) %>%
  distinct()

#We want to be able to group by each team every game they played whether it was home or away,
#Thus, I made a record for each game, one in the perspective of the home team and one in the perspective of the away team
#We will then group by teams and have all the games they played and which side they played on

nba_clean_groups_homefirst <- nba_clean %>%
  mutate(team0 = home, 
         team1 = away,
         team0_score = home_score,
         team1_score = away_score,
         team0_hfa = TRUE,
         team1_hfa = FALSE
         ) %>%
  select(-c(home, away, home_score, away_score, home_win))

nba_clean_groups_awayfirst <- nba_clean %>%
  mutate(team0 = away, 
         team1 = home,
         team0_score = away_score,
         team1_score = home_score,
         team0_hfa = FALSE,
         team1_hfa = TRUE
  ) %>%
  select(-c(home, away, home_score, away_score, home_win))

#With the set of matches for each team find how much time elapsed between each match
nba_clean_groups <- bind_rows(nba_clean_groups_homefirst, nba_clean_groups_awayfirst) %>%
  group_by(season, team0) %>%
  arrange(team0, game_date) %>%
  mutate(
    game_date_lag = lag(x = game_date, n = 1),
    team0_days_since_last_game = if_else(!is.na(game_date_lag), true = game_date - game_date_lag, false = 0),
    team0_win = team0_score > team1_score
  ) %>%
  ungroup() %>%
  group_by(season, team1) %>%
  arrange(team1, game_date) %>%
  mutate(
    game_date_lag = lag(x = game_date, n = 1),
    team1_days_since_last_game = if_else(!is.na(game_date_lag), true = game_date - game_date_lag, false = 0)
  ) %>%
  select(-c(game_date_lag, team1_hfa))

#With the feature now recorded, go back to the format of having one record for each match with a home and an away column
nba_back_to_home_away <- nba_clean_groups %>%
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
  arrange(game_date) %>%
  select(-team0_hfa)

# write_csv(x = nba_back_to_home_away, path = "data/nba.csv")

#Repeat for next sport

### NFL ------
nfl_clean <- nfl %>%
  mutate(
    season = as.factor(case_when(
      game_date <= "2006-12-31" ~ "2006",
      "2007-09-06" <= game_date & game_date <= "2007-12-30" ~ "2007",
      "2008-09-04" <= game_date & game_date <= "2008-12-28" ~ "2008",
      "2009-09-10" <= game_date & game_date <= "2010-01-03" ~ "2009",
      "2010-09-09" <= game_date & game_date <= "2011-01-02" ~ "2010",
      "2011-09-08" <= game_date & game_date <= "2012-01-01" ~ "2011",
      "2012-09-05" <= game_date & game_date <= "2012-12-30" ~ "2012",
      "2013-09-05" <= game_date & game_date <= "2013-12-29" ~ "2013",
      "2014-09-04" <= game_date & game_date <= "2014-12-28" ~ "2014",
      "2015-09-10" <= game_date & game_date <= "2016-01-03" ~ "2015",
      "2016-09-08" <= game_date & game_date <= "2017-01-01" ~ "2016",
      "2017-09-07" <= game_date & game_date <= "2017-12-31" ~ "2017",
      "2018-09-06" <= game_date ~ "2018"
    )),
    home_win = case_when(
      home_score > away_score ~ 1,
      home_score < away_score ~ -1,
      home_score == away_score ~ 0
    )
  )

nfl_clean_groups_homefirst <- nfl_clean %>%
  mutate(team0 = home, 
         team1 = away,
         team0_score = home_score,
         team1_score = away_score,
         team0_hfa = TRUE,
         team1_hfa = FALSE
  ) %>%
  select(-c(home, away, home_score, away_score, home_win))

nfl_clean_groups_awayfirst <- nfl_clean %>%
  mutate(team0 = away, 
         team1 = home,
         team0_score = away_score,
         team1_score = home_score,
         team0_hfa = FALSE,
         team1_hfa = TRUE
  ) %>%
  select(-c(home, away, home_score, away_score, home_win))

nfl_clean_groups <- bind_rows(nfl_clean_groups_homefirst, nfl_clean_groups_awayfirst) %>%
  group_by(season, team0) %>%
  arrange(team0, game_date) %>%
  mutate(
    game_date_lag = lag(x = game_date, n = 1),
    team0_days_since_last_game = if_else(!is.na(game_date_lag), true = game_date - game_date_lag, false = 0),
    team0_win = team0_score > team1_score
  ) %>%
  ungroup() %>%
  group_by(season, team1) %>%
  arrange(team1, game_date) %>%
  mutate(
    game_date_lag = lag(x = game_date, n = 1),
    team1_days_since_last_game = if_else(!is.na(game_date_lag), true = game_date - game_date_lag, false = 0)
  ) %>%
  select(-c(game_date_lag, team1_hfa))

nfl_back_to_home_away <- nfl_clean_groups %>%
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
  arrange(game_date) %>%
  select(-team0_hfa)

# write_csv(x = nfl_back_to_home_away, path = "data/nfl.csv")

### NHL ----

nhl_clean <- nhl %>%
  mutate(
    season = as.factor(case_when(
      game_date <= "2007-06-06" ~ "2006-07",
      "2007-09-29" <= game_date & game_date <= "2008-06-04" ~ "2007-08",
      "2008-10-04" <= game_date & game_date <= "2009-06-12" ~ "2008-09",
      "2009-10-01" <= game_date & game_date <= "2010-06-09" ~ "2009-10",
      "2010-10-07" <= game_date & game_date <= "2011-06-15" ~ "2010-11",
      "2011-10-06" <= game_date & game_date <= "2012-06-11" ~ "2011-12",
      "2013-01-19" <= game_date & game_date <= "2013-06-24" ~ "2012-13",
      "2013-10-01" <= game_date & game_date <= "2014-06-13" ~ "2013-14",
      "2014-10-08" <= game_date & game_date <= "2015-06-15" ~ "2014-15",
      "2015-10-07" <= game_date & game_date <= "2016-06-12" ~ "2015-16",
      "2016-10-12" <= game_date & game_date <= "2017-06-11" ~ "2016-17",
      "2017-10-04" <= game_date & game_date <= "2018-06-07" ~ "2017-18",
      "2018-10-03" <= game_date & game_date <= "2019-06-12" ~ "2018-19"
    ))
  ) %>% drop_na()

nhl_clean_groups_homefirst <- nhl_clean %>%
  mutate(team0 = home, 
         team1 = away,
         team0_score = home_score,
         team1_score = away_score,
         team0_hfa = TRUE,
         team1_hfa = FALSE
  ) %>%
  select(-c(home, away, home_score, away_score, home_win))

nhl_clean_groups_awayfirst <- nhl_clean %>%
  mutate(team0 = away, 
         team1 = home,
         team0_score = away_score,
         team1_score = home_score,
         team0_hfa = FALSE,
         team1_hfa = TRUE
  ) %>%
  select(-c(home, away, home_score, away_score, home_win))

nhl_clean_groups <- bind_rows(nhl_clean_groups_homefirst, nhl_clean_groups_awayfirst) %>%
  group_by(season, team0) %>%
  arrange(team0, game_date) %>%
  mutate(
    game_date_lag = lag(x = game_date, n = 1),
    team0_days_since_last_game = if_else(!is.na(game_date_lag), true = game_date - game_date_lag, false = 0),
    team0_win = team0_score > team1_score
  ) %>%
  ungroup() %>%
  group_by(season, team1) %>%
  arrange(team1, game_date) %>%
  mutate(
    game_date_lag = lag(x = game_date, n = 1),
    team1_days_since_last_game = if_else(!is.na(game_date_lag), true = game_date - game_date_lag, false = 0)
  ) %>%
  select(-c(game_date_lag, team1_hfa))

nhl_back_to_home_away <- nhl_clean_groups %>%
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
  arrange(game_date) %>%
  select(-team0_hfa)

# write_csv(x = nhl_back_to_home_away, path = "data/nhl.csv")



