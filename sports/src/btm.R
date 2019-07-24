library(tidyverse)
library(lubridate)
library(BradleyTerry2)
library(prefmod)

nba <- read_csv(file = "data/nba.csv")

nba_10 <- nba %>%
  filter(season == "2010-11")

nba_10_model <- BTm(outcome = home_win, #1 if player1 (home) wins and 0 if player2 (away) does
    player1 = data.frame(team = home, hfa = 1, days_since_last_game = home_days_since_last_game), 
    player2 = data.frame(team = away, hfa = 0, days_since_last_game = away_days_since_last_game),
    formula = ~team + hfa + days_since_last_game, id = "team", data = nba_10)
summary(nba_10_model)

nba_10_thetas.qv <- qvcalc(BTabilities(nba_10_model)) #quasi normal standard errors
plot(nba_10_thetas.qv)
