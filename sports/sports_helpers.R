# Function takes a dataframe of matches and spreads it into a design matrix with a column for results mov or binary and with ... predictors
match_design_matrix <- function(df, mov, ...) {
  home_teams <- df %>%
    rename('team' = home) %>%
    mutate(hfa = 1) %>%
    model.matrix(~team + hfa - 1, ., contrasts.arg = list(team = "contr.sum"))
  
  away_teams <- df %>%
    rename('team' = away) %>%
    mutate(hfa = 0) %>%
    model.matrix(~team + hfa - 1, ., contrasts.arg = list(team = "contr.sum"))
  
  design <- cbind(home_teams - away_teams, df %>% select(...))
  if (mov) {
    design <- cbind(Y = df$home_mov, design)
  }
  else {
    design <- cbind(Y = df$home_win, design)
  }
  
  return(design[,-2]) # Remove one col to have reference level at 0
}


# For each proportion, make n_sets of training and test sets and bind into dataframe
training_split_df <- function(df, prop, n_sets, mov) {
  train_list <- list()
  test_list <- list()
  
  for (p in prop) {
    for (idx in 1:n_sets) {
      #Create training set
      train <- df %>%
        sample_frac(p, replace = FALSE) %>%
        match_design_matrix(mov = mov, game_id)
      
      #Create test set
      test  <- df %>%
        anti_join(y = train, by = 'game_id') %>%
        match_design_matrix(mov = mov)
      
      train_list[[as.character(p)]][[idx]] <- train[1:ncol(test)]
      test_list[[as.character(p)]][[idx]] <- test
    }
  }
  
  tibble(
    train_prop = rep(prop, n_sets) %>% sort(),
    train = train_list %>% unlist(recursive = FALSE),
    test = test_list %>% unlist(recursive = FALSE)
  )
}

# Transform log_odds to prob
sigmoid <- function(log_odds) {
  1/(1 + exp(-log_odds))
}

#Function returns the sum of sim and actual predicting the same, conf.matrix = true only returns the confusion matrix version
pred_accuracy <- function(sim, actual, conf.matrix = FALSE) {
  if (conf.matrix) {
    return(xtabs(formula = ~ sim + actual))
  }
  
  return(sum(actual == sim)/length(actual))
}