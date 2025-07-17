Calculate1stSetWonWins <- function() {
  
  require(stringr)
  
  # Remove matches with abnormal or incomplete results such as walkovers (W/O), defaults (DEF),
  # weather issues (WEA), and abandonments (ABN)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]
  
  # Helper function to determine if the winner won the first set based on the score string
  won_first_set <- function(score_string) {
    # Return NA if score is missing or empty
    if (is.na(score_string) || score_string == "") return(NA)
    
    # Remove tiebreak details enclosed in parentheses (e.g., (7-3))
    score_clean <- gsub("\\(.*?\\)", "", score_string)
    
    # Split the score string into individual sets by space
    sets <- strsplit(score_clean, " ", fixed = TRUE)[[1]]
    
    # Return NA if no set scores are found
    if (length(sets) < 1) return(NA)
    
    # Extract the first set score
    first_set <- sets[1]
    
    # Split the first set score into games won by winner and opponent using "-"
    games <- unlist(strsplit(first_set, "-", fixed = TRUE))
    
    # Return NA if the score does not have exactly two numbers
    if (length(games) != 2) return(NA)
    
    # Convert the games won to integers
    player_games <- as.integer(games[1])
    opponent_games <- as.integer(games[2])
    
    # Return TRUE if the winner won more games in the first set, otherwise FALSE
    return(player_games > opponent_games)
  }
  
  # Apply the helper function to the 'score' column and create a logical vector
  # indicating whether the winner won the first set
  db$won_first_set <- sapply(db$score, won_first_set)
  
  # Calculate total wins per player
  total_wins <- table(db$winner_name)
  
  # Calculate number of wins where the winner also won the first set,
  # ignoring NA values
  wins_after_first_set_win <- tapply(db$won_first_set, db$winner_name, function(x) sum(x, na.rm = TRUE))
  
  # Create a data frame summarizing:
  # - player name
  # - total wins
  # - wins where the first set was won
  result <- data.frame(
    player = names(total_wins),
    total_wins = as.integer(total_wins),
    wins_after_first_set_win = as.integer(wins_after_first_set_win)
  )
  
  # Sort the results by descending number of wins after winning the first set
  result <- result[order(-result$wins_after_first_set_win), ]
  
  # Print the summary table to the console
  print(result)
  
  # Return the summary data frame as the function output
  return(result)
}


Calculate1stSetWonLosses <- function() {
  
  library(stringr)
  
  # Filter out abnormal match results as in previous function
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN"), ]
  
  # Helper function to check if the loser won the first set based on score string
  lost_after_winning_1st_set <- function(score_string) {
    # Return NA if score is missing or empty
    if (is.na(score_string) || score_string == "") return(NA)
    
    # Remove tiebreak scores inside parentheses
    score_clean <- gsub("\\(.*?\\)", "", score_string)
    
    # Split the score string into sets by space
    sets <- strsplit(score_clean, " ", fixed = TRUE)[[1]]
    if (length(sets) < 1) return(NA)
    
    # Get the first set score
    first_set <- sets[1]
    
    # Split first set games into winner's and loser's games
    games <- unlist(strsplit(first_set, "-", fixed = TRUE))
    if (length(games) != 2) return(NA)
    
    winner_games <- as.integer(games[1])
    loser_games <- as.integer(games[2])
    
    # The loser won the first set if their games count is higher than the winner's
    return(loser_games > winner_games)
  }
  
  # Apply the helper function to the 'score' column, creating a logical vector
  db$lost_after_winning_1st_set <- sapply(db$score, lost_after_winning_1st_set)
  
  # Calculate total losses per player
  total_losses <- table(db$loser_name)
  
  # Calculate number of losses after winning the first set per player
  losses_after_1st_set_win <- tapply(db$lost_after_winning_1st_set, db$loser_name, function(x) sum(x, na.rm = TRUE))
  
  # Create a summary data frame
  result <- data.frame(
    player = names(total_losses),
    total_losses = as.integer(total_losses),
    losses_after_winning_1st_set = as.integer(losses_after_1st_set_win)
  )
  
  # Sort results by descending losses after winning the first set
  result <- result[order(-result$losses_after_winning_1st_set), ]
  
  # Print the summary
  print(result)
  
  # Return the summary data frame
  return(result)
}

Calculate1stSetWonPercent <- function(min_matches = 100) {
  
  library(stringr)
  
  # Filter out incomplete or invalid matches
  db <- db[!db$score %in% c("W/O", "DEF") & 
             !str_detect(db$score, "WEA") & 
             !str_detect(db$score, "ABN") & 
             !str_detect(db$score, "RET"), ]
  
  # Helper function to determine first set winner
  first_set_winner <- function(score_string) {
    if (is.na(score_string) || score_string == "") return(NA)
    score_clean <- gsub("\\(.*?\\)", "", score_string)
    sets <- strsplit(score_clean, " ", fixed = TRUE)[[1]]
    if (length(sets) < 1) return(NA)
    games <- unlist(strsplit(sets[1], "-", fixed = TRUE))
    if (length(games) != 2) return(NA)
    p1 <- as.integer(games[1])
    p2 <- as.integer(games[2])
    if (is.na(p1) || is.na(p2)) return(NA)
    if (p1 > p2) return("winner")
    else return("loser")
  }
  
  db$first_set_winner <- sapply(db$score, first_set_winner)
  
  # Build dataset in long format
  winner_df <- data.frame(
    player = db$winner_name,
    won_match = TRUE,
    won_first_set = db$first_set_winner == "winner"
  )
  
  loser_df <- data.frame(
    player = db$loser_name,
    won_match = FALSE,
    won_first_set = db$first_set_winner == "loser"
  )
  
  all_matches <- rbind(winner_df, loser_df)
  all_matches <- all_matches[!is.na(all_matches$won_first_set), ]
  
  matches_with_first_set_win <- all_matches[all_matches$won_first_set, ]
  
  wins <- aggregate(won_match ~ player, matches_with_first_set_win, sum)
  totals <- table(matches_with_first_set_win$player)
  
  matches_won <- wins$won_match[match(names(totals), wins$player)]
  matches_won[is.na(matches_won)] <- 0
  
  result <- data.frame(
    player = names(totals),
    matches_won_after_1st_set = matches_won,
    total_matches_with_1st_set_won = as.integer(totals)
  )
  
  result$win_percent_after_1st_set <- round(100 * result$matches_won_after_1st_set / result$total_matches_with_1st_set_won, 2)
  
  result <- result[result$total_matches_with_1st_set_won >= min_matches, ]
  result <- result[order(-result$win_percent_after_1st_set), ]
  
  print(result)
  return(result)
}


