

GamesLost <- function(target_round = "F"){
  
  library(stringr)
  library(tableHTML)
  
  # Filter only Grand Slam db
  db <- db[db$tourney_level == 'G', ]
  
  # If a specific round is requested (not 0), filter by that round
  if (target_round != 0) {
    db <- db[db$round == target_round, ]
  }
  
  # Function to compute how many games the losing player won
  games_won_by_loser <- function(score) {
    score_clean <- gsub("RET", "", score)     # Remove 'RET'
    score_clean <- trimws(score_clean)
    sets <- unlist(strsplit(score_clean, " "))  # Split into sets
    
    games <- sapply(sets, function(set_score) {
      clean_score <- gsub("\\(.*\\)", "", set_score)  # Remove tiebreak
      scores <- unlist(strsplit(clean_score, "-"))
      if (length(scores) == 2) {
        return(as.numeric(scores[2]))  # Loser's games
      } else {
        return(0)  # Malformed score
      }
    })
    
    sum(games, na.rm = TRUE)
  }
  
  # Calculate games won by the loser for each match
  db$games_won_by_loser <- sapply(db$score, games_won_by_loser)
  
  # Select relevant columns
  db <- db[, c("tourney_name", "year", "surface", "round",
               "winner_name", "loser_name", "score", "games_won_by_loser")]
  
  # Sort db by games_won_by_loser ascending (most dominant first)
  matches <- db[order(db$games_won_by_loser), ]
  
  matches
  
  return(matches)
}

