# Load required libraries
source("Reader.R")               # Custom data reading function
library(stringr)
library(tableHTML)

# Read the match database
db <- ParallelReaderATP()

# Remove matches with invalid or incomplete scores
db <- db[!db$score == "W/O" &                     # Walkover
           !str_detect(db$score, "DEF") &           # Default
           !db$score == "(ABN)" &                   # Abandoned
           !str_detect(db$score, "WEA")]            # Weather-related

# Extract year from tournament ID
db$year <- stringr::str_sub(db$tourney_id, 0, 4)

# Clean up score field
db$score <- gsub('RET', '', db$score)            # Remove 'RET' (retired)
db$score <- gsub('ABD', '', db$score)            # Remove 'ABD' (abandoned)
db$score <- gsub("\\[|\\]", "", db$score)        # Remove brackets (e.g., from tiebreaks)

# Optional filter for surface (uncomment to use)
# db <- db[surface == 'Hard']

# Alias
df <- db

# -------------------------------------------
# Main function: Calculates all streaks of ≥ 20 consecutive sets won
# -------------------------------------------
calculate_all_consecutive_wins <- function(df) {
  
  # Get all unique player names
  players <- unique(c(df$winner_name, df$loser_name))
  
  # Initialize result table
  results <- data.frame(
    player_name = character(), 
    start_tourney = character(), 
    start_year = integer(), 
    start_round = character(), 
    start_opponent = character(), 
    start_score = character(), 
    end_tourney = character(), 
    end_year = integer(), 
    end_round = character(), 
    end_opponent = character(), 
    end_score = character(), 
    consecutive_wins = integer(), 
    stringsAsFactors = FALSE
  )
  
  # Track number of consecutive sets won per player
  consecutive_wins_list <- setNames(rep(0, length(players)), players)
  
  # Store starting point of streak
  start_info <- list()
  
  # Loop through all matches
  for (i in 1:nrow(df)) {
    print(df$tourney_date[i])  # Print current processing date (for monitoring)
    
    scores <- unlist(strsplit(df$score[i], " "))   # Split match score into individual sets
    winner <- df$winner_name[i]
    loser <- df$loser_name[i]
    tourney <- df$tourney_name[i]
    year <- df$year[i]
    round <- df$round[i]
    
    # Loop through each set in the match
    for (score in scores) {
      sets <- unlist(strsplit(score, "-"))         # Split set score (e.g., 6-3)
      
      # Handle edge cases
      if (length(sets) < 2) next
      
      set1 <- as.numeric(sets[1])
      set2 <- as.numeric(gsub("\\(.*\\)", "", sets[2]))  # Remove tiebreak details
      
      if (is.na(set1) || is.na(set2)) next
      
      if (set1 > set2) {
        # Winner won the set
        if (consecutive_wins_list[winner] == 0) {
          start_info[[winner]] <- list(tourney = tourney, year = year, opponent = loser, score = df$score[i], round = round)
        }
        consecutive_wins_list[winner] <- consecutive_wins_list[winner] + 1
        
        # Check if loser's streak ends (and was ≥ 20)
        if (consecutive_wins_list[loser] >= 20) {
          results <- rbind(results, data.frame(
            player_name = loser, 
            start_tourney = start_info[[loser]]$tourney, 
            start_year = start_info[[loser]]$year, 
            start_round = start_info[[loser]]$round, 
            start_opponent = start_info[[loser]]$opponent, 
            start_score = start_info[[loser]]$score, 
            end_tourney = tourney, 
            end_year = year, 
            end_round = round, 
            end_opponent = winner, 
            end_score = df$score[i], 
            consecutive_wins = consecutive_wins_list[loser]
          ))
        }
        consecutive_wins_list[loser] <- 0
        
      } else {
        # Loser won the set
        if (consecutive_wins_list[loser] == 0) {
          start_info[[loser]] <- list(tourney = tourney, year = year, opponent = winner, score = df$score[i], round = round)
        }
        consecutive_wins_list[loser] <- consecutive_wins_list[loser] + 1
        
        # Check if winner's streak ends (and was ≥ 20)
        if (consecutive_wins_list[winner] >= 20) {
          results <- rbind(results, data.frame(
            player_name = winner, 
            start_tourney = start_info[[winner]]$tourney, 
            start_year = start_info[[winner]]$year, 
            start_round = start_info[[winner]]$round, 
            start_opponent = start_info[[winner]]$opponent, 
            start_score = start_info[[winner]]$score, 
            end_tourney = tourney, 
            end_year = year, 
            end_round = round, 
            end_opponent = loser, 
            end_score = df$score[i], 
            consecutive_wins = consecutive_wins_list[winner]
          ))
        }
        consecutive_wins_list[winner] <- 0
      }
    }
  }
  
  # Check for any remaining ongoing streaks at end of loop
  for (player in names(consecutive_wins_list)) {
    if (consecutive_wins_list[player] >= 20) {
      results <- rbind(results, data.frame(
        player_name = player, 
        start_tourney = start_info[[player]]$tourney, 
        start_year = start_info[[player]]$year, 
        start_round = start_info[[player]]$round, 
        start_opponent = start_info[[player]]$opponent, 
        start_score = start_info[[player]]$score, 
        end_tourney = tourney, 
        end_year = year, 
        end_round = round, 
        end_opponent = ifelse(player == winner, loser, winner), 
        end_score = df$score[i], 
        consecutive_wins = consecutive_wins_list[player]
      ))
    }
  }
  
  # Sort results by most consecutive sets won
  results <- results[order(-results$consecutive_wins), ]
  
  return(results)
}

# Run the function
results <- calculate_all_consecutive_wins(df)

# Print result table
print(results)

# Export results to HTML
write_tableHTML(tableHTML(results), file = paste("Test.html"))
