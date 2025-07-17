# Load required libraries
library(dplyr)
library(stringr)
library(tableHTML)

# Function to count max consecutive sets won against Top 10 opponents
count_consecutive_sets_top10 <- function(player_name, data) {
  consecutive_sets <- 0
  max_consecutive_sets <- 0
  start_match <- NULL
  max_start_match <- NULL
  max_end_match <- NULL
  
  # Filter matches where the player played against a Top 10 opponent
  player_data <- data %>% filter(
    (winner_name == player_name & loser_rank <= 10) |
      (loser_name == player_name & winner_rank <= 10)
  )
  
  for (i in seq_len(nrow(player_data))) {
    score <- unlist(strsplit(player_data$score[i], " "))
    
    for (set in score) {
      set <- gsub("\\(.*\\)", "", set)  # Remove any tiebreak info (e.g., 7-6(5) → 7-6)
      games <- unlist(strsplit(set, "-"))
      
      # Ensure the score is valid
      if (length(games) == 2 && !any(is.na(as.numeric(games)))) {
        # Check if the player won the set
        if ((player_data$winner_name[i] == player_name && as.numeric(games[1]) > as.numeric(games[2])) ||
            (player_data$loser_name[i] == player_name && as.numeric(games[2]) > as.numeric(games[1]))) {
          
          if (consecutive_sets == 0) {
            start_match <- player_data[i, ]
          }
          
          consecutive_sets <- consecutive_sets + 1
        } else {
          # If streak breaks, check if it's the longest so far
          if (consecutive_sets > max_consecutive_sets) {
            max_consecutive_sets <- consecutive_sets
            max_start_match <- start_match
            max_end_match <- player_data[i, ]
          }
          consecutive_sets <- 0
          start_match <- NULL
        }
      }
    }
  }
  
  # Final check at end of loop
  if (consecutive_sets > max_consecutive_sets) {
    max_consecutive_sets <- consecutive_sets
    max_start_match <- start_match
    max_end_match <- player_data[nrow(player_data), ]
  }
  
  return(list(
    max_consecutive_sets = max_consecutive_sets, 
    start_match = max_start_match, 
    end_match = max_end_match
  ))
}

# -----------------------------
# Data Preparation
# -----------------------------
source("Reader.R")
db <- ParallelReaderATP()

# Clean the data
db <- db %>% 
  filter(score != "W/O" & !str_detect(score, "DEF") & score != "(ABN)" & !str_detect(score, "WEA")) %>%
  mutate(
    year = str_sub(tourney_id, 1, 4),
    score = gsub('RET|ABD|\\[|\\]', '', score),
    winner_rank = ifelse(is.na(winner_rank), 9999, winner_rank),
    loser_rank = ifelse(is.na(loser_rank), 9999, loser_rank)
  ) %>%
  filter(winner_rank < 11 | loser_rank < 11)  # Keep only matches with Top 10 players involved

data <- db

# -----------------------------
# Process All Players
# -----------------------------
players <- unique(c(data$winner_name, data$loser_name))

# Initialize results table
results <- data.frame(
  player = character(), 
  max_consecutive_sets_top10 = numeric(), 
  start_tourney = character(), 
  start_year = character(), 
  start_round = character(), 
  start_score = character(), 
  start_loser = character(), 
  end_tourney = character(), 
  end_year = character(), 
  end_round = character(), 
  end_score = character(), 
  end_winner = character(), 
  stringsAsFactors = FALSE
)

# Main loop
for (player in players) {
  result <- count_consecutive_sets_top10(player, data)
  
  # Extract match details
  start_tourney <- ifelse(is.null(result$start_match), NA, result$start_match$tourney_name)
  start_year <- ifelse(is.null(result$start_match), NA, result$start_match$year)
  start_round <- ifelse(is.null(result$start_match), NA, result$start_match$round)
  start_loser <- ifelse(is.null(result$start_match), NA, result$start_match$loser_name)
  start_score <- ifelse(is.null(result$start_match), NA, result$start_match$score)
  
  end_tourney <- ifelse(is.null(result$end_match), NA, result$end_match$tourney_name)
  end_year <- ifelse(is.null(result$end_match), NA, result$end_match$year)
  end_round <- ifelse(is.null(result$end_match), NA, result$end_match$round)
  end_winner <- ifelse(is.null(result$end_match), NA, result$end_match$winner_name)
  end_score <- ifelse(is.null(result$end_match), NA, result$end_match$score)
  
  # Append to result table
  results <- rbind(results, data.frame(
    player = player, 
    max_consecutive_sets_top10 = result$max_consecutive_sets, 
    start_tourney = start_tourney, 
    start_year = start_year, 
    start_round = start_round, 
    start_score = start_score, 
    start_loser = start_loser, 
    end_tourney = end_tourney, 
    end_year = end_year, 
    end_round = end_round, 
    end_score = end_score, 
    end_winner = end_winner
  ))
}

# Sort by most consecutive sets won
results <- results[order(-results$max_consecutive_sets_top10), ]

# Print the results
print(results)

# Export to HTML
write_tableHTML(tableHTML(results), file = paste("AllStreaks.html"))
