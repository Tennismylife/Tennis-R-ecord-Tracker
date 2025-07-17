source("Reader.R")
library(stringr)
library(tableHTML)
library(parallel)
library(plyr)

# Read and filter the database
db <- ParallelReaderATP()
db <- db[!db$score == "W/O" & 
           !str_detect(db$score, "DEF") & 
           !db$score == "(ABN)" & 
           !str_detect(db$score, "WEA"), ]

# Extract year and filter only Hard court Grand Slams
db$year <- stringr::str_sub(db$tourney_id, 1, 4)
db <- subset(db, surface == 'Hard' & tourney_level == 'G')

# Set global parameters
MIN_WINS <- 20          # Minimum total wins to be included
MIN_STREAK <- 10        # Minimum length of a streak to report
MAX_YOUNG_AGE <- 23.44147844  # Age threshold (under 23.44 years old)

# Function to calculate all consecutive wins for a player under the age threshold
calculate_all_consecutive_wins <- function(data, player_name) {
  streaks <- list()
  current_streak <- 0
  streak_start <- 0
  
  for (i in 1:nrow(data)) {
    if (data$winner_name[i] == player_name & data$winner_age[i] < MAX_YOUNG_AGE) {
      current_streak <- current_streak + 1
      if (current_streak == 1) {
        streak_start <- i
      }
    } else if (data$loser_name[i] == player_name) {
      if (current_streak > 0) {
        streaks[[length(streaks) + 1]] <- list(
          streak = current_streak,
          start = streak_start,
          end = i - 1  # The last win before the loss
        )
        current_streak <- 0
      }
    }
  }
  
  # Capture the final streak if the player ends the dataset with a win
  if (current_streak > 0) {
    streaks[[length(streaks) + 1]] <- list(
      streak = current_streak,
      start = streak_start,
      end = nrow(data)
    )
  }
  
  return(streaks)
}

# Function to filter and structure a player's streaks into a data frame
calculate_player_streaks <- function(data, player, min_streak = MIN_STREAK) {
  player_streaks <- calculate_all_consecutive_wins(data, player)
  streaks_list <- list()
  
  for (streak in player_streaks) {
    if (streak$streak >= min_streak) {  
      start_info <- data[streak$start, c("tourney_name", "year", "round", "score", "loser_name")]
      end_info <- if (streak$end + 1 <= nrow(data)) {
        data[streak$end + 1, c("tourney_name", "year", "round", "score", "winner_name")]
      } else {
        list(tourney_name = NA, year = NA, round = NA, score = NA, winner_name = NA)
      }
      
      streaks_list[[length(streaks_list) + 1]] <- data.frame(
        player_name = player,
        streak_length = streak$streak,
        start_tourney = start_info$tourney_name,
        start_year = start_info$year,
        start_round = start_info$round,
        start_score = start_info$score,
        start_loser_name = start_info$loser_name,
        end_tourney = end_info$tourney_name,
        end_year = end_info$year,
        end_round = end_info$round,
        end_winner_name = end_info$winner_name,
        end_score = end_info$score
      )
    }
  }
  
  return(streaks_list)
}

# Main function to compute filtered streaks for all players
calculate_all_filtered_consecutive_wins <- function(data, min_wins = MIN_WINS, min_streak = MIN_STREAK, cores = detectCores() - 1) {
  total_wins <- as.data.frame(table(data$winner_name))
  colnames(total_wins) <- c("player_name", "total_wins")
  
  eligible_players <- total_wins$total_wins >= min_wins
  filtered_players <- total_wins[eligible_players, "player_name"]
  
  # Set up parallel cluster
  cl <- makeCluster(cores)
  clusterExport(cl, c("calculate_player_streaks", 
                      "calculate_all_consecutive_wins", 
                      "data", 
                      "MIN_STREAK", 
                      "MIN_WINS", 
                      "MAX_YOUNG_AGE"), 
                envir = environment())
  
  # Compute in parallel
  all_streaks <- parLapply(cl, filtered_players, function(player) {
    calculate_player_streaks(data, player, MIN_STREAK)
  })
  
  stopCluster(cl)
  
  # Combine and sort results
  all_streaks_df <- do.call(c, all_streaks)
  all_streaks_df <- rbind.fill(all_streaks_df)
  
  return(all_streaks_df[order(-all_streaks_df$streak_length), ])
}

# Run the analysis and save the results
all_results <- calculate_all_filtered_consecutive_wins(db)
print(all_results)

# Export results as HTML
write_tableHTML(tableHTML(all_results), file = "AllStreaks.html")
