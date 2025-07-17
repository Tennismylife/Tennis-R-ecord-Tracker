source("Reader.R")
library(stringr)
library(tableHTML)
library(parallel)
library(plyr)

# Load match database
db <- ParallelReaderATP()

# Filter out walkovers, abandoned matches, defaults, and weather-affected matches
db <- db[!grepl("W/O|DEF|\\(ABN\\)|WEA", db$score), ]
db$year <- stringr::str_sub(db$tourney_id, 1, 4)

# Filter for Wimbledon tournament only
db <- subset(db, tourney_name == 'Wimbledon')

# Parameters
age <- 22.1               # Age limit for young players
minimum_wins <- 10        # Minimum total wins to be included in the analysis
minimum_streak <- 10      # Minimum length of streak to be considered

# Function to calculate all consecutive win streaks of a player
calculate_all_consecutive_wins <- function(data, player_name) {
  streaks <- list()
  current_streak <- 0
  streak_start <- 0
  
  for (i in 1:nrow(data)) {
    if (data$winner_name[i] == player_name & data$winner_age[i] < age) {
      current_streak <- current_streak + 1
      if (current_streak == 1) {
        streak_start <- i
      }
    } else if (data$loser_name[i] == player_name) {
      if (current_streak > 0) {
        streaks[[length(streaks) + 1]] <- list(
          streak = current_streak,
          start = streak_start,
          end = i - 1
        )
        current_streak <- 0
      }
    }
  }
  
  # Handle final streak if the dataset ends with a win
  if (current_streak > 0) {
    streaks[[length(streaks) + 1]] <- list(
      streak = current_streak,
      start = streak_start,
      end = nrow(data)
    )
  }
  
  return(streaks)
}

# Function to convert streak data into a structured format for a given player
calculate_player_streaks <- function(data, player, min_streak = 10) {
  player_streaks <- calculate_all_consecutive_wins(data, player)
  streaks_list <- list()
  
  for (streak in player_streaks) {
    if (streak$streak >= min_streak) {
      start_info <- data[streak$start, c("tourney_name", "year", "round", "score", "loser_name")]
      
      if (streak$end + 1 <= nrow(data)) {
        end_info <- data[streak$end + 1, c("tourney_name", "year", "round", "score", "winner_name")]
      } else {
        end_info <- data.frame(tourney_name = NA, year = NA, round = NA, score = NA, winner_name = NA)
      }
      
      streaks_list[[length(streaks_list) + 1]] <- data.frame(
        player_name = player,
        streak_length = streak$streak,
        start_tourney = start_info$tourney_name,
        start_year = start_info$year,
        start_round = start_info$round,
        start_loser_name = start_info$loser_name,
        start_score = start_info$score,
        end_tourney = end_info$tourney_name,
        end_year = end_info$year,
        end_round = end_info$round,
        end_winner_name = end_info$winner_name,
        end_score = end_info$score,
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(streaks_list)
}

# Main function to calculate filtered winning streaks for all players in parallel
calculate_all_filtered_consecutive_wins <- function(data, min_wins = 10, min_streak = 10, cores = detectCores() - 1) {
  total_wins <- as.data.frame(table(data$winner_name))
  colnames(total_wins) <- c("player_name", "total_wins")
  
  eligible_players <- total_wins$total_wins >= min_wins
  filtered_players <- total_wins[eligible_players, "player_name"]
  
  # Start parallel cluster
  cl <- makeCluster(cores)
  
  # Export necessary variables and functions to each worker
  clusterExport(cl, c("calculate_player_streaks", 
                      "calculate_all_consecutive_wins", 
                      "data", 
                      "age", 
                      "min_streak"), envir = environment())
  
  clusterEvalQ(cl, {
    library(stringr)
    library(plyr)
  })
  
  # Apply the calculation in parallel
  all_streaks <- parLapply(cl, filtered_players, function(player) {
    calculate_player_streaks(data, player, min_streak)
  })
  
  stopCluster(cl)
  
  # Combine and sort the results
  all_streaks_df <- do.call(c, all_streaks)
  all_streaks_df <- rbind.fill(all_streaks_df)
  
  return(all_streaks_df[order(-all_streaks_df$streak_length), ])
}

# Run the analysis
all_results <- calculate_all_filtered_consecutive_wins(db, min_wins = minimum_wins, min_streak = minimum_streak)
print(all_results)

# Save results as HTML table
write_tableHTML(tableHTML(all_results), file = "AllStreaks.html")
