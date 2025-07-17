source("Reader.R")
library(stringr)
library(tableHTML)
library(parallel)
library(plyr)

# Read database from csv
db <- ParallelReaderATP()

db <- db[!db$score=="W/O" & !str_detect(db$score, "DEF") & !db$score=="(ABN)" & !str_detect(db$score, "WEA")]

db$year <- stringr::str_sub(db$tourney_id, 0 ,4)

calculate_all_consecutive_losses <- function(data, player_name) {
  streaks <- list()
  current_streak <- 0
  streak_start <- 0
  
  for (i in 1:nrow(data)) {
    if (data$loser_name[i] == player_name) {
      current_streak <- current_streak + 1
      if (current_streak == 1) {
        streak_start <- i
      }
    } else if (data$winner_name[i] == player_name) {
      if (current_streak > 0) {
        streaks[[length(streaks) + 1]] <- list(
          streak = current_streak,
          start = streak_start,
          end = i - 1  # The last loss before the win
        )
        current_streak <- 0
      }
    }
  }
  
  # Capture the last streak if the player ends the data set on a loss
  if (current_streak > 0) {
    streaks[[length(streaks) + 1]] <- list(
      streak = current_streak,
      start = streak_start,
      end = nrow(data)
    )
  }
  
  return(streaks)
}

calculate_player_streaks <- function(data, player, min_streak = 20) {
  player_streaks <- calculate_all_consecutive_losses(data, player)
  streaks_list <- list()
  
  for (streak in player_streaks) {
    if (streak$streak >= min_streak) {  
      start_info <- data[streak$start, c("tourney_name", "year", "round", "score", "winner_name")]
      end_info <- if (streak$end <= nrow(data)) data[streak$end + 1, c("tourney_name", "year", "round", "score", "loser_name")] else list(tourney_name = NA, year = NA, round = NA, score = NA, loser_name = NA)
      
      streaks_list[[length(streaks_list) + 1]] <- data.frame(
        player_name = player,
        streak_length = streak$streak,
        start_tourney = start_info$tourney_name,
        start_year = start_info$year,
        start_round = start_info$round,
        start_winner_name = start_info$winner_name,
        start_score = start_info$score,  # Score of the first loss in the streak
        end_tourney = end_info$tourney_name,
        end_year = end_info$year,
        end_round = end_info$round,
        end_loser_name = end_info$loser_name,  # The player who ended the streak
        end_score = end_info$score  # Score of the match that ended the streak
      )
    }
  }
  
  return(streaks_list)
}

calculate_all_filtered_consecutive_losses <- function(data, min_losses = 40, min_streak = 20, cores = detectCores() - 1) {
  total_losses <- as.data.frame(table(data$loser_name))
  colnames(total_losses) <- c("player_name", "total_losses")
  
  eligible_players <- total_losses$total_losses >= min_losses
  filtered_players <- total_losses[eligible_players, "player_name"]
  
  # Use parallel processing
  cl <- makeCluster(cores)
  clusterExport(cl, c("calculate_player_streaks", "calculate_all_consecutive_losses", "data"), envir = environment())
  
  all_streaks <- parLapply(cl, filtered_players, function(player) {
    calculate_player_streaks(data, player, min_streak)
  })
  
  stopCluster(cl)
  
  # Combine all results 
  all_streaks_df <- do.call(c, all_streaks)
  all_streaks_df <- rbind.fill(all_streaks_df)
  
  # Sort by streak_length in descending order
  return(all_streaks_df[order(-all_streaks_df$streak_length), ])
}

# Execute function
all_results <- calculate_all_filtered_consecutive_losses(db, min_losses = 300, min_streak = 10)
print(all_results)

write_tableHTML(tableHTML(all_results), file = paste("AllStreaksLosses.html"))