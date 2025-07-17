library(tableHTML)
library(data.table)
library(stringr)

# Load the custom data reader function
source("Reader.R")

# Main function to count longest consecutive streaks of players reaching at least a target round,
# with an additional filter on player age (max_age)
count_consecutive_rounds_all_players <- function(matches, target_round, max_age) {
  
  # Define the hierarchy of tournament rounds from earliest to latest
  rounds_order <- c("R256", "R128", "R64", "R32", "R16", "QF", "SF", "3rd", "BR", "F", "W")
  
  # Find the index of the target round within the rounds hierarchy
  target_index <- match(target_round, rounds_order)
  
  # Stop if the target round is invalid (not found in rounds_order)
  if (is.na(target_index)) stop("Invalid target_round: ", target_round)
  
  # Select players who reached the target round at least once
  # Special case: if target_round is "W" (Winner), then look at players who won the final (round "F")
  if (target_round == "W") {
    round_players <- matches$winner_name[matches$round == "F"]
  } else {
    round_players <- matches$winner_name[matches$round == target_round]
  }
  
  # Count how many times each player appeared as a winner in the target round
  player_counts <- table(round_players)
  
  # Extract player names who have at least one appearance in the target round
  players <- names(player_counts[player_counts >= 1])
  
  results_list <- list()  # To store each player’s streak results
  
  # Loop over each eligible player
  for (player_name in players) {
    
    # Filter matches involving the current player (either as winner or loser)
    player_matches <- matches[matches$winner_name == player_name | matches$loser_name == player_name, ]
    
    # Sort these matches chronologically by year and tournament date
    player_matches <- player_matches[order(player_matches$year, player_matches$tourney_date), ]
    
    # Initialize streak counters and placeholders for tracking streak start/end
    consecutive_count <- 0
    max_consecutive_count <- 0
    start_tourney <- NA
    start_year <- NA
    end_tourney <- NA
    end_year <- NA
    current_start_tourney <- NA
    current_start_year <- NA
    
    # Iterate over all matches for the player
    for (i in 1:nrow(player_matches)) {
      row <- player_matches[i, ]
      
      # Get the round index for the current match
      round_index <- match(row$round, rounds_order)
      if (is.na(round_index)) next  # Skip unknown rounds
      
      # Determine the player’s age in this match
      age <- if (row$winner_name == player_name) row$winner_age else row$loser_age
      
      # If age is missing or exceeds max_age threshold, reset the streak counters
      if (is.na(age) || age >= max_age) {
        consecutive_count <- 0
        current_start_tourney <- NA
        current_start_year <- NA
        next
      }
      
      if (target_round == "W") {
        # Special handling when target is winner of tournament (W)
        if (row$round == "F" && row$winner_name == player_name) {
          # Player won final - increment streak
          if (consecutive_count == 0) {
            current_start_tourney <- row$tourney_name
            current_start_year <- row$year
          }
          consecutive_count <- consecutive_count + 1
          
          # Update max streak info if needed
          if (consecutive_count > max_consecutive_count) {
            max_consecutive_count <- consecutive_count
            start_tourney <- current_start_tourney
            start_year <- current_start_year
            end_tourney <- row$tourney_name
            end_year <- row$year
          }
          
        } else if (row$round == "F" && row$loser_name == player_name) {
          # Player lost final - streak ends
          consecutive_count <- 0
          current_start_tourney <- NA
          current_start_year <- NA
        }
      } else {
        # For other rounds than winner:
        # Check if player participated in the target round in this match (either winner or loser)
        if ((row$winner_name == player_name || row$loser_name == player_name) && round_index == target_index) {
          # Starting a new streak if none ongoing
          if (consecutive_count == 0) {
            current_start_tourney <- row$tourney_name
            current_start_year <- row$year
          }
          consecutive_count <- consecutive_count + 1
          
          # Update max streak info if needed
          if (consecutive_count > max_consecutive_count) {
            max_consecutive_count <- consecutive_count
            start_tourney <- current_start_tourney
            start_year <- current_start_year
            end_tourney <- row$tourney_name
            end_year <- row$year
          }
        } else if (row$loser_name == player_name && round_index < target_index) {
          # Player lost before reaching the target round → streak breaks
          consecutive_count <- 0
          current_start_tourney <- NA
          current_start_year <- NA
        }
      }
    }
    
    # Calculate player's age at the end of the longest streak tournament
    age <- NA
    if (!is.na(end_tourney) && !is.na(end_year)) {
      end_matches <- player_matches[
        player_matches$tourney_name == end_tourney & player_matches$year == end_year &
          (player_matches$winner_name == player_name | player_matches$loser_name == player_name),
      ]
      
      ages <- ifelse(
        end_matches$winner_name == player_name,
        end_matches$winner_age,
        end_matches$loser_age
      )
      
      age <- max(ages, na.rm = TRUE)
      if (is.infinite(age)) age <- NA
    }
    
    # Save the streak results for the current player
    results_list[[length(results_list) + 1]] <- data.frame(
      player_name = player_name,
      consecutive_count = max_consecutive_count,
      start_tourney = start_tourney,
      start_year = start_year,
      end_tourney = end_tourney,
      end_year = end_year,
      age = age,
      stringsAsFactors = FALSE
    )
  }
  
  # Combine all player streaks into a single data.table and return
  results <- rbindlist(results_list, fill = TRUE)
  return(results)
}

# --- Execution ---

# Read the tennis matches database using the custom function
db <- ParallelReaderATP()

# Extract the year (as integer) from tournament ID
db$year <- as.integer(str_sub(db$tourney_id, 1, 4))

# Filter out round robin matches
db <- db[db$round != 'RR', ]

# Filter to include only Grand Slam tournaments
db <- db[db$tourney_level == 'G', ]

# Further filter to include only Wimbledon tournaments
db <- db[db$tourney_name == 'Wimbledon', ]

# Set parameters: target round and maximum player age allowed
target_round <- "QF"  # Looking for consecutive tournament wins
max_age <- 22.181    # Max age cutoff for matches included in streak calculation

# Calculate the consecutive streaks based on criteria
result <- count_consecutive_rounds_all_players(db, target_round, max_age)

# Sort results by longest streak descending
setorder(result, -consecutive_count, na.last = FALSE)

# Format the age column (if you have a FormatAge function)
result <- FormatAge(result)

# Print the final results
print(result)

# Save results to an HTML file for viewing
write_tableHTML(tableHTML(result), file = "AllStreaks.html")
