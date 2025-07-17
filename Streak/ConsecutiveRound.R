library(tableHTML)

source("Reader.R")  # Load custom function to read data

# Function to count consecutive appearances of players reaching at least a given tournament round
count_consecutive_rounds_all_players <- function(matches, target_round) {
  
  # Define the hierarchy/order of rounds in a tournament from earliest to latest
  rounds_order <- c("R256", "R128", "R64", "R32", "R16", "QF", "SF", "3rd", "BR", "F")
  
  # Find the index of the target round in the rounds_order vector
  target_index <- match(target_round, rounds_order)
  
  # Identify all players who have won at least one match in the target round
  round_players <- matches$winner_name[matches$round == target_round]
  
  # Count the frequency of players appearing as winners in the target round
  player_counts <- table(round_players)
  
  # Extract player names who appeared at least once in the target round
  players <- names(player_counts[player_counts >= 1])
  
  # Initialize a data frame to store results for each player:
  # their name, max consecutive appearances in target round, and tournament info for streak start and end
  results <- data.frame(
    player_name = character(),
    consecutive_count = integer(),
    start_tourney = character(),
    start_year = integer(),
    end_tourney = character(),
    end_year = integer(),
    stringsAsFactors = FALSE
  )
  
  # Loop over each player who reached the target round at least once
  for (player_name in players) {
    # Subset all matches involving the player (both wins and losses)
    player_matches <- matches[matches$winner_name == player_name | matches$loser_name == player_name, ]
    
    # Initialize counters and placeholders for tracking the current streak
    consecutive_count <- 0
    max_consecutive_count <- 0
    start_tourney <- NA
    start_year <- NA
    end_tourney <- NA
    end_year <- NA
    current_start_tourney <- NA
    current_start_year <- NA
    
    # Iterate over all player matches in chronological order
    for (i in 1:nrow(player_matches)) {
      # Get the round index for this match based on rounds_order
      round_index <- match(player_matches$round[i], rounds_order)
      
      # If the player lost in a round earlier than the target round, reset the current streak
      if (player_matches$loser_name[i] == player_name && round_index < target_index) {
        consecutive_count <- 0
        current_start_tourney <- NA
        current_start_year <- NA
        
        # If the player played (win or loss) exactly in the target round, update the streak
      } else if ((player_matches$winner_name[i] == player_name | player_matches$loser_name[i] == player_name) && round_index == target_index) {
        
        # If starting a new streak, record the start tournament and year
        if (consecutive_count == 0) {
          current_start_tourney <- player_matches$tourney_name[i]
          current_start_year <- player_matches$year[i]
        }
        
        # Increment the consecutive count for appearances at target round
        consecutive_count <- consecutive_count + 1
        
        # Update max streak info if current streak is longer
        if (consecutive_count > max_consecutive_count) {
          max_consecutive_count <- consecutive_count
          start_tourney <- current_start_tourney
          start_year <- current_start_year
          end_tourney <- player_matches$tourney_name[i]
          end_year <- player_matches$year[i]
        }
      }
    }
    
    # Add the player's longest consecutive streak info to results
    results <- rbind(results, data.frame(
      player_name = player_name,
      consecutive_count = max_consecutive_count,
      start_tourney = start_tourney,
      start_year = start_year,
      end_tourney = end_tourney,
      end_year = end_year
    ))
  }
  
  return(results)
}

# Read database using the custom reader function
db <- ParallelReaderATP()

# Extract the year from the tournament ID
db$year <- stringr::str_sub(db$tourney_id, 1, 4)

# Remove round robin matches from the dataset
db <- db[db$round != 'RR']

# Filter dataset to include only Grand Slam level tournaments ('G')
db <- db[tourney_level == 'M']

# Set the target round to Quarterfinals ("QF")
target_round <- "F"

# Call the function to count consecutive appearances at the target round for all players
result <- count_consecutive_rounds_all_players(db, target_round)

# Order the result by descending consecutive counts (players with longest streaks first)
setorder(result, -consecutive_count, na.last=FALSE)

# Print the results
print(result)

# Write the results as an HTML table to file
write_tableHTML(tableHTML(result), file = "AllStreaks.html")
