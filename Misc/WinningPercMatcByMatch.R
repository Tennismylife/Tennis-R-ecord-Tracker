library(tableHTML)
require(stringr)
source("Reader.R")

# Filter matches for a specific player and extract their win percentage
filter_player_percentage <- function(data, player) {
  filtered_data <- data %>%
    # Keep only matches where the player was either the winner or loser and the win percentage is not NA
    filter((winner_name == player & !is.na(winner_win_percentage)) | 
             (loser_name == player & !is.na(loser_win_percentage))) %>%
    # Create a new column selecting the appropriate percentage depending on whether the player won or lost
    mutate(player_percentage = ifelse(winner_name == player, winner_win_percentage, loser_win_percentage)) %>%
    # Format percentage with two decimals and comma as decimal mark
    mutate(player_percentage = format(player_percentage, nsmall = 2, decimal.mark = ",")) %>%
    # Select relevant columns for output
    select(winner_name, loser_name, score, year, tourney_name, round, player_percentage)
  
  return(filtered_data)
}

# Compute the win percentage of each player incrementally, match by match
calculate_win_percentage <- function(data) {
  # Initialize a data frame to store number of wins and matches per player
  player_stats <- data.frame(player = character(), wins = integer(), matches = integer())
  
  # Add columns to store win percentages (initially NA)
  data <- data %>%
    mutate(winner_win_percentage = NA_real_, loser_win_percentage = NA_real_)
  
  # Iterate through each match to update statistics
  for (i in 1:nrow(data)) {
    winner <- data$winner_name[i]
    loser <- data$loser_name[i]
    
    # Update stats for winner
    if (!(winner %in% player_stats$player)) {
      # First match for this player — add to stats
      player_stats <- rbind(player_stats, data.frame(player = winner, wins = 1, matches = 1))
    } else {
      # Increment win and match count
      player_stats[player_stats$player == winner, "wins"] <- player_stats[player_stats$player == winner, "wins"] + 1
      player_stats[player_stats$player == winner, "matches"] <- player_stats[player_stats$player == winner, "matches"] + 1
    }
    
    # Update stats for loser
    if (!(loser %in% player_stats$player)) {
      # First match for this player — add to stats
      player_stats <- rbind(player_stats, data.frame(player = loser, wins = 0, matches = 1))
    } else {
      # Increment only match count
      player_stats[player_stats$player == loser, "matches"] <- player_stats[player_stats$player == loser, "matches"] + 1
    }
    
    # Compute win percentage for the winner
    winner_win_percentage <- player_stats[player_stats$player == winner, "wins"] /
      player_stats[player_stats$player == winner, "matches"] * 100
    data$winner_win_percentage[i] <- round(winner_win_percentage, 2)
    
    # Compute win percentage for the loser (only if matches > 0)
    if (player_stats[player_stats$player == loser, "matches"] > 0) {
      loser_win_percentage <- player_stats[player_stats$player == loser, "wins"] /
        player_stats[player_stats$player == loser, "matches"] * 100
      data$loser_win_percentage[i] <- round(loser_win_percentage, 2)
    } else {
      data$loser_win_percentage[i] <- 0
    }
  }
  
  return(data)
}

# Load the database using custom function (must be defined in Reader.R)
db <- ParallelReaderATP()

# Extract the year from the tournament ID
db$year <- stringr::str_sub(db$tourney_id, 0, 4)

# Specify the player name to analyze
player_name <- "Novak Djokovic"

# Filter the dataset to only include matches played by the chosen player
db <- db[db$winner_name == player_name | db$loser_name == player_name, ]

# Remove matches with no result (walkovers, weather issues, etc.)
db <- db[!db$score == "W/O" & !db$score == "DEF" & 
           !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN"), ]

# Apply the win percentage calculation to the dataset
data_with_win_percentage <- calculate_win_percentage(db)

# Filter the data to only include rows related to the selected player and format output
filtered_data <- filter_player_percentage(data_with_win_percentage, player_name)

# Display the result in console
print(filtered_data)

# Export the result as an HTML table
write_tableHTML(tableHTML(filtered_data), file = paste("Test.html"))
