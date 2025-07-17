# Load required packages
library(stringr)
library(tableHTML)

# Read the match data (assumes ParallelReaderATP is already defined)
matches <- ParallelReaderATP()

# Filter for only Grand Slam tournaments ('G' = Slam level)
matches <- matches[matches$tourney_level == 'G', ]

# Extract the year from the tourney_id (first 4 characters)
matches$year <- stringr::str_sub(matches$tourney_id, 1, 4)

# Define the list of the 4 Grand Slam tournament names
slams <- c("Australian Open", "Roland Garros", "Wimbledon", "US Open")

# Filter only Grand Slam finals
final_slam_matches <- matches[matches$tourney_name %in% slams & matches$round == "F", ]

# Create a dataset of all finalists (winners and losers), with player name, slam, year, and age
winners <- data.frame(
  player_name = final_slam_matches$winner_name,
  tourney_name = final_slam_matches$tourney_name,
  year = final_slam_matches$year,
  age = final_slam_matches$winner_age,
  stringsAsFactors = FALSE
)

losers <- data.frame(
  player_name = final_slam_matches$loser_name,
  tourney_name = final_slam_matches$tourney_name,
  year = final_slam_matches$year,
  age = final_slam_matches$loser_age,
  stringsAsFactors = FALSE
)

# Combine winners and losers into one dataset of finalists
finalists <- rbind(winners, losers)

# Sort by player and year to make sure we're processing chronologically
finalists <- finalists[order(finalists$player_name, finalists$year), ]

# Get the list of unique players
players <- unique(finalists$player_name)

# Initialize an empty list to collect results (faster than using rbind in loop)
results_list <- list()

# For each player, check the first match where they completed all 4 different Slam finals
for (p in players) {
  player_data <- finalists[finalists$player_name == p, ]
  
  seen_slams <- character()  # Track which Slam tournaments the player has appeared in
  for (i in 1:nrow(player_data)) {
    seen_slams <- unique(c(seen_slams, player_data$tourney_name[i]))
    
    if (length(seen_slams) == 4) {
      # Player has now appeared in all 4 Slams
      results_list[[length(results_list) + 1]] <- player_data[i, ]
      break  # Stop processing this player
    }
  }
}

# Combine the results into a single data frame
result <- do.call(rbind, results_list)

# Order by age (youngest to oldest when completing all 4 Slams)
result <- result[order(result$age), ]

# Format the age (round to 1 decimal place)
FormatAge <- function(df) {
  df$age <- round(as.numeric(df$age), 1)
  return(df)
}
result <- FormatAge(result)

# Display the result in the console
print(result)

# Export the result as a styled HTML table
write_tableHTML(tableHTML(result), file = paste("Test.html"))

