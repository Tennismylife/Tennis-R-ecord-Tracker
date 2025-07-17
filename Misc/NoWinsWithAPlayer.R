# Load external script or function definitions (assumed to provide ParallelReaderATP)
source("Reader.R")

# Define a function to find players who have beaten a given player
# but have never lost to that player in any match
find_undefeated_opponents <- function(data, target_player) {
  # Make sure winner and loser names are character strings
  data$winner_name <- as.character(data$winner_name)
  data$loser_name <- as.character(data$loser_name)
  
  # Check if the target player appears in the dataset
  if (!(target_player %in% c(data$winner_name, data$loser_name))) {
    stop("Target player not found in the dataset.")
  }
  
  # Identify unique players who defeated the target player
  winners_against_target <- unique(data$winner_name[data$loser_name == target_player])
  
  # Identify unique players who lost to the target player
  losers_against_target <- unique(data$loser_name[data$winner_name == target_player])
  
  # Find players who have beaten the target player but never lost to them
  undefeated <- setdiff(winners_against_target, losers_against_target)
  
  return(undefeated)
}

# Load the dataset using a custom function (likely reads ATP tennis matches)
db <- ParallelReaderATP()

# Ensure the player name columns are treated as character vectors
db$winner_name <- as.character(db$winner_name)
db$loser_name <- as.character(db$loser_name)

# Specify the player to analyze
target_player <- "Roger Federer"

# Use the function to get players undefeated by the target player but who beat him
undefeated_players <- find_undefeated_opponents(db, target_player)

# Load the package for creating HTML tables
library(tableHTML)

# Convert the vector of player names into a data frame for nicer output
df_undefeated <- data.frame(Player = undefeated_players, stringsAsFactors = FALSE)

# Save the resulting table to an HTML file named "Test.html"
write_tableHTML(tableHTML(df_undefeated), file = "Test.html")
