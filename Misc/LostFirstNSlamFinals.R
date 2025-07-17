source("Reader.R")

# Read database using custom function
db <- ParallelReaderATP()

# Extract the year from the tournament ID (first 4 characters)
db$year <- stringr::str_sub(db$tourney_id, 1, 4)

# Filter for Grand Slam tournaments only
db <- subset(db, tourney_level == 'G')

# Filter for final matches only (round equals "F")
final_matches <- db[db$round == "F", ]

# Function to count Slam titles incrementally for a player over their first n finals
count_slam_titles_incremental <- function(player, n) {
  # Get all finals involving the player (either winner or loser)
  player_matches <- final_matches[final_matches$winner_name == player | final_matches$loser_name == player, ]
  
  # Calculate cumulative sum of finals won by the player (winner_name == player)
  player_wins <- cumsum(player_matches$winner_name == player)
  
  # Return a vector of wins up to n finals, padding with NAs if fewer finals played
  return(c(player_wins[1:n], rep(NA, max(0, n - length(player_wins)))))
}

# Get all unique players who have appeared in at least one final
all_players <- unique(c(final_matches$winner_name, final_matches$loser_name))

# Count how many finals each player has played
count_finals <- table(c(final_matches$winner_name, final_matches$loser_name))

# Filter players who played at least 3 finals
players <- names(count_finals[count_finals >= 3])

# Create a result dataframe with players who have at least 3 finals
result <- data.frame(player = players)

# For each number of finals from 1 to 25, calculate the incremental count of Slam titles
for (i in 1:25) {
  col_name <- paste0("Slam_", i)
  
  # For each player, get the number of titles after their i-th final
  result[[col_name]] <- sapply(players, function(x) {
    wins <- count_slam_titles_incremental(x, i)
    # Return wins for i-th final or NA if not enough finals played
    return(ifelse(length(wins) >= i, wins[i], NA))
  })
}

# Print the result table
print(result)

# Save the results as an HTML table
write_tableHTML(tableHTML(result), file = "Test.html")
