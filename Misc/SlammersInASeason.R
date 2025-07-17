source("Reader.R")

# Read database from csv
db <- ParallelReaderATP()

# Extract year from tourney_date
db$year <- stringr::str_sub(db$tourney_id, 0 ,4)
db$year <- as.numeric(db$year)

slam_winners <- character(0)

# Iterate through the dataframe to collect Slam winners
for(i in 1:nrow(db)) {
  # Check if the tournament is a Slam (tourney_level = "G") and if the round is the final (round = "F")
  if(db$tourney_level[i] == "G" && db$round[i] == "F") {
    winner <- db$winner_name[i]
    # Add the winner to the Slam winners vector if not already present
    if(!(winner %in% slam_winners)) {
      slam_winners <- c(slam_winners, winner)
    }
  }
}

# Function to check if a winner played at least one match in any year after their win
get_years_played_after_win <- function(winner, win_year, db) {
  # Find the years after the win
  years_after_win <- (win_year + 1):max(db$year)
  # Find the years the winner played, removing duplicates
  played_years <- unique(c(
    db$year[db$winner_name == winner & db$year %in% years_after_win],
    db$year[db$loser_name == winner & db$year %in% years_after_win]
  ))
  return(played_years)
}

# Create list for output
output_data <- data.frame(player = character(0), year = numeric(0), played = logical(0))

# For each Slam winner, check if they played in the years after their win and in which years
for(winner in slam_winners) {
  # Find the year of the Slam win for the winner
  win_year <- db$year[db$winner_name == winner & db$tourney_level == "G" & db$round == "F"]
  # Find the years the winner played
  played_years <- unlist(sapply(win_year, function(year) get_years_played_after_win(winner, year, db)))
  
  # Add results to the final dataframe
  if(length(played_years) > 0) {
    for(year in played_years) {
      output_data <- rbind(output_data, data.frame(player = winner, year = year, played = TRUE))
    }
  } else {
    # If they did not play in the following years, add FALSE for each subsequent year
    output_data <- rbind(output_data, data.frame(player = winner, year = (win_year + 1):max(db$year), played = FALSE))
  }
}

# Display the final output
print(output_data)

output_data <- output_data[order(output_data$year), ]

output_data <- unique(output_data)

# Using table() for counting
count_per_year <- table(output_data$year)

# Convert to data frame if necessary
count_per_year <- as.data.frame(count_per_year)
names(count_per_year) <- c("year", "count")

# Print the result
print(count_per_year)

write_tableHTML(tableHTML(output_data), file = paste("Test.html"))
