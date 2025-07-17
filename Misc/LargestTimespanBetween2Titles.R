source("Reader.R")

library(data.table)      # For efficient data manipulation
library(stringr)         # For string manipulation functions
library(tableHTML)       # To generate HTML tables

# Load the database (make sure ParallelReaderATP() returns a data.table)
db <- ParallelReaderATP()

# Extract the year from 'tourney_id' (first 4 characters)
db$year <- str_sub(db$tourney_id, 1, 4)  # Note: start=1 (not 0) in str_sub

# Filter the dataset for a specific round, e.g., "F" for finals
round_filter <- "F"
db <- db[round == round_filter]

# Convert 'tourney_date' from numeric or character to Date format
db$tourney_date <- as.Date(as.character(db$tourney_date), format = "%Y%m%d")

# Define a function to calculate days between tournaments for each player
process_player_data <- function(player_data) {
  # Order data by tournament date chronologically
  player_data <- player_data[order(tourney_date)]
  
  # Calculate difference in days between consecutive tournaments
  # The first row will have NA because there is no previous tournament
  player_data[, days_between := c(NA, diff(tourney_date))]
  
  # Add columns for previous tournament name and previous tournament year
  player_data[, previous_tourney := c(NA, head(tourney_name, -1))]
  player_data[, previous_tourney_year := c(NA, head(format(tourney_date, "%Y"), -1))]
  
  # Extract the year of the current tournament as a separate column
  player_data[, tourney_date_year := format(tourney_date, "%Y")]
  
  # Keep only relevant columns and rename winner_name as player
  player_data[, .(
    player = winner_name,
    round,
    previous_tourney,
    previous_tourney_year,
    tourney_name,
    tourney_date_year,
    days_between
  )]
}

# Split the data by each winner and apply the processing function
results_list <- lapply(split(db, db$winner_name), process_player_data)

# Combine the list of data.tables into one data.table
final_results <- rbindlist(results_list, use.names = TRUE, fill = TRUE)

# Remove rows where 'days_between' is NA (i.e., the first tournament per player)
final_results <- final_results[!is.na(days_between)]

# Order the results by descending days between tournaments (largest gap first)
setorder(final_results, -days_between, na.last = FALSE)

# Print a preview of the final results
print(final_results)

# Save the results as an HTML file
write_tableHTML(tableHTML(final_results), file = "Test.html")
