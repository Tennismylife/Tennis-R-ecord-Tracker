library(data.table)
library(tableHTML)
library(stringr)

source("Reader.R")  # Load your custom data loading function

# Load the dataset
db <- ParallelReaderATP()

# Extract year from tournament ID (first 4 characters)
db[, year := str_sub(tourney_id, 1, 4)]

# Find all players who have ever been ranked #1 (as winners or losers)
players_rank_1 <- unique(c(
  db$winner_name[db$winner_rank == 1],
  db$loser_name[db$loser_rank == 1]
))

# Filter matches involving these #1 ranked players (either winner or loser)
filtered_matches <- db[winner_name %in% players_rank_1 | loser_name %in% players_rank_1]

# Order matches by winner's height in descending order (NA values last)
setorder(filtered_matches, -winner_ht, na.last = TRUE)

# Select relevant columns for output
stat <- filtered_matches[, .(tourney_name, year, surface, round, winner_name, winner_ht, loser_name, score)]

# Print the result to console
print(stat)

# Export the result as an HTML table
write_tableHTML(tableHTML(stat), file = "Test.html")
