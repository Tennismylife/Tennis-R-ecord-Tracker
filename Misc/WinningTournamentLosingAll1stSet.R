library(data.table)
library(stringr)

# Load data (assuming these functions exist in Reader.R and Remover.R)
source("Reader.R")
source("Remover.R")

# Load and preprocess data
db <- ParallelReaderATP()
db[, year := str_sub(tourney_id, 1, 4)]
db <- removeTeamEvents(db)
db <- db[!(score %in% c("W/O", "DEF")) & !str_detect(score, "WEA|ABN")]

# Filter matches by tournament winner
filter_matches_by_winner <- function(data) {
  winner <- unique(data[round == "F", winner_name])
  data[winner_name %in% winner]
}

matches <- db[, filter_matches_by_winner(.SD), by = tourney_id]

# Check if winner lost first set
has_lost_first_set <- function(score) {
  first_set <- strsplit(score, " ")[[1]][1]
  first_set <- gsub("\\(.*\\)", "", first_set)
  set_scores <- as.numeric(strsplit(first_set, "-")[[1]])
  set_scores[1] < set_scores[2]
}

# Add column for first set loss
matches[, lost_first_set := sapply(score, has_lost_first_set)]

# Find tournaments where the winner lost the first set in every match
filtered_tournaments <- matches[, .(all_lost_first_set = all(lost_first_set)), 
                                by = .(tourney_name, year, winner_name)][all_lost_first_set == TRUE]

# Output
print(filtered_tournaments)

# Save to HTML
write_tableHTML(tableHTML(filtered_tournaments), file = "Test.html")