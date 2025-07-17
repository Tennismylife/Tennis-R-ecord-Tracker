library(data.table)
library(stringr)
library(tableHTML)

source("Reader.R")
# Read the dataset (replace with your actual data loading function)
db <- ParallelReaderATP()

# Filter out unwanted matches and focus on Australian Open only
db <- db[!score %in% c("W/O", "DEF") & 
           !str_detect(score, "WEA") & 
           !str_detect(score, "ABN") &
           tourney_name == "Australian Open"]

# Extract year from tournament ID (assuming first 4 chars are year)
db[, year := str_sub(tourney_id, 1, 4)]

# Function to check if the match went to the 5th set
# Assumes 'score' is a string with set scores separated by spaces, e.g. "6-3 4-6 7-5 6-7 6-4"
is_fifth_set <- function(score) {
  # Count sets by counting spaces + 1
  num_sets <- str_count(score, " ") + 1
  return(num_sets == 5)
}

# Add a logical column indicating if match ended in 5 sets
db[, is_fifth := is_fifth_set(score)]

# Find all players’ matches (both winners and losers)
player_matches <- rbind(
  db[, .(player_name = winner_name, score, is_fifth, tourney_name, year, round)],
  db[, .(player_name = loser_name, score, is_fifth, tourney_name, year, round)]
)

# Sort matches by player and year (consider adding exact date if available)
setorder(player_matches, player_name, year)

# Identify consecutive streaks of matches with same 'is_fifth' status
player_matches[, streak := rleid(is_fifth), by = player_name]

# Calculate streak lengths and keep only streaks of length 5 where all matches were 5-set matches
streaks <- player_matches[, .(streak_length = .N, is_fifth_val = first(is_fifth)), by = .(player_name, streak)]
valid_streaks <- streaks[is_fifth_val == TRUE & streak_length == 5]

# Join back to get details of matches in these streaks
five_set_streak_matches <- merge(player_matches, valid_streaks, by = c("player_name", "streak"))

# Select relevant columns for output
result <- five_set_streak_matches[, .(tourney_name, year, round, player_name, score)]

# Export the result to an HTML file
write_tableHTML(tableHTML(result), file = "Test.html")
