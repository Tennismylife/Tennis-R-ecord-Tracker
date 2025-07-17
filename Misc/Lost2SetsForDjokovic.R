library(stringr)
library(tableHTML)

# Load the database
db <- ParallelReaderATP()

# Filter out matches with invalid or incomplete scores (walkovers, defaults, withdrawals, retirements)
db <- db[!db$score %in% c("W/O", "DEF") & 
           !str_detect(db$score, "WEA|ABN|RET"), ]

# Extract the year from tournament ID (first 4 characters)
db$year <- substr(db$tourney_id, 1, 4)

# Function to count the number of sets lost by a player in a match based on the score string
# 'is_winner' indicates whether to interpret the score from the winner's or loser's perspective
count_sets_lost <- function(score, is_winner) {
  if (is.na(score) || score == "") return(0)
  
  # Split the score into individual set results
  sets <- unlist(strsplit(score, " "))
  
  # For each set, determine if the player lost that set
  sum(sapply(sets, function(set) {
    # Remove tiebreak details (e.g., "(7)" in "7-6(7)")
    set <- sub("\\(.*\\)", "", set)
    parts <- as.numeric(unlist(strsplit(set, "-")))
    if (length(parts) == 2) {
      # If we are the winner, we lost a set if our games are fewer than the opponent's
      # If we are the loser, we lost a set if our games are more than the opponent's (rare case)
      if ((is_winner && parts[1] < parts[2]) || (!is_winner && parts[1] > parts[2])) {
        return(1)
      }
    }
    return(0)
  }))
}

# Filter matches where Novak Djokovic was either winner or loser, at Grand Slam level,
# and only rounds Round of 128 (R128) or Round of 64 (R64)
djokovic_matches <- db[
  (db$winner_name == "Novak Djokovic" | db$loser_name == "Novak Djokovic") &
    db$tourney_level == "G" &
    db$round %in% c("R128", "R64"),
]

# Compute sets lost by Djokovic for each match
djokovic_matches$sets_lost <- mapply(
  count_sets_lost,
  djokovic_matches$score,
  djokovic_matches$winner_name == "Novak Djokovic"
)

# Sum the sets lost by Djokovic per tournament
sets_lost_per_tournament <- aggregate(
  sets_lost ~ tourney_id,
  data = djokovic_matches,
  FUN = sum
)

# Keep only tournaments where Djokovic lost at least 2 sets
tournaments_with_losses <- sets_lost_per_tournament[sets_lost_per_tournament$sets_lost >= 2, ]

# Select matches from tournaments where Djokovic lost 2 or more sets
result <- djokovic_matches[djokovic_matches$tourney_id %in% tournaments_with_losses$tourney_id, ]

# Select relevant columns to output
result <- result[, c("tourney_name", "year", "surface", "round", "winner_name", "loser_name", "score")]

# Save the result as an HTML table
write_tableHTML(tableHTML(result), file = "Test.html")
