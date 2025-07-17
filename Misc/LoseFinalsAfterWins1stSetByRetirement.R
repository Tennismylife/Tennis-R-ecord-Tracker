library(tableHTML)
library(stringr)

# Load custom functions from external scripts
source("Reader.R")
source("Remover.R")

# Read the database
db <- ParallelReaderATP()

# Extract the year from the tournament ID (first 4 characters)
db$year <- substr(db$tourney_id, 1, 4)

# Remove team events using a custom function (assumed to filter out doubles/team matches)
db <- removeTeamEvents(db)

# Filter out matches with walkovers (W/O), defaults (DEF), withdrawals (WEA, ABN), 
# keep only matches with retirements ("RET") and only final round matches ("F")
db <- db[
  !db$score %in% c("W/O", "DEF") &                 # Exclude walkovers and defaults
    !str_detect(db$score, "WEA|ABN") &                # Exclude withdrawals and abandonments
    grepl("RET", db$score) &                           # Keep matches with retirements
    db$round == "F",                                  # Only finals
]

# Function to check if the winner lost the first set based on the score string
has_lost_first_set <- function(score) {
  set_results <- unlist(strsplit(score, " "))                # Split score by sets
  if (length(set_results) == 0) return(FALSE)
  
  # Remove possible tiebreak details from first set score (e.g., 7-6(5) -> 7-6)
  first_set <- gsub("\\(.*\\)", "", set_results[1])
  
  # Check if the first set has a valid score format (with a dash)
  if (!grepl("-", first_set)) return(FALSE)
  
  # Extract numeric scores of the two players from the first set
  scores <- as.numeric(strsplit(first_set, "-")[[1]])
  
  # Return TRUE if the winner lost the first set (i.e., winner's games < loser's games)
  return(length(scores) == 2 && scores[1] < scores[2])
}

# Filter matches where the winner lost the first set
matches_lost_first_set <- db[
  sapply(db$score, has_lost_first_set),
  c("tourney_name", "year", "surface", "round", "winner_name", "loser_name", "score")
]

# Save the filtered matches as an HTML table
write_tableHTML(tableHTML(matches_lost_first_set), file = "Test.html")
