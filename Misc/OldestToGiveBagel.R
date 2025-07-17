library(dplyr)
library(tableHTML)
library(stringr)

# Load custom functions and dataset
source("Reader.R")        # This should define ParallelReaderATP()
source("AgeFormat.R")     # This should define FormatAge()

# Read the dataset and extract the year from the tournament ID
matches <- ParallelReaderATP()
matches <- subset(matches, tourney_level == 'G')  # Keep only Grand Slam matches
matches$year <- str_sub(matches$tourney_id, 1, 4) # Extract year from tourney_id

# Function to detect who gave the "bagel" (6-0 set)
who_gave_bagel <- function(score_str) {
  if (is.na(score_str) || score_str == "") return(NA)
  
  sets <- unlist(strsplit(score_str, " "))
  for (set_score in sets) {
    clean_set <- gsub("\\(.*\\)|[[:alpha:]]+", "", set_score)  # Remove tiebreaks and terms like 'ret'
    if (clean_set == "6-0") return("winner")
    if (clean_set == "0-6") return("loser")
  }
  return(NA)
}

# Apply the bagel detection function to each match score
bagel_giver <- sapply(matches$score, who_gave_bagel)

# Filter matches where a bagel occurred
bagel_df <- matches[!is.na(bagel_giver), ]
bagel_df$bagel_by <- bagel_giver[!is.na(bagel_giver)]

# Add the player who gave the bagel and their age
bagel_df$player <- ifelse(bagel_df$bagel_by == "winner", bagel_df$winner_name, bagel_df$loser_name)
bagel_df$age <- ifelse(bagel_df$bagel_by == "winner", bagel_df$winner_age, bagel_df$loser_age)

# Select all relevant columns for final output
result <- bagel_df[, c("tourney_name", "year", "round", "winner_name", "loser_name", "score", "player", "age")]

# Sort by age in descending order (older players first)
result_sorted <- result[order(-as.numeric(result$age)), ]

# Format age (e.g., from 27.45 to "27y 5m")
result_sorted <- FormatAge(result_sorted)

# Export the result as an HTML table
write_tableHTML(
  tableHTML(result_sorted, rownames = FALSE),
  file = "Test.html"
)
