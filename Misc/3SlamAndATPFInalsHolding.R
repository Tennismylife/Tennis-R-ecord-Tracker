library(stringr)
library(tableHTML)

# Function to normalize different names of the same tournament (especially ATP Finals)
normalize_slam_name <- function(name) {
  # Replace "Roland Garros" with "French Open"
  name <- gsub("Roland Garros", "French Open", name)
  
  # Historical name patterns for ATP Finals
  finals_patterns <- c(
    "ATP World Tour Finals",
    "Nitto ATP Finals",
    "Tennis Masters Cup",
    "ATP Tour World Championship",
    "Masters Cup",
    "Tour Finals",
    "^Masters"  # Matches names starting with "Masters"
  )
  
  # Replace any finals variant with "ATP Finals"
  for (p in finals_patterns) {
    name <- gsub(p, "ATP Finals", name)
  }
  
  return(name)
}

# List of valid tournaments to consider (Slams and Finals)
valid_tourneys <- c("Australian Open", "French Open", "Wimbledon", "US Open", "ATP Finals")
# Just the Slam names
slam_names <- c("Australian Open", "French Open", "Wimbledon", "US Open")

# Load the ATP match database
matches <- ParallelReaderATP()

# Keep only Grand Slams ('G') and Finals ('F')
matches <- matches[matches$tourney_level %in% c('G', 'F'), ]
# Extract year from tourney_id (first 4 characters)
matches$year <- str_sub(matches$tourney_id, 1, 4)

# Normalize tournament names (to unify variants)
matches$tourney_name <- normalize_slam_name(matches$tourney_name)

# Keep only the finals of Slams and ATP Finals
matches <- matches[
  matches$tourney_name %in% valid_tourneys & matches$round == "F", 
]

# Sort the dataset by tournament date
matches <- matches[order(matches$tourney_date), ]

# Initialize list to store results
results <- list()

# Sliding window of 5 consecutive tournaments
for (i in 1:(nrow(matches) - 4)) {
  window <- matches[i:(i + 4), ]
  winners <- unique(window$winner_name)
  
  for (w in winners) {
    # Get all wins for this player in the current 5-tournament window
    wins <- window[window$winner_name == w, ]
    
    # Must have won at least 4 tournaments including the ATP Finals
    if (nrow(wins) >= 4 && "ATP Finals" %in% wins$tourney_name) {
      # Check how many unique Slams were won
      slams_won <- unique(wins$tourney_name[wins$tourney_name %in% slam_names])
      
      # Must have won at least 3 different Slams
      if (length(slams_won) >= 3) {
        # Prepare string description of wins (tournament + year)
        slams_list <- paste(wins$tourney_name, wins$year)
        slams_str <- paste(slams_list, collapse = ", ")
        
        # Collect the result for this window and player
        result_row <- data.frame(
          winner_name = w,
          num_titles = nrow(wins),
          unique_slams = length(slams_won),
          years = paste(sort(unique(wins$year)), collapse = ", "),
          slams = slams_str,
          slams_in_window = paste(paste(window$tourney_name, window$year), collapse = " | "),
          start_index = i,
          stringsAsFactors = FALSE
        )
        
        results[[length(results) + 1]] <- result_row
      }
    }
  }
}

# Final output
if (length(results) > 0) {
  # Combine all individual rows into a single data frame
  final_result <- do.call(rbind, results)
  
  # Remove duplicates based on player, years, and slam combination
  final_result <- final_result[!duplicated(final_result[c("winner_name", "years", "slams")]), ]
  
  # Print result to console
  print(final_result)
  
  # Export the result to HTML
  write_tableHTML(tableHTML(final_result), file = "Test.html")
  
} else {
  # No player met the criteria
  cat("No player has won at least 3 different Slams and the ATP Finals within 5 consecutive tournaments.\n")
}
