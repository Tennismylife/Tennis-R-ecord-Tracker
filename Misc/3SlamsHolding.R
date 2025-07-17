library(stringr)
library(tableHTML)

# Function to normalize Slam tournament names
normalize_slam_name <- function(name) {
  name <- gsub("Roland Garros", "French Open", name)
  name <- gsub("US Open", "US Open", name)
  name <- gsub("Australian Open", "Australian Open", name)
  name <- gsub("Wimbledon", "Wimbledon", name)
  return(name)
}

# Official Slam order (can be used later for checking correct sequence, if needed)
slam_order <- c("Australian Open", "French Open", "Wimbledon", "US Open")

# Load the ATP dataset
matches <- ParallelReaderATP()

# Keep only Grand Slam matches
matches <- matches[tourney_level == 'G']

# Extract year from the tournament ID
matches$year <- stringr::str_sub(matches$tourney_id, 0 ,4)

# Normalize Slam names to unify naming
matches$tourney_name <- normalize_slam_name(matches$tourney_name)

# Keep only Grand Slam finals
gs_matches <- matches[matches$tourney_name %in% slam_order & matches$round == "F", ]

# Sort matches in chronological order by tournament date
gs_matches <- gs_matches[order(gs_matches$tourney_date), ]

# Initialize list to collect qualifying windows
results <- list()

# Sliding window of 4 consecutive Slam finals
for (i in 1:(nrow(gs_matches) - 3)) {
  window <- gs_matches[i:(i + 3), ]
  winners <- unique(window$winner_name)  # Get unique winners in the window
  
  for (w in winners) {
    wins <- window[window$winner_name == w, ]  # Filter wins for this player in the window
    
    # Player must have won at least 3 of the 4 finals
    if (nrow(wins) >= 3) {
      slam_won <- unique(wins$tourney_name)
      
      # And must have won at least 3 different Slam tournaments
      if (length(slam_won) >= 3) {
        
        # Create descriptive strings: list of tournaments won (with year)
        wins_in_order <- window[window$winner_name == w, ]
        slams_list <- paste(wins_in_order$tourney_name, wins_in_order$year)
        slams_str <- paste(slams_list, collapse = ", ")
        
        # Store the result for this qualifying case
        result_row <- data.frame(
          winner_name = w,
          num_titles = nrow(wins),
          unique_slams = length(slam_won),
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
  final_result <- do.call(rbind, results)  # Combine list into a single data frame
  
  # Remove duplicate results (same player, years, and Slam titles)
  final_result <- final_result[!duplicated(final_result[c("winner_name", "years", "slams")]), ]
  
  print(final_result)
  
  # Export to HTML
  write_tableHTML(tableHTML(final_result), file = "Test.html")
  
} else {
  # No qualifying player found
  cat("No player has won at least 3 different Slams in 4 consecutive tournaments.\n")
}
