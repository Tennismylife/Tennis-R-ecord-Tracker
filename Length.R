MostLengthMatches <- function() {
  
  # Remove team events from the dataset (assuming this function exists)
  db <- removeTeamEvents(db)
  
  stat <- db
  
  # Keep only Grand Slam tournaments (assuming 'G' stands for Grand Slam)
  stat <- stat[tourney_level == 'G']
  
  # Keep only final rounds
  stat <- stat[round == 'F']
  
  # Replace NA values in 'minutes' column with 0
  stat[is.na(stat$minutes)] <- 0
  
  # Keep only matches longer than 1 minute (exclude very short or missing durations)
  stat <- subset(stat, minutes > 1)
  
  # Extract year from the first 4 characters of 'tourney_id' (assumes format starts with year)
  stat$year <- stringr::str_sub(stat$tourney_id, 1, 4)
  
  # Order data by decreasing match length (minutes)
  stat <- setorder(stat, -minutes, na.last = FALSE)
  
  # Select relevant columns for the output
  stat <- stat[, c("tourney_name", "year", "round", "winner_name", "loser_name", "score", "minutes")]
  
  return(stat)
}
