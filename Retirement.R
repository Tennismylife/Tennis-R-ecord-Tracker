RetirementInSFAndF <- function() {
  
  # Filter only semifinals and finals
  sf_matches <- db[round == "SF"]
  f_matches <- db[round == "F"]
  
  # Extract SF wins due to opponent retirement
  sf_retires <- sf_matches[grepl("RET", score), 
                           .(winner_name, tourney_name, year)]
  
  # Extract Final wins due to opponent retirement
  f_retires <- f_matches[grepl("RET", score), 
                         .(winner_name, tourney_name, year)]
  
  # Find players who won *both* SF and F by retirement in the same tournament and year
  both_retires <- merge(
    sf_retires, 
    f_retires, 
    by = c("winner_name", "tourney_name", "year")
  )
  
  return(both_retires)
}

WinsByRetirement <- function() {
  
  # Filter matches where the opponent retired (RET appears in the score)
  retire_wins <- db[grepl("RET", score), .N, by = winner_name]
  
  # Rename column to indicate it's the number of retirement wins
  setnames(retire_wins, "N", "retire_count")
  
  # Sort the results by number of retirements in descending order
  setorder(retire_wins, -retire_count, na.last = FALSE)
  
  return(retire_wins)
}


RetirementAndWalkover <- function() {
  
  # Filter matches where the score indicates retirement (RET) or walkover (W/O)
  filtered_db <- db[score == "W/O" | str_detect(score, "RET")]
  
  # Count losses due to RET and W/O per player
  results <- filtered_db[, .(
    ret_losses = sum(score != "W/O" & loser_name == loser_name),
    wo_losses = sum(score == "W/O" & loser_name == loser_name)
  ), by = loser_name]
  
  # Add total number of losses by RET or W/O
  results[, total := ret_losses + wo_losses]
  
  # Sort by total losses in descending order
  setorder(results, -total)
  
  return(results)
}

RetirementsInASlam <- function() {
  
  # Filter only Grand Slam matches where the match ended with a retirement
  stat <- db[grepl("RET", score) & tourney_level == "G"]
  
  # Count retirements by tournament
  retire_counts <- stat[, .N, by = tourney_id]
  
  # Get official tournament names
  tourney_names <- unique(db[, .(tourney_id, tourney_name)])
  
  # Merge counts with tournament names
  stat <- merge(tourney_names, retire_counts, by = "tourney_id")
  
  # Extract year from tourney_id (first 4 characters)
  stat[, year := stringr::str_sub(tourney_id, 1, 4)]
  
  # Keep only relevant columns and sort by number of retirements descending
  stat <- stat[, .(tourney_name, year, N)][order(-N)]
  
  return(stat)
}
