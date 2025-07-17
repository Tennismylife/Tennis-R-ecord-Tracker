TournamentsPlayedToReachNRound <- function(level = 'M', target_round = 'W', x = 4) {
  
  # Extract year from tourney_id
  db$year <- as.numeric(substr(db$tourney_id, 1, 4))
  
  # Filter by tournament level (e.g., 'G' for Grand Slam)
  db <- db[db$tourney_level == level, ]
  
  # Select relevant columns
  df <- db[, c("tourney_id", "tourney_name", "year", "round", "winner_name", "loser_name")]
  
  # Create a long format: one record per player per match
  winner_df <- df[, c("tourney_id", "tourney_name", "year", "round", "winner_name")]
  colnames(winner_df)[5] <- "player"
  
  loser_df <- df[, c("tourney_id", "tourney_name", "year", "round", "loser_name")]
  colnames(loser_df)[5] <- "player"
  
  long_df <- unique(rbind(winner_df, loser_df))
  long_df <- long_df[order(long_df$player, long_df$year, long_df$tourney_id), ]
  
  # Create a "reached" flag based on the target round
  if (target_round == 'W') {
    # Target = winning the tournament (i.e., winning the final)
    final_winners <- df[df$round == 'F', c("winner_name", "tourney_id")]
    colnames(final_winners)[1] <- "player"
    final_winners$reached <- 1
    reached_df <- unique(final_winners)
  } else {
    # For other rounds (e.g., SF, QF), a player "reaches" it if they played it
    reached_df <- unique(long_df[long_df$round == target_round, c("player", "tourney_id")])
    reached_df$reached <- 1
  }
  
  # Tournament history per player
  player_tourneys <- unique(long_df[, c("player", "tourney_id", "tourney_name", "year")])
  
  # Merge with the "reached" flag
  merged <- merge(player_tourneys, reached_df, by = c("player", "tourney_id"), all.x = TRUE)
  merged$reached[is.na(merged$reached)] <- 0
  
  # Sort by player, year, and tournament
  merged <- merged[order(merged$player, merged$year, merged$tourney_id), ]
  
  # Tournament count for each player (progressive)
  merged$tournament_count <- ave(merged$tourney_id, merged$player, FUN = seq_along)
  # Cumulative count of how many times target round has been reached
  merged$cumulative_reached <- ave(merged$reached, merged$player, FUN = cumsum)
  
  # Keep only rows where the player reached the target for the x-th time
  reached_x <- merged[merged$cumulative_reached == x, ]
  
  # Get the first tournament in which the player reached the target for the x-th time
  result <- aggregate(tournament_count ~ player, data = reached_x, FUN = min)
  colnames(result)[2] <- "tournaments_needed"
  
  # Add tournament name and year for that specific tournament
  result_full <- merge(result, merged[, c("player", "tournament_count", "tourney_name", "year")],
                       by.x = c("player", "tournaments_needed"),
                       by.y = c("player", "tournament_count"),
                       all.x = TRUE)
  
  # Sort and reset row names
  result_full <- result_full[order(as.numeric(result_full$tournaments_needed)), ]
  rownames(result_full) <- NULL
  
  return(result_full)
}
