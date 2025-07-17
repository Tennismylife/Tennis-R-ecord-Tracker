MostAces <- function() {
  
  # Remove rows with invalid score values
  db <- db[!db$score %in% c("W/O", "DEF", "ABN", "(ABN)"), ]
  
  # Select relevant columns and standardize names
  winner_aces <- db[, c("winner_name", "w_ace")]
  loser_aces  <- db[, c("loser_name", "l_ace")]
  
  names(winner_aces) <- c("name", "aces")
  names(loser_aces)  <- c("name", "aces")
  
  # Combine both data frames
  res <- rbind(winner_aces, loser_aces)
  
  # Replace NA values with 0
  res$aces[is.na(res$aces)] <- 0
  
  # Ensure aces are numeric
  res$aces <- as.numeric(res$aces)
  
  # Aggregate total aces by player
  totalAces <- aggregate(aces ~ name, data = res, sum)
  
  # Order by descending number of aces
  totalAces <- totalAces[order(-totalAces$aces), ]
  
  print(totalAces)
  
  # Print the summary
  print(totalAces)
  
  # Return the summary data frame
  return(totalAces)
}

MostAcesinASeasonByPlayer <- function() {
  
  player <- 'Roger Federer'
  
  # extract year from tourney_id
  db[, year := str_sub(tourney_id, 1, 4)]
  
  # filter matches where the player was the winner or the loser
  db1 <- db[winner_name == player]
  winner_aces <- db1[, .(name = winner_name, aces = as.numeric(w_ace), year)]
  
  db2 <- db[loser_name == player]
  loser_aces <- db2[, .(name = loser_name, aces = as.numeric(l_ace), year)]
  
  # combine and clean
  res <- rbindlist(list(winner_aces, loser_aces), fill = TRUE)
  res[is.na(aces), aces := 0]
  
  # aggregate aces by player and year
  totalAces <- res[, .(aces = sum(aces)), by = .(name, year)]
  
  # order descending by aces
  setorder(totalAces, -aces)
  
  # Print the summary
  print(totalAces)
  
  # Return the summary data frame
  return(totalAces)
}


MostAcesInSlamByPlayer <- function() {

  player <- 'Roger Federer'
  
  # Filter only Grand Slam matches where the player participated
  matches <- db[tourney_level == 'G' & (winner_name == player | loser_name == player),
                .(tourney_id, tourney_name, winner_name, loser_name, w_ace, l_ace)]
  
  # Replace NA values with 0 in ace columns
  matches[is.na(w_ace), w_ace := 0]
  matches[is.na(l_ace), l_ace := 0]
  
  # Calculate number of aces served by the player in each match
  matches[, total_aces := ifelse(loser_name == player, l_ace, w_ace)]
  
  # Ensure consistent player labeling
  matches[, player_name := player]
  
  # Aggregate total aces per Slam edition (tourney_id)
  aces_by_tournament <- matches[, .(aces = sum(total_aces)), by = .(tourney_id, player_name)]
  
  # Get unique official tournament names (filtered from Round of 16 to ensure real events)
  official_names <- unique(db[round == 'R16' & tourney_level == 'G', .(tourney_id, tourney_name)])
  
  # Join tournament names with ace counts
  aces <- right_join(official_names, aces_by_tournament, by = "tourney_id")
  
  # Extract year from tourney_id
  aces[, year := str_sub(tourney_id, 1, 4)]
  
  # Reorder and filter only tournaments with more than 80 aces
  aces <- aces[aces > 80, .(player = player_name, tourney_name, year, aces)]
  
  # Sort by number of aces descending
  setorder(aces, -aces, na.last = FALSE)
  
  # Print result
  print(aces)
}


RatioAcesinASlamByPlayer <- function() {

  player <- 'Roger Federer'
  
  # Filter for Grand Slam matches with the player involved
  player_matches <- db[tourney_level == 'G' & (winner_name == player | loser_name == player)]
  
  # Select relevant columns
  player_matches <- player_matches[, .(winner_name, loser_name, tourney_id, tourney_name,
                                       tourney_date, w_ace, l_ace, w_svpt, l_svpt)]
  
  # Replace NA with 0
  player_matches[is.na(player_matches)] <- 0
  
  # Determine aces and service points from the player perspective
  player_matches[, total_aces := ifelse(loser_name == player, l_ace, w_ace)]
  player_matches[, total_svpts := ifelse(loser_name == player, l_svpt, w_svpt)]
  player_matches[, player := ifelse(loser_name == player, loser_name, winner_name)]
  
  # Aggregate per tournament
  aces_summary <- player_matches[, .(aces = sum(total_aces)), by = .(tourney_id, player)]
  svpts_summary <- player_matches[, .(serve_pts = sum(total_svpts)), by = .(tourney_id, player)]
  
  # Merge the summaries
  stat <- merge(aces_summary, svpts_summary, by = c("tourney_id", "player"))
  
  # Get official tournament names from R16 stage
  slams <- db[round == 'R16' & tourney_level == 'G']
  official_names <- unique(slams[, .(tourney_id, tourney_name)])
  
  # Merge with stat
  stat <- merge(official_names, stat, by = "tourney_id", all.y = TRUE)
  
  # Extract year
  stat[, year := str_sub(tourney_id, 1, 4)]
  
  # Replace NA with 0
  stat[is.na(stat)] <- 0
  
  # Calculate ratio
  stat[, ratio := round(aces / serve_pts, 4)]
  
  # Order by descending ratio
  setorder(stat, -ratio, na.last = TRUE)
  
  # Select and print final result
  final <- stat[, .(tourney_name, year, player, aces, serve_pts, ratio)]
  print(final)
}


MostAcesBySlammerinASlam <- function() {
  
  library(imputeTS)
  library(stringr)
  
  # Filter final matches to get Slam winners
  res <- db[db$round == 'F', c("tourney_id", "winner_name")]
  
  # Filter Grand Slam matches with ace data
  stat <- db[db$tourney_level == 'G', c("tourney_id", "winner_name", "w_ace")]
  
  # Keep only matches where the winner ended up winning the tournament
  wins <- merge(stat, res, by = c("tourney_id", "winner_name"))
  
  # Replace missing ace values with 0
  wins$w_ace <- na.replace(wins$w_ace, 0)
  
  # Calculate total aces by winner and tournament
  aces <- aggregate(wins$w_ace, 
                    by = list(tourney_id = wins$tourney_id, winner_name = wins$winner_name), 
                    FUN = sum)
  
  names(aces)[3] <- "total_aces"
  
  # Extract official tournament names from round of 16 matches
  res_r16 <- db[db$round == 'R16', c("tourney_id", "tourney_name")]
  official_names <- unique(res_r16)
  
  # Join tournament names to ace totals
  aces <- merge(official_names, aces, by = "tourney_id", all.y = TRUE)
  
  # Extract year from tourney_id
  aces$tourney_year <- str_sub(aces$tourney_id, 1, 4)
  
  # Reorder columns for final output
  aces <- aces[, c("tourney_name", "tourney_year", "winner_name", "total_aces")]
  
  # Sort by total aces in descending order
  aces <- aces[order(-aces$total_aces), ]
  
  print(aces)
}



AcesInAMatch <- function(){
  
  # Remove team events (e.g., Davis Cup, Laver Cup) from the dataset
  db <- removeTeamEvents(db)
  
  # Replace all NA values in the dataset with 0
  db[is.na(db)] <- 0
  
  # Make a copy of the cleaned dataset for further processing
  stat <- copy(db)
  
  # Optional filters (uncomment to apply):
  # Keep only best-of-3 matches
  # stat <- stat[best_of == '3']
  
  # Keep only matches played on clay courts
  # stat <- stat[surface == 'Clay']
  
  # Extract the tournament year from the first 4 characters of 'tourney_id'
  stat[, year := stringr::str_sub(tourney_id, 1, 4)]
  
  # Convert ace columns to integers (in case they're characters)
  stat[, w_ace := as.integer(w_ace)]
  stat[, l_ace := as.integer(l_ace)]
  
  # Replace any remaining NAs in ace columns with 0
  stat[is.na(w_ace), w_ace := 0]
  stat[is.na(l_ace), l_ace := 0]
  
  # Create a new column for total aces in the match (winner + loser)
  stat[, totalaces := w_ace + l_ace]
  
  # Filter matches where total aces exceed 16
  stat <- stat[totalaces > 16]
  
  # Sort the dataset by total aces in descending order
  setorder(stat, -totalaces, na.last = FALSE)
  
  # Keep only the relevant columns for the final output
  stat <- stat[, .(tourney_name, year, round, winner_name, loser_name, score, totalaces)]
  
  # Return the filtered and formatted dataset
  return(stat)
}