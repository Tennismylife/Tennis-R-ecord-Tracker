PlayedOverall <- function() {
  # Remove walkovers, defaults, and abandoned matches
  db_filtered <- db[!score %in% c("W/O", "DEF", "ABN")]
  
  # Count total wins per player
  wins <- db_filtered[, .N, by = winner_name]
  
  # Count total losses per player
  losses <- db_filtered[, .N, by = loser_name]
  
  # Rename columns for merging
  setnames(wins, c("winner_name", "N"), c("Player", "wins"))
  setnames(losses, c("loser_name", "N"), c("Player", "losses"))
  
  # Merge wins and losses by player name
  stats <- merge(wins, losses, by = "Player", all = TRUE)
  
  # Replace NA values with 0
  stats[is.na(stats)] <- 0
  
  # Compute total matches played
  stats[, matches := wins + losses]
  
  # Order by total matches played (descending)
  setorder(stats, -matches)
  
  # Select only relevant columns
  result <- stats[, .(Player, matches)]
  
  return(result)
}



PlayedCategory <- function(category) {
  
  ## only select tournaments in the previously defined pool
  db <- db[tourney_level == category]
  
  #drop not played matches
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="ABN"]
  
  ## wins
  wins <- db[,.N, by=winner_name]
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "Player"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("Player"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  ## order by decreasing total matches
  setorder(res, -played)
  
  res <- res[, c("Player", "played")]
  
  names(res)[1] <- "Player"
  names(res)[2] <- "#Ms"
  
  print(res)
}

PlayedSurface <- function(court) {
  
  ## only select tournaments in the previously defined pool
  db <- db[surface == court]
  
  #remove not played matches
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="ABN"]
  
  ## wins
  wins <- db[,.N, by=winner_name]
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "Player"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("Player"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## order by decreasing total matches
  setorder(res, -played)
  
  res <- res[, c("Player", "played")]
  
  names(res)[1] <- "Player"
  names(res)[2] <- "matches"
  
  print(res)
}


PlayedTour <- function(id) {
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  db <- db[tourney_id == id]
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="ABN"]
  
  ## wins
  wins <- db[,.N, by=winner_name]
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "Player"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("Player"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## order by decreasing total matches
  setorder(res, -played)
  
  res <- res[, c("Player", "played")]
  
  names(res)[1] <- "Player"
  names(res)[2] <- "#Ms"
  
  print(res)
}