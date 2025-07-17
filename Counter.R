library(stringr)

CountOverallRound <- function(stage) {
  
  # Remove team events (assuming this function modifies db accordingly)
  db <- removeTeamEvents(db)
  
  # Filter out score strings with "WEA" and "ABN" if stage is 'W'
  if(stage == 'W') {
    db <- db[!str_detect(score, "WEA") & !str_detect(score, "ABN")]
    db <- db[round == 'F' & !str_detect(score, "WEA")]
  } else {
    db <- db[round == stage]
  }
  
  ## Wins summary
  wins <- db[, .N, by=winner_name]
  setnames(wins, old = "winner_name", new = "name")
  setnames(wins, old = "N", new = "wins")
  
  if(stage != 'W') {
    ## Losses summary
    losses <- db[, .N, by=loser_name]
    setnames(losses, old = "loser_name", new = "name")
    setnames(losses, old = "N", new = "losses")
    
    ## Merge wins and losses
    res <- merge(wins, losses, by = "name", all = TRUE)
    res[is.na(res)] <- 0
    res[, played := wins + losses]
    
    ## Order by played descending
    setorder(res, -played)
    
    ## Select output columns
    res <- res[, .(name, played)]
  } else {
    ## For stage 'W', just wins
    res <- wins
    res[is.na(res)] <- 0
    setorder(res, -wins)
    res <- res[, .(name, wins)]
  }
  
  print(res)
  
  return(res)
}



CountSurfaceRound <- function(court, stage) {
  
  # Remove team events first
  db <- removeTeamEvents(db)
  
  # Filter for the specified court surface
  db <- db[surface == court]
  
  # Filter by stage
  if(stage != 'W') {
    db <- db[round == stage]
  } else {
    db <- db[round == 'F' & !str_detect(score, "ABN") & !str_detect(score, "WEA")]
  }
  
  # Summarize wins
  wins <- db[, .N, by = winner_name]
  setnames(wins, old = c("winner_name", "N"), new = c("name", "wins"))
  
  if(stage != 'W') {
    # Summarize losses
    losses <- db[, .N, by = loser_name]
    setnames(losses, old = c("loser_name", "N"), new = c("name", "losses"))
    
    # Merge wins and losses by name
    res <- merge(wins, losses, by = "name", all = TRUE)
    res[is.na(res)] <- 0
    res[, played := wins + losses]
    
    # Order by number of matches played descending
    setorder(res, -played)
    
    # Select only name and played columns
    res <- res[, .(name, played)]
    
  } else {
    # For 'W' stage only wins matter
    res <- wins
    res[is.na(res)] <- 0
    setorder(res, -wins)
    res <- res[, .(name, wins)]
  }
  
  print(res)
  
  return(res)
}



CountCategoryRound <- function(category, stage) {
  
  # Filter by tournament category
  db <- db[tourney_level == category]
  
  # Filter by stage
  if(stage != 'W') {
    db <- db[round == stage]
  } else {
    db <- db[round == 'F' & !str_detect(score, "ABN") & !str_detect(score, "WEA")]
  }
  
  # Wins count
  wins <- db[, .N, by = winner_name]
  setnames(wins, old = c("winner_name", "N"), new = c("name", "wins"))
  
  if(stage != 'W') {
    # Losses count
    losses <- db[, .N, by = loser_name]
    setnames(losses, old = c("loser_name", "N"), new = c("name", "losses"))
    
    # Merge wins and losses
    res <- merge(wins, losses, by = "name", all = TRUE)
    res[is.na(res)] <- 0
    res[, played := wins + losses]
    
    # Order by total matches played
    setorder(res, -played)
    res <- res[, .(name, played)]
  } else {
    # Only wins count for stage 'W'
    res <- wins
    res[is.na(res)] <- 0
    setorder(res, -wins)
    res <- res[, .(name, wins)]
  }
  
  print(res)
  
  return(res)
}



CountTourRound <- function(id, stage) {
  
  # Clean tourney_id inside a local copy to avoid modifying original db
  db <- copy(db)  # make a local copy if db is a data.table
  
  # Remove prefix from tourney_id (everything before first hyphen)
  db[, tourney_id := sub("^[^-]*-", "", tourney_id)]
  
  # Filter by tournament id
  db <- db[tourney_id == id]
  
  # Filter by stage
  if(stage != 'W') {
    db <- db[round == stage]
  } else {
    db <- db[round == 'F' & !str_detect(score, "ABN") & !str_detect(score, "WEA")]
  }
  
  # Wins summary
  wins <- db[, .N, by = winner_name]
  setnames(wins, old = c("winner_name", "N"), new = c("name", "wins"))
  
  if(stage != 'W') {
    # Losses summary
    losses <- db[, .N, by = loser_name]
    setnames(losses, old = c("loser_name", "N"), new = c("name", "losses"))
    
    # Merge wins and losses
    res <- merge(wins, losses, by = "name", all = TRUE)
    res[is.na(res)] <- 0
    res[, played := wins + losses]
    
    # Order by matches played descending
    setorder(res, -played)
    res <- res[, .(name, played)]
  } else {
    res <- wins
    res[is.na(res)] <- 0
    setorder(res, -wins)
    res <- res[, .(name, wins)]
  }
  
  print(res)
  
  return(res)
}

