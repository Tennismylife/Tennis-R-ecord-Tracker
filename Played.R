PlayedOverall <- function() {
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "WEA")]
  
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


PlayedCategory <- function(category) {
  
  ## only select tournaments in the previously defined pool
  db <- db[tourney_level == category]
  
  #drop not played matches
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
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
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
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


PlayedTour <- function(id) {
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  db <- db[tourney_id == id]
  
  db <- db[!db$score=="W/O" & !db$score=="DEF"]
  
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