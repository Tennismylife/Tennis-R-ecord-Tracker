################################################################ PLAYED #####################################################################################

PlayedOverall <- function() {
  dbm <- db
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  ## wins
  wins <- dbm[,.N, by=winner_name]
  ## losses
  losses <- dbm[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  ## order by decreasing total matches
  setorder(res, -played)
  res <- res[1:100,]
  print(res)
}


PlayedCategory <- function(category) {
  
  ## only select tournaments in the previously defined pool
  dbm <- db[tourney_level == category]
  
  #drop not played matches
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  ## wins
  wins <- dbm[,.N, by=winner_name]
  ## losses
  losses <- dbm[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  ## order by decreasing total matches
  setorder(res, -played)
  res <- res[1:100,]
  print(res)
}

PlayedSurface <- function(court) {
  ## only select tournaments in the previously defined pool
  dbm <- db[surface == court]
  
  #remove not played matches
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  ## wins
  wins <- dbm[,.N, by=winner_name]
  ## losses
  losses <- dbm[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## order by decreasing total matches
  setorder(res, -played)
  res <- res[1:100,]
  print(res)
}


PlayedTour <- function(tournament, tournament2, tournament3) {
  
  ## only select tournaments in the previously defined pool
  dbm <- db[tourney_name == tournament | tourney_name == tournament2 | tourney_name == tournament3]
  
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF"]
  
  ## wins
  wins <- dbm[,.N, by=winner_name]
  ## losses
  losses <- dbm[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  ## order by decreasing total matches
  setorder(res, -played)
  res <- res[1:100,]
  print(res)
}



PlayedPlayer <- function(player, year) {
  dbm <- db
  
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)" & !dbm$score=="ABN"]
  dbm <- dbm[winner_name == player | loser_name == player]
  dbm$tourney_id <- stringr::str_sub(dbm$tourney_id, 0 ,4)
  dbm <- dbm[tourney_id == year]

  ## wins
  wins <- dbm[,.N, by=winner_name]
  ## losses
  losses <- dbm[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  res <- res[, percetage:=wins/played]

  ## order by decreasing total matches
  setorder(res, -played)
  #res <- res[1:1,]
  print(year)
  print(res)
}