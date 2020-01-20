SameTournamentNation <- function(stage) {
  db <- removeTeamEvents(db)
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  nat <- 'ITA'
  
  db <- db[db$winner_ioc == nat | db$loser_ioc == nat]
  
  #db <- db[tourney_level == 'G']
  
  db <- db[tourney_name == 'Australian Open']
  
  #tournaments won
  dbm <- db[winner_ioc == nat]
  wins <- unique(dbm[,c('tourney_id','winner_ioc', 'winner_id')])
  wins <- dplyr::distinct(wins)
  
  #tournaments lost
  dbm <- db[loser_ioc == nat]
  losses <- unique(dbm[,c('tourney_id','loser_ioc', 'loser_id')])
  losses <- dplyr::distinct(losses)
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "tourney_id"
  names(wins)[2] <- names(losses)[2] <- "nation"
  
  ## merge the tables by "name"
  res <- rbind(wins, losses, use.names=FALSE)
  res <- dplyr::distinct(res)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  same <- res[, .N, by = list(res$tourney_id, res$nation)]
  
  print(same)
  
  names(same)[1] <- "tourney_id"
  names(same)[2] <- "Nation"
  names(same)[3] <- "N"

  officialName <- unique(db[,c('tourney_id', 'tourney_name')])
  
  same <- join(officialName, same, by="tourney_id")
  
  ## order by decreasing age
  same <- same[order(tourney_id)]
  
  
  print(same)
  
}
