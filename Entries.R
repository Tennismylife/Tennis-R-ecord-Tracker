EntriesOverall <- function() {
  
  db <- removeTeamEvents(db)

  #tournaments won
  wins <- unique(db[,c('winner_name','tourney_name','tourney_date')])
  wins <- dplyr::distinct(wins)
  
  #tournaments lost
  losses <- unique(db[,c('loser_name','tourney_name','tourney_date')])
  losses <- dplyr::distinct(losses)
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, all = TRUE, allow.cartesian=TRUE)
  res <- dplyr::distinct(res)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  entries <- res[,.N, by = .(name)]
  
  setorder(entries, -N, na.last=FALSE)
  
  print(entries)
}

EntriesSurface <- function(court) {
  
  db <- removeTeamEvents(db)
  
  #Select surface
  dbm <- db[surface == court]
  
  #tournaments won
  wins <- unique(dbm[,c('winner_name','tourney_name','tourney_date')])
  wins <- dplyr::distinct(wins)
  
  #tournaments lost
  losses <- unique(dbm[,c('loser_name','tourney_name','tourney_date')])
  losses <- dplyr::distinct(losses)
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, all = TRUE, allow.cartesian=TRUE)
  res <- dplyr::distinct(res)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  entry <- res[,.N, by = .(name)]
  
  #entry <- subset(entry, N > 10)
  
  setorder(entry, -N, na.last=FALSE)
  
  names(entry)[2] <- "entries"
  
  print(entry)
}


EntriesCategory <- function(category) {
  
  ## only select tournaments in the previously defined pool
  dbm <- db[tourney_level == category]
  
  dbm1 <- dbm[round == 'F']
  dbm1 <- unique(dbm1[,c('winner_name','tourney_name','tourney_date')])
  dbm2 <- unique(dbm[,c('loser_name','tourney_name','tourney_date')])
  
  ## wins
  wins <- dbm1[,.N, by = .(winner_name)]
  ## losses
  losses <- dbm2[,.N, by = .(loser_name)]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, entries:=wins+losses]
  
  res <- res[,c('name','entries')]
  
  ## order by decreasing total matches
  setorder(res, -entries)
   
  print(res)
}

EntriesTournament <- function(id) {
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  dbm <- db[tourney_id == id]
  
  #tournaments won
  wins <- unique(dbm[,c('winner_name','tourney_name','tourney_date')])
  wins <- dplyr::distinct(wins)
  
  #tournaments lost
  losses <- unique(dbm[,c('loser_name','tourney_name','tourney_date')])
  losses <- dplyr::distinct(losses)
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, all = TRUE, allow.cartesian=TRUE)
  res <- dplyr::distinct(res)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  entry <- res[,.N, by = .(name)]
  
  setorder(entry, -N, na.last=FALSE)
  
  print(entry)
}



EntrieTournamentByAge <- function(tournament1, tournament2, tournament3) {
  
  ## only select tournaments in the previously defined pool
  dbm <- db[tourney_name == tournament1 | tourney_name == tournament2 | tourney_name == tournament3]
  
  dbm1 <- dbm[round == 'F']
  dbm1 <- unique(dbm1[,c('tourney_name', 'tourney_date', 'winner_ioc', 'winner_name', 'winner_age')])
  
  dbm2 <- unique(dbm[,c('tourney_name', 'tourney_date', 'loser_ioc', 'loser_name', 'loser_age')])
  
  ## wins
  wins <- dbm1
  
  ## losses
  losses <- dbm2

  ## common name to merge with
  names(wins)[3] <- names(losses)[3] <- "flag"
  names(wins)[4] <- names(losses)[4] <- "name"
  names(wins)[5] <- names(losses)[5] <- "age"
  
  ## merge the tables by "name"
  res <- rbind( wins, losses, fill=TRUE)
  res$tourney_date <- substr(res$tourney_date, 0, 4)
  res$age <- substr(res$age, 0, 5)
  res$age <- suppressWarnings(as.numeric(str_replace_all(res$age,pattern=',',replacement='.')))

  ## order by decreasing age
  setorder(res, -age, na.last=FALSE)

  print(res)
}


EntrieCategoryByAge <- function(category) {

  ## only select tournaments in the previously defined pool
  dbm <- db[tourney_level == category]
  
  dbm1 <- dbm[round == 'F']
  dbm1 <- unique(dbm1[,c('tourney_name', 'tourney_date', 'winner_ioc', 'winner_name', 'winner_age')])
  
  dbm2 <- unique(dbm[,c('tourney_name', 'tourney_date', 'loser_ioc', 'loser_name', 'loser_age')])
  
  ## wins
  wins <- dbm1

  ## losses
  losses <- dbm2
  
  ## common name to merge with
  names(wins)[3] <- names(losses)[3] <- "flag"
  names(wins)[4] <- names(losses)[4] <- "name"
  names(wins)[5] <- names(losses)[5] <- "age"
  
  ## merge the tables by "name"
  res <- rbind( wins, losses, fill=TRUE)
  
  res$tourney_date <- substr(res$tourney_date, 0, 4)
  res$age <- substr(res$age, 0, 5)
  res$age <- suppressWarnings(as.numeric(str_replace_all(res$age,pattern=',',replacement='.')))

  ## order by decreasing age
  res <- res[order(-age)]
  
  print(res)
}


EntrieSurfaceByAge <- function(court, order, stage) {
  
  db <- removeTeamEvents(db)
  
  dbm <- db[surface == court]
  
  if(stage != 'W' & stage!='0')
    dbm <- dbm[round == stage]
  
  if(stage == 'W')
    dbm <- dbm[round == 'F']
  
  dbm1 <- unique(dbm[,c('tourney_name', 'tourney_id', 'winner_ioc', 'winner_name', 'winner_age')])
  
  if(stage != 'W')
    dbm2 <- unique(dbm[,c('tourney_name', 'tourney_id', 'loser_ioc', 'loser_name', 'loser_age')])
  
  ## wins
  wins <- dbm1
  
  ## losses
  if(stage != 'W')
    losses <- dbm2
  
  ## common name to merge with
  names(wins)[3] <- "flag"
  names(wins)[4] <- "name"
  names(wins)[5]  <- "age"
  
  if(stage != 'W')
  {
    names(losses)[3] <- "flag"
    names(losses)[4] <- "name"
    names(losses)[5] <- "age"
  }
  
  ## merge the tables by "name"
  if(stage != 'W'){
    res <- rbind(wins, losses, fill=TRUE)
    res <- unique(res)
  }
  else 
    res <- wins
  
  res$tourney_id <- substr(res$tourney_id, 0, 4)
  
  res$age <- substr(res$age, 0, 5)
  res$age <- suppressWarnings(as.numeric(str_replace_all(res$age,pattern=',',replacement='.')))
  
  if(order == 'oldest'){
    ## order by decreasing age
    res <- res[order(-age)] 
  }
  
  if(order == 'youngest'){
    ## order by creasing age
    res <- res[order(age)] 
  }
  
  print(res)
}
