library(stringr)

SameTournamentRound <- function(stage) {
  db <- removeTeamEvents(db)
  
  dbm <- db
  ## get round matches
  if(stage !='W' & stage !='0')
    dbm <- dbm[round == stage]
  
  if(stage =='W')
    dbm <- dbm[round == 'F']
  
  wins <- dbm[,c('winner_name','tourney_id', 'tourney_name')]
  
  if(stage !='W')
  losses <- dbm[,c('loser_name','tourney_id', 'tourney_name')]
  
  #extract id from tourney_id
  wins$tourney_id <- sub("^[^-]*", "", wins$tourney_id)
  losses$tourney_id <- sub("^[^-]*", "", losses$tourney_id)
  
  names(wins)[1] <- "name"
  
  if(stage !='W')
  names(losses)[1] <- "name"
  
  ## merge the tables by "name"
  if(stage !='W')
  res <- rbind(wins, losses, by = c("name"), fill=TRUE)
  
  if(stage =='W')
  res <- wins
  
  #in the same tournament
  list <- res[, .N, by = list(res$name, res$tourney_id)]
  
  
  #Retrieve the official tournament name
  names(list)[1] <- "Player"
  names(list)[2] <- "tourney_id"
  
  officialName <- unique(db[,c('tourney_id', 'tourney_name')])
  officialName$tourney_id <- sub("^[^-]*", "", officialName$tourney_id)

  same <- left_join(officialName, list, by="tourney_id")
  same <- unique(same)

  same <- same[!duplicated(same[,c('tourney_id', 'Player')]),]
  
  same <- same[,c('Player','tourney_name', 'N')]
  
  ## order by decreasing number
  same <- same[order(-same$N),] 

  #select first 20
  same <- same[1:20,]
  
  print(same)
  
}

SameTournamentEntries <- function(stage) {
  db <- removeTeamEvents(db)
  
  #tournaments won
  wins <- unique(db[,c('winner_name','tourney_name','tourney_date')])
  wins <- dplyr::distinct(wins)
  
  #tournaments lost
  losses <- unique(db[,c('loser_name','tourney_name','tourney_date')])
  losses <- dplyr::distinct(losses)
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- names(losses)[2] <- "tournament"
  names(wins)[3] <- names(losses)[3] <- "date"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, all = TRUE, allow.cartesian=TRUE)
  #res <- dplyr::distinct(res)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  print(res)
  
  same <- res[, .N, by = list(res$name, res$tournament)]
  
  ## order by decreasing age
  same <- same[order(-N)] 
  
  names(same)[1] <- "Player"
  names(same)[2] <- "Tournament"
  names(same)[3] <- "N"
  
  #select first 20
  same <- same[1:20,]
  
  print(same)
  
}

SameSurfaceRound <- function(stage) {
  db <- removeTeamEvents(db)
  
  dbm <- db
  
  dbm <- dbm[!dbm$score=="ABN" & !dbm$score=="(ABN)" & !str_detect(dbm$score, "(WEA)")]
  
  ## get round matches
  if(stage !='W' & stage !='0')
    dbm <- dbm[round == stage]
  
  if(stage =='W')
    dbm <- dbm[round == 'F']
  
  wins <- dbm[,c('winner_name','tourney_id', 'tourney_name', 'surface')]
  
  if(stage !='W')
    losses <- dbm[,c('loser_name','tourney_id', 'tourney_name', 'surface')]
  
  #extract id from tourney_id
  wins$tourney_id <- sub("^[^-]*", "", wins$tourney_id)
  losses$tourney_id <- sub("^[^-]*", "", losses$tourney_id)
  
  names(wins)[1] <- "name"
  
  if(stage !='W')
    names(losses)[1] <- "name"
  
  ## merge the tables by "name"
  if(stage !='W')
    res <- rbind(wins, losses, by = c("name"), fill=TRUE)
  
  if(stage =='W')
    res <- wins
  
  #in the same surface
  same <- res[, .N, by = list(res$name, res$surface)]
  
  names(same)[1] <- "Player"
  names(same)[2] <- "Surface"
  
  same <- same[,c('Player','Surface', 'N')]
  
  ## order by decreasing number
  same <- same[order(-same$N),] 
  
  #select first 20
  same <- same[1:20,]
  
  print(same)
  
}

removeTeamEvents <- function(db) {
  ind_Dusseldorf <- grep("^World Team Cup", db$tourney_name)
  if (length(ind_Dusseldorf)>0)
    db <- db[-ind_Dusseldorf, ]
  
  ind_davis <- grep("^Davis", db$tourney_name)
  if (length(ind_davis)>0)
    db <- db[-ind_davis, ]
  
  
  ind_Laver <- grep("^Laver Cup", db$tourney_name)
  if (length(ind_Laver)>0)
    db <- db[-ind_Laver, ]
  
  ind_Nations_Cup <- grep("^Nations Cup", db$tourney_name)
  if (length(ind_Nations_Cup)>0)
    db <- db[-ind_Nations_Cup, ]
}

