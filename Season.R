library(stringr)

source("Entries.R")


EntriesSeason <- function() {
  
  db <- removeTeamEvents(db)
  
  #tournaments won
  wins <- unique(db[,c('winner_name','tourney_name','tourney_id')])
  wins <- dplyr::distinct(wins)
  
  #tournaments lost
  losses <- unique(db[,c('loser_name','tourney_name','tourney_id')])
  losses <- dplyr::distinct(losses)
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, all = TRUE, allow.cartesian=TRUE)
  res <- dplyr::distinct(res)
  
  #extract year from tourney_date
  res$tourney_id <- stringr::str_sub(res$tourney_id, 0 ,4)
  
  names(res)[1] <- "name"
  names(res)[2] <- "year"
  
  #In a season
  #res <- res [year == '2019']
  
  season <- res[, .N, by = list(res$name, res$year)]
  
  names(season)[1] <- "Player"
  names(season)[2] <- "Season"
  names(season)[3] <- "Entries"
  
  entry <- season[order(-Entries)] 
  
  entry <- entry[1:100,]
  
  print(entry)
  
}

SameSeasonWins <- function() {
  
  #tournaments won
  wins <- db[,c('winner_name','tourney_id')]
  
  #extract year from tourney_date
  wins$tourney_id <- stringr::str_sub(wins$tourney_id, 0 ,4)

  
  names(wins)[1] <- "name"
  names(wins)[2] <- "year"
  
  sameseason <- wins[, .N, by = list(wins$name, wins$year)]
  
  names(sameseason)[1] <- "Player"
  names(sameseason)[2] <- "Season"

  print(sameseason) 
  
  sameseason <- sameseason[Season  < 1989]
  
  entry <- sameseason[order(-N)] 
  
  entry <- entry[1:300,]
  
  print(entry)
  
}



SameSeasonRound <- function(stage) {
  db <- removeTeamEvents(db)
  
  dbm <- db
  
  dbm <- dbm[!dbm$score=="ABN" & !dbm$score=="(ABN)" & !str_detect(dbm$score, "(WEA)")]
  
  ## get round matches
  if(stage !='W' & stage !='0')
    dbm <- dbm[round == stage]
  
  if(stage =='W')
    dbm <- dbm[round == 'F']
  
  wins <- dbm[,c('winner_name','tourney_id', 'tourney_name', 'round')]
  
  if(stage !='W')
    losses <- dbm[,c('loser_name','tourney_id', 'tourney_name', 'round')]
  
  names(wins)[1] <- "name"
  
  if(stage !='W')
    names(losses)[1] <- "name"
  
  ## merge the tables by "name"
  if(stage !='W')
    res <- rbind(wins, losses, by = c("name"), fill=TRUE)
  
  if(stage =='W')
    res <- wins
  
  
  #extract year from tourney_date
  res$tourney_id <- stringr::str_sub(res$tourney_id, 0 ,4)
  
  same <- res[, .N, by = list(res$name, res$tourney_id)]
  
  names(same)[1] <- "Player"
  names(same)[2] <- "Season"
  names(same)[3] <- "N"
  
  season <- same[order(-N)] 
  season <- season[1:400,]
  
  print(season)
}
