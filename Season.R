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



WinsSeason <- function() {
  dbm <- db
  ## drop walkover matches (not countable)
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  wins <- dbm[,c('winner_name','tourney_id', 'tourney_name')]
  
  #extract year from tourney_date
  wins$tourney_id <- stringr::str_sub(wins$tourney_id, 0 ,4)
  
  names(wins)[2] <- "year"
  
  season <- wins[, .N, by = list(wins$winner_name, wins$year)]
  
  names(season)[1] <- "Player"
  names(season)[2] <- "Season"
  names(season)[3] <- "Wins"
  
  season <- season[order(-Wins)] 
  season <- season[1:20,]
  
  print(season)
}



titlesSeason <- function() {
  dbm <- db
  
  ## drop walkover matches (not countable)
  dbm <- dbm[!dbm$score=="W/O",]
  
  ## drop walkover matches (not countable)
  dbm <- dbm[round == 'F']
  
  wins <- dbm[,c('winner_name','tourney_id')]
  
  #extract year from tourney_date
  wins$tourney_id <- stringr::str_sub(wins$tourney_id, 0 ,4)
  
  names(wins)[2] <- "year"
  
  season <- wins[, .N, by = list(wins$winner_name, wins$year)]
  
  ## order by decreasing age
  season <- season[order(-N)] 
  
  #select 1st 20
  season <- season[1:20,]
  
  print(season)
}
