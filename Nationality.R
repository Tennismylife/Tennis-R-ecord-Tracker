SameTournamentNation <- function() {
  
  db <- removeTeamEvents(db)
  
  #db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  #extract id from tourney_id
  #db$tourid <- sub("^[^-]*", "", db$tourney_id)
  
  #db <- db[tourid == -580 | tourid == -581]
  
  nat <- 'ITA'
  
  db <- db[db$winner_ioc == nat | db$loser_ioc == nat]
  
  #db <- db[tourney_level == 'M']
  
  #db <- db[round == 'QF']
  
  #tournaments won
  dbm <- db[winner_ioc == nat]
  wins <- unique(dbm[,c('tourney_id','winner_ioc', 'winner_name')])
  wins <- dplyr::distinct(wins)
  
  #tournaments lost
  dbm <- db[loser_ioc == nat]
  losses <- unique(dbm[,c('tourney_id','loser_ioc', 'loser_name')])
  losses <- dplyr::distinct(losses)
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "tourney_id"
  names(wins)[2] <- names(losses)[2] <- "nation"
  names(wins)[3] <- names(losses)[3] <- "player"
  
  ## merge the tables by "name"
  res <- rbind(wins, losses, use.names=FALSE)
  res <- dplyr::distinct(res)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  print(res)
  
  same <- res[, .N, by = list(res$tourney_id, res$nation)]
  
  names(same)[1] <- "tourney_id"
  names(same)[2] <- "Nation"
  names(same)[3] <- "N"
  
  print(same)
  
  officialName <- unique(db[,c('tourney_id', 'tourney_name')])
  
  same <- join(officialName, same, by="tourney_id")
  
  ## order by decreasing age
  #same <- same[order(-N)]
  
  same <- same[N > 1]
  
  # #extract year from tourney_date
  same$year <- stringr::str_sub(same$tourney_id, 0 ,4)
  
  same <- same[,c("tourney_name", "year", "N", "Nation")]
  
  ## order by decreasing
  setorder(same, -N, na.last=FALSE)
  
  print(same)
  
}


#################################################################################### WINNER IN A TOURNAMENT BY NATION #######################################
WinnerInATourByNation <- function() {
  db <- db[winner_ioc == 'ITA']
  
  db <- db[grepl("-580", db$tourney_id)]
  
  
  db <- unique(db[, c('winner_name')])
  
}


###################################################################################### TITLE BY NATION ######################################################


titlesByNation <- function() {
  db <- db[round == 'F']
  
  db <-
    db[score != 'ABN' &
         score != '(ABN)' & !str_detect(db$score, "(WEA)")]
  
  #db <- db[winner_ioc == 'ARG']
  
  db <- removeTeamEvents(db)
  
  res <- db[, .N, by = list(db$winner_ioc)]
  
  setorder(res, -N)
  
}


PlayeFromANation <- function(){
  
  db <- db[grepl("-560", db$tourney_id)]
  
  # #extract year from tourney_date
  db$year <- stringr::str_sub(db$tourney_id, 0 ,4)
  
  
}


PercentageAgainstANationbyPlayer <- function(){
  
  stat <- count(db, c("winner_name", "loser_ioc"))
  
  stat2 <- count(db, c("loser_name", "winner_ioc"))
  
  names(stat)[1] <- names(stat2)[1] <- "name"
  names(stat)[2] <- names(stat2)[2] <- "flag"
  
  stat <- merge(stat, stat2, by = c("name", "flag"), all = TRUE)
  
  stat[is.na(stat)] <- 0
  
  names(stat)[3] <- "wins"
  names(stat)[4] <- "losses"
  
  print(stat)
  
  stat <- setDT(stat)
  
  stat <- stat[wins > 15]
  
  stat <- stat[, percentage:=wins/(wins +losses)*100]
  
  ## order by decreasing
  setorder(stat, -percentage, na.last=FALSE)
  
  stat <- stat[1:100,]
}