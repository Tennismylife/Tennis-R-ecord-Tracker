library(NLP)


CountRoundSeason <- function() {
  dbm <- db
  
  ## drop walkover matches (not countable)
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  ##SelectRound
  dbm <- dbm[tourney_level == 'G']
  
  dbm <- dbm[round == 'F']
  
  #dbm <- dbm[winner_ioc =='ITA']
  
  wins <- dbm[,c('winner_name','tourney_id')]
  
  #extract year from tourney_date
  wins$tourney_id <- stringr::str_sub(wins$tourney_id, 0 ,4)
  
  names(wins)[2] <- "year"
  
  season <- wins[, .N, by = list(wins$winner_name, wins$year)]
  
  names(season)[1] <- "player"
  names(season)[2] <- "year"
  
  #select where N > 4
  #season <- season[which(N >  1)]
  
  ## order by decreasing
  season <- season[order(-N)]
  
  season <- season[order(year)]
  
  out <- aggregate(season$year ~ season$player, season, c)
  
  names(out)[1] <- "player"
  names(out)[2] <- "seasons"
  
  nn <- sapply(strsplit(as.character(out$seasons), ", "), length)
  
  mm <- as.data.frame(cbind(out, n=nn))
  
  mm <- mm[order(mm$n, decreasing=TRUE), ]
  
  mm$seasons <-  as.character(mm$seasons)
  
  #select 1st 20
  mm <- mm[1:20,]
  
  print(mm)
  
}