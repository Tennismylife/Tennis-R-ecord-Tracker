library(NLP)


CountRoundSeason <- function() {
  
  #db <- removeTeamEvents(db)
  
  ## drop walkover matches (not countable)
  #db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  ##SelectRound
  #db <- db[tourney_level == 'M']
  
  #db <- db[winner_ioc =='ITA']
  
  #db <- db[winner_age < 21.547]
  
  db <- db[round == 'F']
  
  wins <- db[,c('winner_name','tourney_id')]
  
  #extract year from tourney_date
  wins$tourney_id <- stringr::str_sub(wins$tourney_id, 0 ,4)
  
  names(wins)[2] <- "year"
  
  season <- wins[, .N, by = list(wins$winner_name, wins$year)]
  
  names(season)[1] <- "player"
  names(season)[2] <- "year"
  
  #select where N > 4
  season <- season[which(N >  0)]
  
  ## order by decreasing
  season <- season[order(-N)]
  
  season <- season[order(year)]
  
  out <- aggregate(season$year ~ season$player, season, c)
  
  names(out)[1] <- "player"
  names(out)[2] <- "seasons"
  
  nn <- sapply(strsplit(as.character(out$seasons), ", "), length)
  
  mm <- as.data.frame(cbind(out, n=nn))
  
  mm <- mm[order(mm$n, decreasing=TRUE), ]
  
  #mm$seasons <- str_replace_all(mm$seasons, "c", "")
  
  mm$seasons <- gsub("c(", "", mm$seasons, fixed = TRUE)
  
  mm$seasons <- gsub("\"", "", mm$seasons, fixed = TRUE)
  
  mm$seasons <- gsub(")", "", mm$seasons, fixed = TRUE)
  
  #select 1st 20
  #mm <- mm[1:100,]
  
  print(mm)
  
}