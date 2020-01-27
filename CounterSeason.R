library(NLP)


CountRoundSeason <- function() {
  
  ## drop walkover matches (not countable)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  ##SelectRound
  #db <- db[tourney_level == 'G']
  
  #db <- db[round == 'R16']
  
  #dbm <- dbm[winner_ioc =='ITA']
    
  wins <- db[,c('winner_name','tourney_id', 'score')]
  
  wins <- wins[lengths(regmatches(wins$score, gregexpr("-", wins$score))) == '5']
  
  losses <- db[,c('loser_name','tourney_id', 'score')]
  
  losses <- losses[lengths(regmatches(losses$score, gregexpr("-", losses$score))) == '5']
  
  names(wins)[1] <- names(losses)[1] <- "name"
  
  matches <- union(wins, losses)
  
  #extract year from tourney_date
  matches$tourney_id <- stringr::str_sub(matches$tourney_id, 0 ,4)
  
  names(matches)[2] <- "year"
  
  #matches <- matches[year == '2020']
  
  print(matches)
  
  season <- matches[, .N, by = list(matches$name, matches$year)]
  
  print(season)
  
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
  mm <- mm[1:40,]
  
  print(mm)
  
}