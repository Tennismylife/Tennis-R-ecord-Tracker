###################################################################### LEAST GAME TO WIN A SLAM ##################################################################

LeastMinuteToRound <- function() {
  res <- db[round =='F' & tourney_level=='G' & winner_name == 'Novak Djokovic']
  res <- res[,c("winner_name", "tourney_id", "tourney_name", "tourney_date", "winner_name")]
  
  dbm <- db[tourney_level=='G']
  wins <- match_df(dbm, res)

  #calculate sum by edition
  time <- aggregate(wins$minutes, by=list(tourney_id=wins$tourney_id, winner_name=wins$winner_name), FUN=sum)
  
  names(time)[3] <- "minutes"
  
  res <- db[round =='F' & tourney_level == 'G' & winner_name == 'Novak Djokovic']
  officialName <- unique(res[,c('tourney_id', 'tourney_name')])
  
  time <- join(officialName, time, by="tourney_id")
  
  #extract year from tourney_id
  time$tourney_id <- stringr::str_sub(time$tourney_id, 0 ,4)
  
  time <- time[,c("tourney_name", "tourney_id", "winner_name", "minutes")]
  
  time <- arrange(time, time$minutes)
  
  print(time)
  
}


LeastBreakToRound <- function() {
  res <- db[round =='F' & tourney_level=='G' & winner_name == 'Novak Djokovic']
  res <- res[,c("winner_name", "tourney_id", "tourney_name", "tourney_date", "winner_name")]
  
  dbm <- db[tourney_level=='G']
  wins <- match_df(dbm, res)
  
  wins <- wins[,c("winner_name", "tourney_id", "tourney_name", "tourney_date", "winner_name", "w_bpFaced", "w_bpSaved")]
  
  wins <- wins[, breaks:=w_bpFaced - w_bpSaved]
  
  print(wins)
  
  #calculate sum by edition
  breaks <- aggregate(wins$breaks, by=list(tourney_id=wins$tourney_id, winner_name=wins$winner_name), FUN=sum)
  
  names(breaks)[3] <- "breaks"
  
  res <- db[round =='F' & tourney_level == 'G' & winner_name == 'Novak Djokovic']
  officialName <- unique(res[,c('tourney_id', 'tourney_name')])
  
  breaks <- join(officialName, breaks, by="tourney_id")
  
  #extract year from tourney_id
  breaks$tourney_id <- stringr::str_sub(breaks$tourney_id, 0 ,4)
  
  breaks <- breaks[,c("tourney_name", "tourney_id", "winner_name", "breaks")]
  
  breaks <- arrange(breaks, breaks)
  
  print(breaks)
  
}


LeastGameToWinSlam <- function() {
  res <- db[round =='F' & tourney_level == 'G' & winner_name == 'Novak Djokovic']
  res <- res[,c("winner_name", "tourney_id", "tourney_name", "tourney_date", "winner_name")]
  
  db <- db[tourney_level == 'G']
  wins <- match_df(db, res)
  
  print(wins)
  
  #print(wins)
  #print(length(wins$score))
  
  for(i in 1:length(wins$score))
  {
    #print("Score")
    #print(wins$score[i])  
    
    wins$score[i] <- gsub('W/O', '0-0 0-0', wins$score[i])
    
    set <- strsplit(wins$score[i], " ")
    
    #print("SET")
    #print(set)
    
    for(k in 1:length(set))
    {
      score <- strsplit(set[[k]], "-")
    }
    
    
    #print("Score")
    #print(score)
    
    total <- 0
    for (j in 1:length(score)) {
      score[[j]][2] <-  sub("\\(.*", "", score[[j]][2])
      #print(score[[j]][2])
      
      score[[j]][2][is.na(score[[j]][2])] <- 0
      total<-total+as.numeric(score[[j]][2])
    }
    #print("Total")
    #print(total)
    
    wins$score[i]<- unlist(total)
  }
  
  wins$score <- as.numeric(as.character(unlist(wins$score)))
  
  #calculate sum by edition
  lostgame <- aggregate(wins$score, by=list(tourney_id=wins$tourney_id), FUN=sum, na.rm=TRUE)
  
  names(lostgame)[2] <- "games"
  
  res <- db[round =='F' & tourney_level == 'G' & winner_name == 'Novak Djokovic']
  officialName <- unique(res[,c('tourney_id', 'tourney_name', 'winner_name')])
  
  lostgame <- join(officialName, lostgame, by="tourney_id")
  
  #extract year from tourney_id
  lostgame$tourney_id <- stringr::str_sub(lostgame$tourney_id, 0 ,4)
  
  print(lostgame)
  
  lostgame <- lostgame[,c("tourney_name", "tourney_id", "winner_name", "games")]
  
  lostgame <- arrange(lostgame, lostgame$games)
  
  print(lostgame)
  
}
