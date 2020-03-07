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


LeastGameToWintour <- function() {
  res <- db[round =='F']
  res <- res[,c("tourney_id", "tourney_name", "winner_name")]
  
  #db <- db[tourney_level == 'G']
  wins <- match_df(db, res)
  
  print(wins)
  
  library(foreach)
  foreach(i = 1:length(wins$score)) %do%  
  {
    
    #change walkover with 0 games
    wins$score[i] <- gsub('W/O', '0-0 0-0', wins$score[i])
    
    #split to catch the sets
    set <- strsplit(wins$score[i], " ")

    foreach(k = 1:length(set)) %do%
    {
      score <- strsplit(set[[k]], "-")
    }
    
    total <- 0
    
    #count lost games
    foreach(j = 1:length(score)) %do%
    {
      #sub for tiebreaks
      score[[j]][2] <-  sub("\\(.*", "", score[[j]][2])
      
      score[[j]][2][is.na(score[[j]][2])] <- 0
      
      total<-total+as.numeric(score[[j]][2])
    }
    
    #sub score with lost games
    wins$score[i]<- unlist(total)
  }
  
  wins$score <- as.numeric(as.character(unlist(wins$score)))
  
  #calculate sum by edition
  lostgame <- aggregate(wins$score, by=list(tourney_id=wins$tourney_id), FUN=sum, na.rm=TRUE)
  
  names(lostgame)[2] <- "games"
  
  res <- db[round =='F']
  officialName <- unique(res[,c('tourney_id', 'tourney_name', 'winner_name')])
  
  lostgame <- join(officialName, lostgame, by="tourney_id")
  
  #extract year from tourney_id
  lostgame$tourney_id <- stringr::str_sub(lostgame$tourney_id, 0 ,4)

  lostgame <- lostgame[,c("tourney_name", "tourney_id", "winner_name", "games")]
  
  lostgame <- arrange(lostgame, lostgame$games)
  
  print(lostgame)
  
}


NoDroppedSetTitle <- function(){
  db <-  removeTeamEvents(db)
  
  #db <- db[tourney_level == 'G']
  
  res <- db[round == 'F']
  res <- res[, c("tourney_id", "winner_name")]
  
  wins <- match_df(db, res)
  
  wins$score <- ifelse(grepl("3", wins$best_of), gsub('W/O', '1-0 1-0', wins$score),  wins$score)
  wins$score <- ifelse(grepl("5", wins$best_of), gsub('W/O', '1-0 1-0 1-0', wins$score),  wins$score)
  
  wins$score <- str_count(wins$score, " ")
  
  search <- wins[(wins$score == 2 & wins$best_of == 3) | (wins$score == 3 & wins$best_of == 5) | (wins$score == 4 & wins$best_of == 5)]
  
  search <- search[, c("tourney_id", "tourney_name", "winner_name", "score", "best_of")]
  
  print(search)

  
  #search the tourney with strength sets played
  library(dplyr)    
  wins <- anti_join(wins, search, by = c("tourney_id", "winner_name"))
  
  require(data.table) # v1.9.0+
  setDT(wins) # converts data which is a data.frame to data.table *by reference
  
  wins <-unique(wins[, c("tourney_id", "tourney_name", "winner_name")])
  
  #wins <- wins[wins$winner_name == 'Rafael Nadal']
  
  res <- wins[, .N, by = winner_name]
  
  ## order by decreasing
  setorder(res,-N, na.last = FALSE)
  
  print(res)
}

