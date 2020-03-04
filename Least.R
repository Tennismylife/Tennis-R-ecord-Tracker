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


NoDroppedSetTitle <- function() {
  db <-  removeTeamEvents(db)
  
  #db <- db[tourney_level == 'G']
  
  res <- db[round == 'F']
  res <- res[, c("tourney_id", "tourney_name", "winner_name")]
  
  wins <- match_df(db, res)
  
  wins$score <- gsub('W/O', '1-0 1-0 1-0', wins$score)
  
  
library("foreach")  
 foreach(i = 1:length(wins$score)) %do%
    {
      print(wins[i]$tourney_id)
      
      #split to catch the sets
      set <- strsplit(wins$score[i], " ")

      foreach(k = 1:length(set)) %dopar%
        {
          score <- strsplit(set[[k]], "-")
        }
      
      totalLostSet <- 0
      

      registerDoParallel(cl, cores=cores)
      foreach(j = 1:length(score)) %dopar%
        {
          #sub for tiebreaks
          score[[j]][2] <-  sub("\\(.*", "", score[[j]][2])
          score[[j]][2][is.na(score[[j]][2])] <- 0
          
          games <- unlist(score[j][1])
          
          if(games[2] > games[1])
            totalLostSet <- totalLostSet + 1
        }

      #sub score with lost games
     wins$score[i] <- as.numeric(totalLostSet)
    }
  
  print(wins)
  
  wins$score <- as.numeric(wins$score)
  
  #calculate sum by edition
  setLost <- aggregate(wins$score, by =list(tourney_id = wins$tourney_id, winner_name = wins$winner_name), FUN = sum, na.rm = TRUE)
  
  require(data.table) # v1.9.0+
  setDT(setLost) # converts data which is a data.frame to data.table *by reference*
  
  names(setLost)[3] <- "Number"
  
  setLost <- setLost[Number == 0]
  
  res <- setLost[,.N, by=winner_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  print(res)
}
