LeastMinuteToRound <- function() {
  
  res <- db[round =='F' & winner_name == 'Novak Djokovic']
  
  res <- res[,c("tourney_id", "winner_name")]
  
  #search all matches played to reach a specific round
  stat <- db[tourney_level == 'G']
  
  stat <- stat[,c("tourney_id", "winner_name", "minutes")]
  
  wins <- match_df(stat, res)
  
  print(wins)
  
  library("imputeTS")
  wins <- na.replace(wins, 0)

  #calculate sum by edition
  time <- aggregate(wins$minutes, by=list(tourney_id=wins$tourney_id, winner_name=wins$winner_name), FUN=sum)
  
  names(time)[3] <- "minutes"
  
  res <- db[round =='R16']
  officialName <- unique(res[,c('tourney_id', 'tourney_name')])
  
  time <- right_join(officialName, time, by="tourney_id")
  
  #extract year from tourney_id
  time$tourney_id <- stringr::str_sub(time$tourney_id, 0 ,4)
  
  time <- time[,c("tourney_name", "tourney_id", "minutes")]
  
  time <- arrange(time, time$minutes)
  
  print(time)
  
}


LeastBreakToRound <- function() {
  
  db[is.na(db)] <- 0
  
  res <- db[tourney_level == 'M' & round =='F']
  res <- res[,c("tourney_id", "winner_name")]
  
  #search all matches played to reach a specific round
  stat <- db[tourney_level == 'M']
  
  #search all matches played to reach a round
  wins <- match_df(stat, res)
  
  wins <- wins[,c("winner_name", "tourney_id", "tourney_name", "tourney_date", "winner_name", "w_bpFaced", "w_bpSaved")]
  
  wins <- wins[, breaks:=w_bpFaced - w_bpSaved]
  
  print(wins)
  
  #calculate sum by edition
  breaks <- aggregate(wins$breaks, by=list(tourney_id=wins$tourney_id, winner_name=wins$winner_name), FUN=sum)
  
  names(breaks)[3] <- "breaks"
  
  res <- db[round =='R16']
  officialName <- unique(res[,c('tourney_id', 'tourney_name')])
  
  breaks <- right_join(officialName, breaks, by="tourney_id")
  
  #extract year from tourney_id
  breaks$year <- stringr::str_sub(breaks$tourney_id, 0 ,4)
  
  breaks <- breaks[,c("tourney_name", "year", "breaks")]
  
  breaks <- arrange(breaks, breaks)
  
  print(breaks)
  
}


LeastGameToReachRound <- function() {
  
  res <- db[round =='F']
  res <- res[,c("tourney_id", "winner_name")]
  
  #search all matches played to reach a specific round
  stat <- db
  stat <- stat[,c("tourney_id", "winner_name", "score")]

  wins <- match_df(stat, res)
  
  #change walkover with 0 games
  wins$score <- gsub('W/O', '0-0 0-0', wins$score)
  
  library(foreach)
  foreach(i = 1:length(wins$score)) %do%  
  {
    print(wins$tourney_id[i])

    
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
  
  print(wins)
  
  #calculate sum by edition
  lostgame <- wins[, lapply(.SD,sum), by=list(tourney_id, winner_name)]
  
  names(lostgame)[1] <- "tourney_id"
  names(lostgame)[2] <- "winner_name"
  names(lostgame)[3] <- "games"
  
  
  res <- db[round =='F']
  officialName <- unique(res[,c('tourney_id', 'tourney_name', 'winner_name')])
  
  lostgame <- right_join(officialName, lostgame, by="tourney_id")
  
  
  names(lostgame)[4] <- "winner_name"
  
  #extract year from tourney_id
  lostgame$tourney_id <- stringr::str_sub(lostgame$tourney_id, 0 ,4)

  lostgame <- lostgame[,c("tourney_name", "tourney_id", "winner_name", "games")]
  
  lostgame <- arrange(lostgame, lostgame$games)
  
  ## common name to merge with
  names(lostgame)[1] <- "Tournament"
  names(lostgame)[2] <- "Year"
  names(lostgame)[3] <- "Winner"
  names(lostgame)[4] <- "Games"  
  
  
  lostgame <- unique(lostgame[,c('Tournament','Year', 'Winner', 'Games')])
  
  print(lostgame)
  
}


LeastSetToWintour <- function() {
  
  res <- db[tourney_level == 'M' & round =='F']
  res <- res[,c("tourney_id", "winner_name")]
  
  #search all matches played to win a tournament
  stat <- db[tourney_level == 'M']
  
  print(stat)
  stat <- stat[,c("tourney_id", "winner_name", "score", "best_of")]
  
  wins <- match_df(stat, res)
  
  #change walkover with 0 games
  wins$score <- gsub('W/O', '0-0 0-0', wins$score)
  
  print(wins)
  
  library(foreach)
  foreach(i = 1:length(wins$score)) %do%  
    {
      print(wins$tourney_id[i])
      
      #change walkover with 0 games
      wins$score[i] <- gsub(" RET", "", wins$score[i])
      
      #split to catch the sets
      set <- strsplit(wins$score[i], " ")
      
      foreach(k = 1:length(set)) %do%
        {
          score <- strsplit(set[[k]], "-")
        }
      #print(wins$score[i])
      #print(wins$best_of[i])
      #print(length (score))

      total <- 0
      
      if(wins$best_of[i] == '5' & length (score) == '1')
        set <- 0
      
      if(wins$best_of[i] == '5' & length (score) == '2')
        set <- 0
      
      if(wins$best_of[i] == '5' & length (score) == '3')
        set <- 0

      if(wins$best_of[i] == '5' & length (score) == '4')
        set <- 1

      if(wins$best_of[i] == '5' & length (score) == '5')
        set <- 2

      if(wins$best_of[i] == '3' & length (score) == '2')
        set <- 0

      if(wins$best_of[i] == '3' & length (score) == '3')
        set <- 1
      
      if(wins$best_of[i] == '3' & length (score) == '1')
        set <- 0
      
      #sub score with lost games
      wins$score[i]<- set
    }
  
  print(wins)
  
  wins$score <- as.numeric(as.character(unlist(wins$score)))
  
  #calculate sum by edition
  lostgame <- aggregate(wins$score, by=list(tourney_id=wins$tourney_id, winner_name =wins$winner_name), FUN=sum, na.rm=TRUE)
  
  res <- db[round =='F']
  officialName <- unique(res[,c('tourney_id', 'tourney_name', 'winner_name')])
  
  lostgame <- right_join(officialName, lostgame, by="tourney_id")
  
  print(lostgame)
  
  names(lostgame)[4] <- "winner_name"
  
  #extract year from tourney_id
  lostgame$tourney_id <- stringr::str_sub(lostgame$tourney_id, 0 ,4)
  
  lostgame <- lostgame[,c("tourney_name", "tourney_id", "winner_name", "x")]
  
  lostgame <- arrange(lostgame, lostgame$x)
  
  ## common name to merge with
  names(lostgame)[1] <- "Tournament"
  names(lostgame)[2] <- "Year"
  names(lostgame)[3] <- "Winner"
  names(lostgame)[4] <- "games"  
  
  print(lostgame)
}


NoDroppedSetTitle <- function(){
  
  db <-  removeTeamEvents(db)
  
  res <- db[round == 'F']
  res <- res[, c("tourney_id", "winner_name")]
  
  wins <- match_df(db, res)
  
  wins$score <- ifelse(grepl("3", wins$best_of), gsub('W/O', '1-0 1-0', wins$score),  wins$score)
  wins$score <- ifelse(grepl("5", wins$best_of), gsub('W/O', '1-0 1-0 1-0', wins$score),  wins$score)
  
  wins$score <- str_count(wins$score, " ")
  
  search <- wins[(wins$score == 2 & wins$best_of == 3) | (wins$score == 3 & wins$best_of == 5) | (wins$score == 4 & wins$best_of == 5)]
  
  search <- search[, c("tourney_id", "tourney_name", "winner_name", "score", "best_of")]

  #search the tourney with strength sets played
  library(dplyr)    
  wins <- anti_join(wins, search, by = c("tourney_id", "winner_name"))
  
  require(data.table) # v1.9.0+
  setDT(wins) # converts data which is a data.frame to data.table *by reference
  
  wins <-unique(wins[, c("tourney_id", "tourney_name", "winner_name")])
  
  # #extract year from tourney_date
  wins$year <- stringr::str_sub(wins$tourney_id, 0 ,4)
  
  res <- wins[wins$winner_name == 'Jimmy Connors']
  
  res <-res[, c("tourney_name", "year", "winner_name")]
  
  #res <- wins[, .N, by = winner_name]
  
  ## order by decreasing
  #ssetorder(res,-N, na.last = FALSE)
  
  print(res)
}