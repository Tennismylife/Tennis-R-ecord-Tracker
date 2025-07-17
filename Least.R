LeastMinuteToRound <- function() {
  
  res <- db[round =='R16' & winner_name == 'Novak Djokovic']
  
  res <- res[,c("tourney_id", "winner_name")]
  
  #search all matches played to reach a specific round
  stat <- db[tourney_level == 'G' & round != 'QF' & round !='SF' & round !='F']
  
  stat <- stat[,c("tourney_id", "winner_name", "minutes")]
  
  wins <- match_df(stat, res)
  
  print(wins)
  
  ## get rid of NAs, have 0 instead
  wins[is.na(wins)] <- 0

  wins$minutes <- as.integer(wins$minutes)
  
  #calculate sum by edition
  time <- aggregate(wins$minutes, by=list(tourney_id=wins$tourney_id, winner_name=wins$winner_name), FUN=sum)
  
  names(time)[3] <- "minutes"
  
  res <- db[round =='R16']
  officialName <- unique(res[,c('tourney_id', 'tourney_name')])
  
  time <- right_join(officialName, time, by="tourney_id")
  
  #extract year from tourney_id
  time$year <- stringr::str_sub(time$tourney_id, 0 ,4)
  
  time <- time[,c("tourney_name", "year", "minutes")]
  
  time <- arrange(time, time$minutes)
  
  print(time)
  
}


LeastBreakToRound <- function() {
  
  db[is.na(db)] <- 0
  
  player <- 'Novak Djokovic'
  
  res <- db[round =='R16' & winner_name == player]
  res <- res[,c("tourney_id", "winner_name")]
  
  #search all matches played to reach a specific round
  stat <- db[tourney_level == 'G' & round !='SF' & round !='F']
  wins <- match_df(stat, res)
  wins <- wins[,c("winner_name", "tourney_id", "tourney_name", "tourney_date", "winner_name", "w_bpFaced", "w_bpSaved")]
  
  
  wins$w_bpFaced <- as.integer(wins$w_bpFaced)
  wins$w_bpSaved <- as.integer(wins$w_bpSaved)
  
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
  
  breaks <- breaks[,c("tourney_name", "year","winner_name", "breaks")]
  
  breaks <- arrange(breaks, breaks)
  
  print(breaks)
  
}


LeastGameToReachRound <- function() {
  
  res <- db[round =='R16' & tourney_level == 'G' & winner_name == 'Novak Djokovic']
  
  res <- res[,c("tourney_id", "winner_name")]
  
  #search all matches played to reach a specific round
  stat <- db[tourney_level == 'G' & round != 'QF' & round !='SF' & round !='F']
  
  stat <- stat[,c("tourney_id", "round", "tourney_name", "winner_name", "score")]
  
  wins <- match_df(stat, res)
  
  
  #change walkover with 0 games
  wins$score <- gsub('W/O', '0-0 0-0', wins$score)
  
  wins$score <- gsub('RET', '', wins$score)
  
  games <- strsplit(wins$score, split =  " ")
  games <-   sapply(games, function(x) sub("\\(.*", "", x))
  games_won <- lapply(games, function(x) sub("-.*", "", x))
  games_lost <- lapply(games, function(x) sub(".*-", "", x))
  
  wins$wongames <- sapply(games_won, function(x) sum(as.numeric(x)))
  wins$lostgames <- sapply(games_lost, function(x) sum(as.numeric(x)))
  
  print(wins)

  wins$diffgames <- wins[, wongames - lostgames]
  wins$totalgames <- wins[, wongames + lostgames]
  
  wins[is.na(wins)] <- 0
  
  #calculate sum by edition
  lostgame <- aggregate(wins$lostgames, by=list(tourney_id=wins$tourney_id, winner_name = wins$winner_name), FUN=sum)
  
  print(lostgame)
  
  names(lostgame)[1] <- "tourney_id"
  names(lostgame)[2] <- "winner_name"
  names(lostgame)[3] <- "games"
  
  
  res <- db[tourney_level == 'G']
  res <- res[!duplicated(res$tourney_id),]
  officialName <- unique(res[,c('round', 'tourney_id', 'tourney_name')])
  lostgame <- right_join(officialName, lostgame, by="tourney_id")
  
  
  names(lostgame)[4] <- "winner_name"
  
  #extract year from tourney_id
  lostgame$tourney_id <- stringr::str_sub(lostgame$tourney_id, 0 ,4)

  lostgame <- lostgame[,c("round", "tourney_name", "tourney_id", "winner_name", "games")]
  
  lostgame <- arrange(lostgame, lostgame$games)
  
  ## common name to merge with
  names(lostgame)[1] <- "Round"
  names(lostgame)[2] <- "Tournament"
  names(lostgame)[3] <- "Year"
  names(lostgame)[4] <- "Winner"
  names(lostgame)[5] <- "Games"  
  
  
  lostgame <- unique(lostgame[,c('Round','Tournament','Year', 'Winner', 'Games')])
  
  print(lostgame)
  
}


LeastSetToWintour <- function() {
  
  res <- db[tourney_level == 'G' & round =='F']
  res <- res[,c("tourney_id", "winner_name")]
  
  #search all matches played to win a tournament
  stat <- db[tourney_level == 'G']
  
  print(stat)
  stat <- stat[,c("tourney_id", "winner_name", "score", "best_of")]
  
  wins <- match_df(stat, res)
  
  #change walkover with 0 games
  wins$score <- gsub('W/O', '0-0 0-0', wins$score)
  
  print(wins)
  
  library(foreach)
  foreach(i = 1:length(wins$score)) %do%  
    {
      print(wins$score[i])
      
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
  
  db <- db[tourney_level == 'G']
  
  res <- db[round == 'F']
  res <- res[, c("tourney_id", "winner_name")]
  
  wins <- match_df(db, res)
  
  wins$score <- ifelse(grepl("3", wins$best_of), gsub('W/O', '1-0 1-0', wins$score),  wins$score)
  wins$score <- ifelse(grepl("5", wins$best_of), gsub('W/O', '1-0 1-0 1-0', wins$score),  wins$score)
  
  wins$score <- str_count(wins$score, " ")
  
  search <- wins[(wins$score == 2 & wins$best_of == 3) | (wins$score == 3 & wins$best_of == 5) | (wins$score == 4 & wins$best_of == 5)]
  
  search <- search[, c("tourney_id", "tourney_name", "winner_name", "winner_age", "score", "best_of")]

  #search the tourney with strength sets played
  library(dplyr)    
  wins <- anti_join(wins, search, by = c("tourney_id", "winner_name"))
  
  require(data.table) # v1.9.0+
  setDT(wins) # converts data which is a data.frame to data.table *by reference
  
  wins <-unique(wins[, c("tourney_id", "tourney_name", "winner_name", "winner_age")])
  
  # #extract year from tourney_date
  wins$year <- stringr::str_sub(wins$tourney_id, 0 ,4)
  
  #res <- wins[wins$winner_name == 'Jimmy Connors']
  
  res <- wins[, c("tourney_name", "year", "winner_name", "winner_age")]
  
  #res <- wins[, .N, by = winner_name]
  
  ## order by decreasing
  setorder(res, winner_age, na.last = FALSE)
  
  res <- FormatwinnerAge(res)
  
  print(res)
}