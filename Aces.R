#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
MostAces <- function() {
  
  db <-
    db[!db$score == "W/O" &
          !db$score == "DEF" &
          !db$score == "(ABN)" & !db$score == "ABN"]
  
  
  winner_aces <- db[, c("winner_name", "w_ace")]
  
  loser_aces <- db[, c("loser_name", "l_ace")]
  
  names(winner_aces)[1] <- names(loser_aces)[1] <- "name"
  names(winner_aces)[2] <- names(loser_aces)[2] <- "aces"
  
  res <- rbind(winner_aces,
               loser_aces,
               by = c("name"),
               fill = TRUE)
  
  res[is.na(res)] <- 0
  
  totalAces <-
    aggregate(as.numeric(res$aces),
              by = list(name = res$name),
              FUN = sum)
  
  setorder(totalAces, -x)
  
  print(totalAces)
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
MostAcesinASeasonByPlayer <- function() {
  
  player <- 'Roger Federer'
  
  #extract year from tourney_date
  db$year <- stringr::str_sub(db$tourney_id, 0 , 4)
  
  db1 <- db[winner_name == player]
  winner_aces <- db1[, c("winner_name", "w_ace", "year")]
  
  db2 <- db[loser_name == player]
  loser_aces <- db2[, c("loser_name", "l_ace", "year")]
  
  names(winner_aces)[1] <- names(loser_aces)[1] <- "name"
  names(winner_aces)[2] <- names(loser_aces)[2] <- "aces"
  
  res <- rbind(winner_aces, loser_aces)
  
  res[is.na(res)] <- 0
  
  totalAces <-
    aggregate(
      as.numeric(res$aces),
      by = list(name = res$name, season = res$year),
      FUN = sum
    )
  
  names(totalAces)[3] <- "aces"
  
  setorder(totalAces, -aces)
  
  print(totalAces)
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
MostAcesinASlamByAPlayer <- function() {
  
  player <- 'Novak Djokovic'
  
  res <- db[tourney_level=='G' & (winner_name == player | loser_name == player)]
  
  res <- res[,c("winner_name", "loser_name", "tourney_id", "tourney_name", "tourney_date", "w_ace", 'l_ace')]
  
  res[is.na(res)] <- 0
  
  res$totalaces <- ifelse(res$loser_name == player, res$l_ace, res$w_ace)
  
  res$player <- ifelse(res$loser_name == player, res$loser_name, res$winner_name)
  
  print(res$player)
  
  #calculate sum by edition
  aces <- aggregate(res$totalaces, by=list(tourney_id=res$tourney_id, player=res$player), FUN=sum)
  
  names(aces)[3] <- "aces"
  
  res <- db[round =='R16' & tourney_level == 'G']
  officialName <- unique(res[,c('tourney_id', 'tourney_name')])
  
  aces <- right_join(officialName, aces, by="tourney_id")
  
  #extract year from tourney_id
  aces$year <- stringr::str_sub(aces$tourney_id, 0 ,4)
  
  aces <- aces[,c("player", "tourney_name", "year", "aces")]
  
  setorder(aces, -aces, na.last=FALSE)
  
  print(aces)
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
RatioAcesinASlamByPlayer <- function() {
  
  player <- 'Roger Federer'
  
  res <- db[tourney_level=='G' & (winner_name == player | loser_name == player)]
  
  res <- res[,c("winner_name", "loser_name", "tourney_id", "tourney_name", "tourney_date", "w_ace", 'l_ace', 'w_svpt', 'l_svpt')]
  
  res[is.na(res)] <- 0
  
  res$totalaces <- ifelse(res$loser_name == player, res$l_ace, res$w_ace)
  
  res$totalservicepoints <- ifelse(res$loser_name == player, res$l_svpt, res$w_svpt)
  
  #print(res)
  
  res$player <- ifelse(res$loser_name == player, res$loser_name, res$winner_name)
  
  #calculate sum by edition
  aces <- aggregate(res$totalaces, by=list(tourney_id=res$tourney_id, player=res$player), FUN=sum)
  
  #print(aces)
  
  #calculate sum by edition
  servicepoints <- aggregate(res$totalservicepoints, by=list(tourney_id=res$tourney_id, player=res$player), FUN=sum)
  
  #print(servicepoints)
  
  stat <- res <- merge(aces, servicepoints, by = c("tourney_id", "player"), all=TRUE)  
  
  #print(stat)
  
  names(stat)[3] <- "aces"
  names(stat)[4] <- "serve_pts"
  
  res <- db[round =='R16' & tourney_level == 'G']
  officialName <- unique(res[,c('tourney_id', 'tourney_name')])
  
  stat <- right_join(officialName, stat, by="tourney_id")
  
  #print(stat)
  
  #extract year from tourney_id
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- stat[,c("tourney_name", "year", "player", "aces", "serve_pts")]
  
  print(stat)
  
  ## get rid of NAs, have 0 instead
  stat[is.na(stat)] <- 0
  
  stat$ratio = stat$aces / stat$serve_pts
  
  stat$ratio  <- substr(stat$ratio, 0, 5)
  stat$ratio <- suppressWarnings(as.numeric(str_replace_all(stat$ratio,pattern=',',replacement='.')))
  
  setorder(stat, -ratio, na.last=FALSE)
  
  print(stat)
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
MostAcesBySlammer <- function() {
  
  res <- db[round =='F']
  
  res <- res[,c("tourney_id", "winner_name")]
  
  stat <- db[tourney_level == 'G']
  
  stat <- stat[,c("tourney_id", "winner_name", "w_ace")]
  
  wins <- match_df(stat, res)
  
  print(wins)
  
  library("imputeTS")
  wins <- na.replace(wins, 0)
  
  #calculate sum by edition
  aces <- aggregate(wins$w_ace, by=list(tourney_id=wins$tourney_id, winner_name=wins$winner_name), FUN=sum)
  
  names(aces)[3] <- "total_aces"
  
  res <- db[round =='R16']
  officialName <- unique(res[,c('tourney_id', 'tourney_name')])
  
  aces <- right_join(officialName, aces, by="tourney_id")
  
  #extract year from tourney_id
  aces$tourney_id <- stringr::str_sub(aces$tourney_id, 0 ,4)
  
  print(aces)
  
  aces <- aces[,c("tourney_name", "tourney_id", "winner_name", "total_aces")]
  
  aces <- arrange(aces, -aces$total_aces)
  
  print(aces)
  
}


