#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
#List with bagel for a player
ListMatchesBagel <- function(){
  
  player <- 'Novak Djokovic'
  
  db$winnerbagel <- str_count(db$score, "6-0")
  
  db$loserbagel <- str_count(db$score, "0-6")
  
  db <- db[(winner_name == player & grepl("6-0", db$score)) | (loser_name == player & grepl("0-6", db$score))]
  
  stat <- db[, bagel:=winnerbagel + loserbagel]
  
  # #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- stat[,c("tourney_name", "year", "round", "winner_name", "loser_name", "score", "bagel")]
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
#most scored bagels
MostBagel <- function() {
  
  score <- '6-0'
  
  reverseScore <- '0-6'
  
  #db <- db[tourney_level == 'G']
  
  db$winnerbagel <- str_count(db$score, score)
  
  db$loserbagel <- str_count(db$score, reverseScore)
  
  db <-
    db[, c("winner_name", "loser_name", "winnerbagel", "loserbagel")]
  
  wins <- db[, c("winner_name", "winnerbagel")]
  losses <- db[, c("loser_name", "loserbagel")]
  
  names(wins)[1] <- names(losses)[1] <- "Player"
  names(wins)[2] <- names(losses)[2] <- "Bagel"
  
  wins[is.na(wins)] <- 0
  losses[is.na(losses)] <- 0
  
  all <- rbind(wins, losses)
  
  bagel1 <-
    aggregate(all$Bagel,
              by = list(Player = all$Player),
              FUN = sum)
  
  ## order by decreasing
  bagel1 <- setorder(bagel1,-x, na.last = FALSE)
  
  print(bagel1)
}