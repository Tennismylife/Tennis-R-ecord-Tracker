PercentageOverall <- function() {
  dbm <- db
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  
  #extract tourney from tourney_id
  dbm$tourney_id <- stringr::str_sub(dbm$tourney_id, 5 ,3)
  
  ## wins
  wins <- dbm[,.N, by=winner_name]
  ## losses
  losses <- dbm[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 20]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  res <- res[1:100,]
  print(res)
}


PercentageSurface <- function(court) {
  dbm <- db
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  dbm <- dbm[surface == court]
  
  ## wins
  wins <- dbm[,.N, by=winner_name]
  ## losses
  losses <- dbm[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 20]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  res <- res[1:100,]
  print(res)
}


PercentageCategory <- function(category) {

  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  db <- db[tourney_level == category]
  
  ## wins
  wins <- db[,.N, by=winner_name]
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 20]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  res <- res[1:100,]
  print(res)
}

PercentageTour <- function(tour) {

  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  db <- db[tourney_name == tour]
  
  ## wins
  wins <- db[,.N, by=winner_name]
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 20]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  res <- res[1:20,]
  print(res)
}



PercentageSameSeason <- function() {
  
  dbm <- db
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  #extract year from tourney_date
  dbm$tourney_id <- stringr::str_sub(dbm$tourney_id, 0 ,4)
  
  dbm <- dbm[tourney_id == '2019']
  
  ## wins
  #dbm1 <- dbm[winner_name == 'Rafael Nadal']
  wins <- dbm[,.N, by=list(winner_name, tourney_id)]
  
  ## losses
  #dbm1 <- dbm[loser_name == 'Rafael Nadal']
  losses <- dbm[,.N, by=list(loser_name, tourney_id)]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[3] <- "wins"
  names(losses)[3] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name", "tourney_id"), allow.cartesian=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  #res <- res[played > 2]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  res <- res[1:100,]
  print(res)
  
}


PercentageSameSurface <- function() {
  
  dbm <- db
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  ## wins
  wins <- dbm[,.N, by=list(winner_name, surface)]
  
  ## losses
  losses <- dbm[,.N, by=list(loser_name, surface)]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[3] <- "wins"
  names(losses)[3] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name", "surface"), allow.cartesian=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 50]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  res <- res[1:100,]
  print(res)
  
}

PercentageSameTour <- function() {
  db <- removeTeamEvents(db)
  dbm <- db
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  dbm <- dbm[tourney_level =='G']
  
  #extract year from tourney_date
  dbm$tourney_id <- stringr::str_sub(dbm$tourney_id, 5 ,9)
  
  ## wins
  wins <- dbm[,.N, by=list(winner_name, tourney_id)]
  
  ## losses
  losses <- dbm[,.N, by=list(loser_name, tourney_id)]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[3] <- "wins"
  names(losses)[3] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name", "tourney_id"))
  
  officialName <- unique(dbm[,c('tourney_id', 'tourney_name')])
  
  res <- join(officialName, res, by="tourney_id")
  
  print(res)  
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 20]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))

  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res <- res[,c("name", "tourney_name", "wins", "losses", "played", "percentage")]
  
  res <- res[1:100,]
  print(res)
  
}