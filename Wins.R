source("Reader.R")

################################################################################## WINS ################################################################################



WinsCategory <- function(category) {
  
  ## only select tournaments in the previously defined pool
  dbm <- db[tourney_level == category]
  
  ## drop walkover matches (not countable)
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  ## count occurrences of won matches
  res <- dbm[,.N, by=winner_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res <- res[1:20,]
  
  print(res)
}


WinsTour <- function(tournament, tournament2, tournament3) {
  
  ## only select tournaments in the previously defined pool
  dbm <- db[tourney_name == tournament | tourney_name == tournament2 | tourney_name == tournament3]
  
  ## drop walkover matches (not countable)
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  ## count occurrences of won matches
  res <- dbm[,.N, by=winner_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res <- res[1:20,]
  
  print(res)
}

winsSurface <- function(court) {
  ## only select tournaments in the previously defined pool
  dbm <- db[surface == court]
  
  ## drop walkover matches (not countable)
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  ## count occurrences of won matches
  res <- dbm[,.N, by=winner_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res <- res[1:20,]
  
  print(res)
}

WinsOverall <- function() {
  ## drop walkover matches (not countable)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  ## count occurrences of won matches
  res <- db[,.N, by=winner_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res <- res[1:20,]
  
  print(res)
  
}

LossesOverall <- function() {
  dbm <- db
  ## drop walkover matches (not countable)
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  ## count occurrences of lost matches
  out <- dbm[,.N, by=loser_name]
  
  ## order by decreasing
  setorder(out, -N, na.last=FALSE)
  
  res <- res[1:100,]
  
  print(out)
}