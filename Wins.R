library(stringr)

################################################################################## WINS ################################################################################

WinsCategory <- function(category) {
  
  ## only select tournaments in the previously defined pool
  db <- db[tourney_level == category]
  
  ## drop walkover matches (not countable)
  require(stringr)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]
  
  ## count occurrences of won matches
  res <- db[,.N, by=winner_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res
}


WinsTour <- function(id) {
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  db <- db[tourney_id == id]
  
  ## drop walkover matches (not countable)
  require(stringr)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]
  
  ## count occurrences of won matches
  res <- db[,.N, by=winner_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res <- res[1:20,]
  
  res
}

winsSurface <- function(court) {
  
  ## only select tournaments in the previously defined pool
  db <- db[surface == court]
  
  ## drop walkover matches (not countable)
  require(stringr)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]
  
  ## count occurrences of won matches
  res <- db[,.N, by=winner_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  #res <- res[1:2000,]
  
  print(res)
}

WinsOverall <- function() {
    
  ## drop walkover matches (not countable)
  require(stringr)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]
  
  ## count occurrences of won matches
  res <- db[,.N, by=winner_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res
}

LossesOverall <- function() {
  
  ## drop walkover matches (not countable)
  require(stringr)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]
  
  ## count occurrences of lost matches
  res <- db[,.N, by=loser_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res
}


LossesCategory <- function(category) {
  
  ## only select tournaments in the previously defined pool
  db <- db[tourney_level == category]
  
  ## drop walkover matches (not countable)
  require(stringr)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]
  
  ## count occurrences of won matches
  res <- db[,.N, by=loser_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res
}


LossesSurface <- function(court) {
  
  ## only select tournaments in the previously defined pool
  db <- db[surface == court]
  
  ## drop walkover matches (not countable)
  require(stringr)
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]
  
  ## count occurrences of won matches
  res <- db[,.N, by=loser_name]
  
  ## order by decreasing
  setorder(res, -N, na.last=FALSE)
  
  res
}