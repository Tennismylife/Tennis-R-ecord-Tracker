library(stringr)

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
  
  res
}
