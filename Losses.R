library(stringr)

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