source("Same.R")


PlayerStats <- function(player) {
  #db <- removeTeamEvents(db)
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  #extract year from tourney_date
  db$tourney_id <- stringr::str_sub(db$tourney_id, 0 ,4)

  db <- db[winner_name == player | loser_name == player]

  dbm1 <- db[winner_name == player & l_bpSaved > 15]
  res1 <- dbm1[,c("round", "tourney_name", "tourney_id", "winner_rank", "winner_age", "winner_name", "loser_rank", "loser_age", "loser_name", "score", "surface", "w_bpSaved", "w_bpFaced", "l_bpSaved", "l_bpFaced")]
  
  #dbm1 <- db[loser_name == player & w_bpSaved > 15]
  #res2 <- dbm1[,c("round", "tourney_name", "tourney_id", "winner_rank", "winner_age", "winner_name", "loser_rank", "loser_age", "loser_name", "score", "surface", "w_bpSaved", "w_bpFaced", "l_bpSaved", "l_bpFaced")]
  
  
  #wins <- db[,.N, by=surface]
  
  #print(wins)
  
}