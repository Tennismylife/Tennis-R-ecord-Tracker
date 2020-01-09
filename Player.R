PlayerStats <- function(player) {
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  #extract year from tourney_date
  db$tourney_id <- stringr::str_sub(db$tourney_id, 0 ,4)

  #db <- db[surface == 'Carpet']
  db <- db[tourney_id == '2000']
  db <- db[winner_name == player]
  

  db <- db[,c("round", "tourney_name", "tourney_id", "winner_rank", "winner_age", "winner_name", "loser_rank", "loser_age", "loser_name", "score", "surface")]
  
  #wins <- db[,.N, by=surface]
  
  #print(wins)
  
}