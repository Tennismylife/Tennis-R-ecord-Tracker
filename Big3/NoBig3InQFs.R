#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
NoBig3inQFs <- function() {
  db <- db[tourney_level == 'M']
  
  wins <-
    db[(
      winner_name == 'Rafael Nadal' |
        winner_name == 'Roger Federer' | winner_name == 'Novak Djokovic'
    )]
  wins <-
    unique(wins[, c('tourney_name', 'tourney_id', 'winner_name')])
  
  losses <-
    db[(
      loser_name == 'Rafael Nadal' |
        loser_name == 'Roger Federer' | loser_name == 'Novak Djokovic'
    )]
  losses <-
    unique(losses[, c('tourney_name', 'tourney_id', 'loser_name')])
  
  names(wins)[3] <- names(losses)[3] <- "Player"
  
  ## merge the tables by "name"
  res <- rbind(wins, losses)
  
  
  res <- subset(db,!(db$tourney_id %in% res$tourney_id))
  
  #extract year from tourney_date
  res$year <- stringr::str_sub(res$tourney_id, 0 , 4)
  
  #print(res)
  
  res <-  unique(res[, c('tourney_name', 'year')])
  
  print(res)
}