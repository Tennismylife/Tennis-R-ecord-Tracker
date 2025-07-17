EntriesBig3InSlams <- function(){
  
  ## get rid of NAs, have 0 instead
  db[is.na(db)] <- 0
  
  wins <- db[tourney_level == 'G' & (winner_name == 'Roger Federer' | winner_name == 'Novak Djokovic' | winner_name == 'Rafael Nadal')] 
  
  losses <- db[tourney_level == 'G' & (loser_name == 'Roger Federer' | loser_name == 'Novak Djokovic' | loser_name == 'Rafael Nadal')]
  
  wins <- wins[, c("tourney_id", "winner_name")]
  
  losses <- losses[, c("tourney_id", "loser_name")]
  
  names(wins)[2] <- names(losses)[2] <- "player"
  
  stat <- rbind(wins, losses)
  
  print(stat)
  
  stat <- unique(stat[, c("tourney_id", "player")])
  
  #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  require(dplyr)
  stat <- stat %>%
    group_by(tourney_id) %>%
    tally()
  
  
  res <- db[round == 'QF']
  officialName <- unique(res[, c('tourney_id', 'tourney_name')])
  
  stat <- right_join(officialName, stat, by = "tourney_id") 
  
  #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- stat[, c("tourney_name", "year", "n")]
  
}