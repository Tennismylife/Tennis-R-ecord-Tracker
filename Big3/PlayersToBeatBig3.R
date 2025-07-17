PlayersToBeatBig3 <- function(){
  
  stat <- db[loser_name == 'Roger Federer' | loser_name == 'Novak Djokovic' | loser_name == 'Rafael Nadal']
  
  stat <- unique(stat[, c("winner_name", "loser_name")])
  
  require(dplyr)
  stat <- stat %>%
    group_by(winner_name) %>%
    tally()
  
  ## order by decreasing
  setorder(stat, -n, na.last=FALSE)
  
}