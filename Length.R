#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
MostLengthMatches <- function() {
  
  db <- removeTeamEvents(db)
  
  stat <- db
  
  stat <- stat[tourney_level == 'M']
  
  #player <- 'Novak Djokovic'
  
  #stat <- db[(winner_name == player | loser_name == player)]
  
  #stat$tourid <- sub("^[^-]*", "", stat$tourney_id)
  
  #extract id from tourney_id
  #stat <- stat[tourid == -580]
  
  stat[is.na(stat$minutes)] <- 0
  
  stat <- subset(stat, minutes > 180)
  
  # #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 , 4)
  
  ## order by decreasing
  stat <- setorder(stat, -minutes, na.last = FALSE)
  
  stat <-
    stat[, c("tourney_name",
             "year",
             "round",
             "winner_name",
             "loser_name",
             "score",
             "minutes")]
}