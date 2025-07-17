TournamentWonByABig3WillAllInDraw <- function() {
  db <- db[tourney_level == 'G']
  
  Nadal <-
    db[winner_name == 'Rafael Nadal' | loser_name == 'Rafael Nadal']
  Nadal <- unique(Nadal[, c('tourney_id')])
  
  Djokovic <-
    db[winner_name == 'Novak Djokovic' |
         loser_name == 'Novak Djokovic']
  Djokovic <- unique(Djokovic[, c('tourney_id')])
  
  Federer <-
    db[winner_name == 'Roger Federer' | loser_name == 'Roger Federer']
  Federer <- unique(Federer[, c('tourney_id')])
  
  NadalAndDjokovic <-
    subset(Djokovic, (Djokovic$tourney_id  %in% Nadal$tourney_id))
  
  #tourney_id where all Big 3 are present
  NadalAndDjokovicAndFederer <-
    subset(Federer,
           (Federer$tourney_id  %in% NadalAndDjokovic$tourney_id))
  
  
  NadalSF <- db[winner_name == 'Rafael Nadal' & round == 'R32']
  NadalSF <- unique(NadalSF[, c('tourney_id')])
  
  DjokovicSF <- db[winner_name == 'Novak Djokovic' & round == 'R32']
  DjokovicSF <- unique(DjokovicSF[, c('tourney_id')])
  
  FedererSF <- db[winner_name == 'Roger Federer' & round == 'R32']
  FedererSF <- unique(FedererSF[, c('tourney_id')])
  
  NadalAndDjokovicAndFedererSF <-
    rbind(NadalSF, DjokovicSF, FedererSF)
  NadalAndDjokovicAndFedererSF <-
    unique(NadalAndDjokovicAndFedererSF[, c('tourney_id')])
  
  
  NadalAndDjokovicAndFedererSF <- FedererSF
  
  
  NoDjokovicNoNadalandFederer <-
    subset(
      NadalAndDjokovicAndFederer,
      (
        NadalAndDjokovicAndFederer$tourney_id  %in% NadalAndDjokovicAndFedererSF$tourney_id
      )
    )
  
  
  print(NoDjokovicNoNadalandFederer)
  
  
  res <- db[round == 'R32']
  officialName <- unique(res[, c('tourney_id', 'tourney_name')])
  toreach <-
    right_join(officialName, NoDjokovicNoNadalandFederer, by = "tourney_id")
  
  #extract year from tourney_date
  toreach$year <- stringr::str_sub(toreach$tourney_id, 0 , 4)
  
  
  print(toreach)
  
}