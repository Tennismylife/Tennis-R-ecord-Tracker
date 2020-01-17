LowestRankingRound <- function(stage) {
  
  db <- removeTeamEvents(db)
  
  dbm <- db
  
  #dbm <- dbm[tourney_level =='G']
  
  if(stage != 'W' & stage!='0')
    dbm <- dbm[round == stage]
  
  if(stage == 'W' & stage!='0')
    dbm <- dbm[round == 'F']
  
  dbm1 <- dbm[round == 'F']
  dbm1 <- unique(dbm1[,c('tourney_name', 'tourney_id', 'winner_ioc', 'winner_name', 'winner_rank')])
  
  if(stage != 'W')
    dbm2 <- unique(dbm[,c('tourney_name', 'tourney_id', 'loser_ioc', 'loser_name', 'loser_rank')])
  
  ## wins
  wins <- dbm1
  
  ## losses
  if(stage != 'W')
    losses <- dbm2
  
  ## common name to merge with
  names(wins)[3] <- "flag"
  names(wins)[4] <- "name"
  names(wins)[5]  <- "rank"
  
  if(stage != 'W'){
    names(losses)[3] <- "flag"
    names(losses)[4] <- "name"
    names(losses)[5] <- "rank"
  }
  
  ## merge the tables by "name"
  if(stage != 'W')
    res <- rbind( wins, losses, fill=TRUE)
  else 
    res <- wins
  
  res$tourney_id <- substr(res$tourney_id, 0, 4)
  
  names(res)[2] <- "year"

  ## order by decreasing rank
  res <- res[order(-rank)] 

  res <- res[1:100,]
  
  print(res)
}