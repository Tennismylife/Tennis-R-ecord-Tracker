require(ggplot2)

AverageAgeTour <- function(tournament1, tournament2, tournament3, stage) {
  
  dbm <-  db
  
  if(stage != 'W' & stage!='0')
    dbm <- dbm[round == stage]
  else if(stage == 'W')
    dbm <- dbm[round == 'F']
  
  ## only select tournaments in the previously defined pool
  dbm <- dbm[tourney_name == tournament1 | tourney_name == tournament2 | tourney_name == tournament3]
  
  #tournaments won
  wins <- unique(dbm[,c('winner_id','tourney_name','tourney_id', 'winner_age')])
  
  #tournaments lost
  if(stage != 'W')
  losses <- unique(dbm[,c('loser_id','tourney_name','tourney_id', 'loser_age')])
  
  ## common name to merge with
  names(wins)[1] <- "name"
  names(wins)[4] <- "age"

  if(stage != 'W'){
    names(losses)[1] <- "name"
    names(losses)[4] <- "age"
  }

  ## merge the tables by "name"
  if(stage != 'W')
   res <- merge(wins, losses, by = c("name", "tourney_name", "tourney_id", "age"), all=TRUE)
  else
    res <- wins
  
  #calculate average by edition
  average <- aggregate(res$age, by=list(tourney_id=res$tourney_id), FUN=mean, na.rm=TRUE)
  
  #extract year from tourney_date
  average$tourney_id <- stringr::str_sub(average$tourney_id, 0 ,4)
  average$x <- stringr::str_sub(average$x, 0 ,5)
  
  average$x <- gsub("\\.", ",", average$x)
  
  print(average)

  #ggplot(average, aes(x = tourney_id, y = x)) + geom_point() + geom_line(size = 1)
  
  #ggsave("av-ggplot.png")
}


AverageAgeH2HRound <- function() {
  
  db <- db[round == 'F']
  db <- db[tourney_level == 'G']
  
  res <- db[, averageage:=(winner_age+loser_age)/2]
  
  #extract year from tourney_date
  res$tourney_id <- stringr::str_sub(res$tourney_id, 0 ,4)
  
  
  ## order by decreasing total matches
  setorder(res, -averageage)
  
  res <- res[,c("tourney_name", "tourney_id", "round", "winner_ioc", "winner_name", "winner_age", "loser_ioc", "loser_name", "loser_age", "averageage")]
  
}
  
  
  
