require(ggplot2)

AverageAgeTour <- function(id, stage) {
  
  ## only select matches of a tournament
  db$tid <- sub("^[^-]*", "", db$tourney_id)
  
  dbm <- db[tid == id]
  
  if(stage != 'W' & stage!='0')
    dbm <- dbm[round == stage]
  else if(stage == 'W')
    dbm <- dbm[round == 'F']
  
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
  library(data.table)
  average <- setDT(res)[ , .(mean_age = mean(age)), by = tourney_id]
  
  #extract year from tourney_date
  average$tourney_id <- stringr::str_sub(average$tourney_id, 0 ,4)
  average$mean_age <- stringr::str_sub(average$mean_age, 0 ,5)
  
  average$mean_age <- gsub("\\.", ",", average$mean_age)
  
  ## order by decreasing
  setorder(average, tourney_id, na.last=FALSE)
  
  print(average)
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
  
  
#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
averageAgeinTournamentOfASeason <- function() {
  dbm <- db
  
  dbm <- dbm[round == 'F' & grepl("2021-", dbm$tourney_id)]
  
  wins <- dbm[, c('tourney_id', 'winner_age')]
  
  losses <- dbm[, c('tourney_id', 'loser_age')]
  
  names(wins)[2] <- "age"
  names(losses)[2] <- "age"
  
  res <- rbind(wins, losses)
  
  #setorder(res, -tourney_id)
  
  res[is.na(res)] <- 0
  
  officialName <- unique(dbm[, c('tourney_id', 'tourney_name')])
  
  res <- left_join(res, officialName, by = "tourney_id")
  
  print(res)
  
  average <-
    aggregate(
      res$age,
      by = list(
        tourney_id = res$tourney_id,
        tourney_name = res$tourney_name
      ),
      FUN = mean,
      na.rm = TRUE
    )
  
  average$x <- gsub("\\.", ",", average$x)
  
  setorder(average, -x)
  
  print(average)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
AverageAgeSeedingInATournament <- function(){
  
  stat <- db[tourney_level == 'M']
  
  stat$winner_seed <- as.integer(stat$winner_seed)
  stat$loser_seed <- as.integer(stat$loser_seed)
  
  winners <- stat[winner_seed < 5]
  
  winners <- unique(winners[, c('winner_age', 'winner_seed', 'tourney_id')])
  
  losers <- stat[loser_seed < 5]
  
  losers <- unique(losers[, c('loser_age', 'loser_seed', 'tourney_id')])
  
  names(winners)[1] <- names(losers)[1] <- "age"
  names(winners)[2] <- names(losers)[2] <- "seed"
  
  
  stat <- rbind(winners, losers, by = c("tourney_id"), fill = TRUE)
  
  
  ## order by decreasing
  setorder(stat, tourney_id, na.last=FALSE)
  
  stat <- unique(stat[,c('age', 'seed', 'tourney_id')])
  
  
  stat <- aggregate( age ~ tourney_id, stat, mean )
  
  res <- db[round == 'F' & tourney_level == 'M']
  officialName <-
    unique(res[, c('tourney_id', 'tourney_name')])
  
  stat <- join(officialName, stat, by = "tourney_id")
  
  #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- FormatAge(stat)
  
  stat <- stat[,c("tourney_name", "year", "age")]
  
  ## order by decreasing
  #setorder(stat, age, na.last=FALSE)
  
  print(stat)
}