TimespanTournamentEntry <- function(id, stage) {
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  dbm <- db[tourney_id == id]
  
  if(length(stage) > 0 & stage != 'W' & stage !='0'){
    dbm <- dbm[round == stage]
  }
  
  if(stage == 'W')
    dbm <- dbm[round == 'F']

  #tournaments won
  wins <- unique(dbm[,c('winner_name','tourney_name','tourney_date', 'winner_age')])

  #tournaments lost
  if(stage !='W')
    losses <- unique(dbm[,c('loser_name','tourney_name','tourney_date', 'loser_age')])

  ## common name to merge with
  names(wins)[1] <- "name"
  names(wins)[4] <- "age"

  if(stage !='W'){
    ## common name to merge with
    names(losses)[1] <- "name"
    names(losses)[4] <- "age"
    
    ## merge the tables by "name"
    res <- merge(wins, losses, by = c("name", "tourney_name", "tourney_date", "age"), all=TRUE)
  }else{
    res <- wins
  }
  
  #transform date format
  res$tourney_date <- lubridate::ymd(res$tourney_date)

  #Select first and last element in date by name
  firstandlastdate<- res[, .SD[c(1,.N)], by=name]

  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=name]

  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=name]

  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_date)

  #rename columns
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_date"

  #erase age column
  timespan$age <- NULL

  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))

  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]

  #rename columns
  names(timespan)[1] <- "Player"
  names(timespan)[2] <- "Tournament"
  names(timespan)[3] <- "1st date"
  names(timespan)[4] <- "last date"
  names(timespan)[5] <- "days"
  
  print(timespan)
  
}

TimespanTournamentWins <- function(id) {
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  db <- db[tourney_id == id]
  
  #tournaments won
  wins <- unique(db[,c('winner_name','tourney_name','tourney_date', 'winner_age')])
  
  #transform date format
  wins$tourney_date <- lubridate::ymd(wins$tourney_date)
  
  #order by date in subgroup by player
  wins<-wins[order(wins$winner_name,wins$tourney_date),]
  
  #Select first and last element in date by name
  firstandlastdate<- wins[, .SD[c(1,.N)], by=winner_name]
  
  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=winner_name]
  
  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=winner_name]
  
  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_date)
  
  #rename columns
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_date"
  
  #erase age column
  timespan$winner_age <- NULL
  
  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))
  
  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]
  
  print(timespan)
}


TimespanOverallWins <- function() {
  
  ## only select tournaments in the previously defined pool
  dbm <- db
  
  #tournaments won
  wins <- unique(dbm[,c('winner_name','tourney_name','tourney_date', 'winner_age')])
  
  ## common name to merge with
  names(wins)[1] <- "name"
  names(wins)[4] <- "age"
  
  ## merge the tables by "name"
  res <- wins
  
  #transform date format
  res$tourney_date <- lubridate::ymd(res$tourney_date)
  
  #order by date in subgroup by player
  res<-res[order(res$name,res$tourney_date),]
  
  #Select first and last element in date by name
  firstandlastdate<- res[, .SD[c(1,.N)], by=name]
  
  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=name]
  
  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=name]
  
  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_name,lastdate$tourney_date)
  
  #rename columns
  names(timespan)[2] <- "first_tournament"
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_tournament"
  names(timespan)[6] <- "last_date"
  
  #erase age column
  timespan$age <- NULL
  
  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))
  
  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]
  
  print(timespan)
}

TimespanCategoryWins <- function(category) {
  ## only select tournaments in the previously defined pool
  dbm <- db
  
  #select category matches
  dbm <- db[tourney_level == category]
  
  #tournaments won
  wins <- unique(dbm[,c('winner_name','tourney_name','tourney_date', 'winner_age')])
  
  ## common name to merge with
  names(wins)[1] <- "name"
  names(wins)[4] <- "age"
  
  ## merge the tables by "name"
  res <- wins
  
  #transform date format
  res$tourney_date <- lubridate::ymd(res$tourney_date)
  
  #order by date in subgroup by player
  res<-res[order(res$name,res$tourney_date),]
  
  #Select first and last element in date by name
  firstandlastdate<- res[, .SD[c(1,.N)], by=name]
  
  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=name]
  
  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=name]
  
  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_name,lastdate$tourney_date)
  
  #rename columns
  names(timespan)[2] <- "first_tournament"
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_tournament"
  names(timespan)[6] <- "last_date"
  
  #erase age column
  timespan$age <- NULL
  
  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))
  
  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]
  
  print(timespan)
}

TimespaSurfaceWins <- function(court) {
  
  ## only select tournaments in the previously defined pool
  dbm <- db
  
  #select category matches
  dbm <- db[surface == court]
  
  #tournaments won
  wins <- unique(dbm[,c('winner_name','tourney_name','tourney_date', 'winner_age')])
  
  ## common name to merge with
  names(wins)[1] <- "name"
  names(wins)[4] <- "age"
  
  ## merge the tables by "name"
  res <- wins
  
  #transform date format
  res$tourney_date <- lubridate::ymd(res$tourney_date)
  
  #order by date in subgroup by player
  res<-res[order(res$name,res$tourney_date),]
  
  #Select first and last element in date by name
  firstandlastdate<- res[, .SD[c(1,.N)], by=name]
  
  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=name]
  
  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=name]
  
  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_name,lastdate$tourney_date)
  
  #rename columns
  names(timespan)[2] <- "first_tournament"
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_tournament"
  names(timespan)[6] <- "last_date"
  
  #erase age column
  timespan$age <- NULL
  
  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))
  
  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]
  
  print(timespan)
}


################################################################################## ENTRY ##########################################################################
TimespanOverallEntry <- function(stage) {
  db <- removeTeamEvents(db)

  ## only select tournaments in the previously defined pool
  dbm <- db
  
  if(length(stage) > 0 & stage != 'W' && stage!='0')
    dbm <- dbm[round == stage]
  
  if(stage == 'W')
    dbm <- dbm[round == 'F']
  
  #tournaments won
  wins <- unique(dbm[,c('winner_name','tourney_name','tourney_date', 'winner_age')])
  
  #tournaments lost
  if(stage !='W')
  losses <- unique(dbm[,c('loser_name','tourney_name','tourney_date', 'loser_age')])
  
  names(wins)[1]<- "name"
  names(wins)[4]<- "age"
  
  if(stage !='W'){
  ## common name to merge with
  names(losses)[1] <- "name"
  names(losses)[4] <- "age"
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name", "tourney_name", "tourney_date", "age"), all=TRUE)
  }else{
    res <- wins
  }
  
  #transform date format
  res$tourney_date <- lubridate::ymd(res$tourney_date)
  
  #order by date in subgroup by player
  res<-res[order(res$name,res$tourney_date),]
  
  #Select first and last element in date by name
  firstandlastdate<- res[, .SD[c(1,.N)], by=name]
  
  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=name]
  
  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=name]
  
  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_name,lastdate$tourney_date)
  
  #rename columns
  names(timespan)[2] <- "first_tournament"
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_tournament"
  names(timespan)[6] <- "last_date"
  
  #erase age column
  timespan$age <- NULL
  
  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))
  
  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]
  
  #rename columns
  names(timespan)[1] <- "Player"
  names(timespan)[2] <- "1st tournament"
  names(timespan)[3] <- "1st date"
  names(timespan)[4] <- "last tournament"
  names(timespan)[5] <- "last date"
  names(timespan)[6] <- "days"
  
  print(timespan)
  
}


TimespanCategoryEntry <- function(category, stage) {
  
  dbm <- db
  
  dbm <- dbm[tourney_level == category]
  
  if(length(stage) > 0 & stage != 'W' && stage !='0'){
    dbm <- dbm[round == stage]
  }
  
  if(stage == 'W')
    dbm <- dbm[round == 'F']
  
  #tournaments won
  wins <- unique(dbm[,c('winner_name','tourney_name','tourney_date', 'winner_age')])
  
  #tournaments lost
  if(stage !='W')
    losses <- unique(dbm[,c('loser_name','tourney_name','tourney_date', 'loser_age')])
  
  ## common name to merge with
  names(wins)[1] <- "name"
  names(wins)[4] <- "age"
  
  if(stage !='W'){
    ## common name to merge with
    names(losses)[1] <- "name"
    names(losses)[4] <- "age"
    ## merge the tables by "name"
    res <- merge(wins, losses, by = c("name", "tourney_name", "tourney_date", "age"), all=TRUE)
  }else{
    res <- wins
  }
  
  #transform date format
  res$tourney_date <- lubridate::ymd(res$tourney_date)
  
  #order by date in subgroup by player
  res<-res[order(res$name,res$tourney_date),]
  
  #Select first and last element in date by name
  firstandlastdate<- res[, .SD[c(1,.N)], by=name]
  
  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=name]
  
  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=name]
  
  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_name,lastdate$tourney_date)
  
  #rename columns
  names(timespan)[2] <- "first_tournament"
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_tournament"
  names(timespan)[6] <- "last_date"
  
  #erase age column
  timespan$age <- NULL
  
  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))
  
  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]
  
  #rename columns
  names(timespan)[1] <- "Player"
  names(timespan)[2] <- "1st tournament"
  names(timespan)[3] <- "1st date"
  names(timespan)[4] <- "last tournament"
  names(timespan)[5] <- "last date"
  names(timespan)[6] <- "days"
  
  print(timespan)
}

TimespaSurfaceEntry <- function(court, stage) {
  
  db <- removeTeamEvents(db)
  
  dbm <- db
  
  dbm <- dbm[surface == court]
  
  if(length(stage) > 0 & stage != 'W' && stage !='0'){
    dbm <- dbm[round == stage]
  }
  
  if(stage == 'W')
    dbm <- dbm[round == 'F']
  
  #tournaments won
  wins <- unique(dbm[,c('winner_name','tourney_name','tourney_date', 'winner_age')])
  
  #tournaments lost
  if(stage !='W')
  losses <- unique(dbm[,c('loser_name','tourney_name','tourney_date', 'loser_age')])
  
  ## common name to merge with
  names(wins)[1] <- "name"
  names(wins)[4] <- "age"
  
  if(stage !='W'){
    ## common name to merge with
    names(losses)[1] <- "name"
    names(losses)[4] <- "age"
    ## merge the tables by "name"
    res <- merge(wins, losses, by = c("name", "tourney_name", "tourney_date", "age"), all=TRUE)
  }else{
    res <- wins
  }
  
  #transform date format
  res$tourney_date <- lubridate::ymd(res$tourney_date)
  
  #order by date in subgroup by player
  res<-res[order(res$name,res$tourney_date),]
  
  #Select first and last element in date by name
  firstandlastdate<- res[, .SD[c(1,.N)], by=name]
  
  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=name]
  
  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=name]
  
  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_name,lastdate$tourney_date)
  
  #rename columns
  names(timespan)[2] <- "first_tournament"
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_tournament"
  names(timespan)[6] <- "last_date"
  
  #erase age column
  timespan$age <- NULL
  
  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))
  
  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]
  
  #rename columns
  names(timespan)[1] <- "Player"
  names(timespan)[2] <- "1st tournament"
  names(timespan)[3] <- "1st date"
  names(timespan)[4] <- "last tournament"
  names(timespan)[5] <- "last date"
  names(timespan)[6] <- "days"
  
  
  print(timespan)
}


SameTournamentTimespan <- function(){
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  stat <- NULL
  
  tour <- unique(dplyr::pull(db, tourney_id))
  
  for(i in 1:length(tour)){
    
    stat2 <- TimespanTournamentEntry(tour[i], 'W')
    
    stat <- rbind(stat, stat2)
    
  }
  
  stat <- subset(stat, days > 5000)
  
}