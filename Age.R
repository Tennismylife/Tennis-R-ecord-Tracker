EntrieOverallByAge <- function(order, stage) {
  
  db <- removeTeamEvents(db)
  
  dbm <- db
  
  if(stage != 'W' & stage!='0')
    dbm <- dbm[round == stage]
  
  if(stage == 'W' & stage!='0')
    dbm <- dbm[round == 'F']
  
  dbm1 <- dbm[round == 'F']
  dbm1 <- unique(dbm1[,c('tourney_name', 'tourney_id', 'winner_ioc', 'winner_name', 'winner_age')])
  
  if(stage != 'W')
  dbm2 <- unique(dbm[,c('tourney_name', 'tourney_id', 'loser_ioc', 'loser_name', 'loser_age')])
  
  ## wins
  wins <- dbm1
  
  ## losses
  if(stage != 'W')
  losses <- dbm2
  
  ## common name to merge with
  names(wins)[3] <- "flag"
  names(wins)[4] <- "name"
  names(wins)[5]  <- "age"
  
  if(stage != 'W'){
  names(losses)[3] <- "flag"
  names(losses)[4] <- "name"
  names(losses)[5] <- "age"
  }
  
  ## merge the tables by "name"
  if(stage != 'W')
  res <- rbind( wins, losses, fill=TRUE)
  else 
  res <- wins
  
  res$tourney_id <- substr(res$tourney_id, 0, 4)
  
  res$age <- substr(res$age, 0, 5)
  res$age <- suppressWarnings(as.numeric(str_replace_all(res$age,pattern=',',replacement='.')))
  
  if(order == 'oldest'){
  ## order by decreasing age
  res <- res[order(-age)] 
  }
  
  if(order == 'youngest'){
    ## order by decreasing age
    res <- res[order(age)] 
  } 
  res <- res[1:100,]
  
  print(res)
}


EntriecategoryByAge <- function(category, order, stage) {

  ind_Dusseldorf <- grep("^World Team Cup", db$tourney_name)
  if (length(ind_Dusseldorf)>0)
    db <- db[-ind_Dusseldorf, ]
  
  ind_davis <- grep("^Davis", db$tourney_name)
  if (length(ind_davis)>0)
    db <- db[-ind_davis, ]
  
  db <- db[tourney_level == category]
  
  dbm <- db
  
  if(stage != 'W' & stage!='0')
    dbm <- dbm[round == stage]
  
  if(stage == 'W')
    dbm <- dbm[round == 'F']
  
  dbm1 <- dbm[round == 'F']
  dbm1 <- unique(dbm1[,c('tourney_name', 'tourney_id', 'winner_ioc', 'winner_name', 'winner_age')])
  
  if(stage != 'W')
    dbm2 <- unique(dbm[,c('tourney_name', 'tourney_id', 'loser_ioc', 'loser_name', 'loser_age')])
  
  ## wins
  wins <- dbm1
  
  ## losses
  if(stage != 'W')
    losses <- dbm2
  
  ## common name to merge with
  names(wins)[3] <- "flag"
  names(wins)[4] <- "name"
  names(wins)[5]  <- "age"
  
  if(stage != 'W'){
    names(losses)[3] <- "flag"
    names(losses)[4] <- "name"
    names(losses)[5] <- "age"
  }
  
  ## merge the tables by "name"
  if(stage != 'W')
    res <- rbind( wins, losses, fill=TRUE)
  else 
    res <- wins
  
  res$tourney_id <- substr(res$tourney_id, 0, 4)
  
  res$age <- substr(res$age, 0, 5)
  res$age <- suppressWarnings(as.numeric(str_replace_all(res$age,pattern=',',replacement='.')))
  
  if(order == 'oldest'){
    ## order by decreasing age
    res <- res[order(-age)] 
  }
  
  if(order == 'oldest'){
    ## order by decreasing age
    res <- res[order(-age)] 
  } 
  
  if(order == 'youngest'){
    ## order by decreasing age
    res <- res[order(age)] 
  } 
  res <- res[1:100,]
  
  print(res)
}


EntrieSurfaceByAge <- function(court, order, stage) {
  ind_Dusseldorf <- grep("^World Team Cup", db$tourney_name)
  if (length(ind_Dusseldorf)>0)
    db <- db[-ind_Dusseldorf, ]
  
  ind_davis <- grep("^Davis", db$tourney_name)
  if (length(ind_davis)>0)
    db <- db[-ind_davis, ]
  
  db <- db[surface == court]
  
  dbm <- db
  
  if(stage != 'W' & stage!='0')
    dbm <- dbm[round == stage]
  
  if(stage == 'W')
    dbm <- dbm[round == 'F']
  
  dbm1 <- dbm[round == 'F']
  dbm1 <- unique(dbm1[,c('tourney_name', 'tourney_id', 'winner_ioc', 'winner_name', 'winner_age')])
  
  if(stage != 'W')
    dbm2 <- unique(dbm[,c('tourney_name', 'tourney_id', 'loser_ioc', 'loser_name', 'loser_age')])
  
  ## wins
  wins <- dbm1
  
  ## losses
  if(stage != 'W')
    losses <- dbm2
  
  ## common name to merge with
  names(wins)[3] <- "flag"
  names(wins)[4] <- "name"
  names(wins)[5]  <- "age"
  
  if(stage != 'W'){
    names(losses)[3] <- "flag"
    names(losses)[4] <- "name"
    names(losses)[5] <- "age"
  }
  
  ## merge the tables by "name"
  if(stage != 'W')
    res <- rbind( wins, losses, fill=TRUE)
  else 
    res <- wins
  
  res$tourney_id <- substr(res$tourney_id, 0, 4)
  
  res$age <- substr(res$age, 0, 5)
  res$age <- suppressWarnings(as.numeric(str_replace_all(res$age,pattern=',',replacement='.')))
  
  if(order == 'oldest'){
    ## order by decreasing age
    res <- res[order(-age)] 
  }
  
  if(order == 'oldest'){
    ## order by decreasing age
    res <- res[order(-age)] 
  } 
  
  if(order == 'youngest'){
    ## order by decreasing age
    res <- res[order(age)] 
  } 
  res <- res[1:100,]
  
  print(res)
}


EntrieTourByAge <- function(tournament, order, stage) {
  
  
  dbm <- db[tourney_name == tournament]
  
  if(stage != 'W' & stage!='0')
    dbm <- dbm[round == stage]
  
  if(stage == 'W')
    dbm <- dbm[round == 'F']
  
  dbm1 <- unique(dbm[,c('tourney_name', 'tourney_id', 'winner_ioc', 'winner_name', 'winner_age')])
  
  if(stage != 'W')
  dbm2 <- unique(dbm[,c('tourney_name', 'tourney_id', 'loser_ioc', 'loser_name', 'loser_age')])
  
  ## wins
  wins <- dbm1
  
  ## losses
  if(stage != 'W')
  losses <- dbm2
  
  ## common name to merge with
  names(wins)[3] <- "flag"
  names(wins)[4] <- "name"
  names(wins)[5]  <- "age"
  
  if(stage != 'W')
  {
  names(losses)[3] <- "flag"
  names(losses)[4] <- "name"
  names(losses)[5] <- "age"
  }
  
  ## merge the tables by "name"
  if(stage != 'W'){
    res <- rbind(wins, losses, fill=TRUE)
    res <- unique(res)
  }
  else 
    res <- wins
  
  res$tourney_id <- substr(res$tourney_id, 0, 4)
  
  res$age <- substr(res$age, 0, 5)
  res$age <- suppressWarnings(as.numeric(str_replace_all(res$age,pattern=',',replacement='.')))
  
  if(order == 'oldest'){
    ## order by decreasing age
    res <- res[order(-age)] 
  }
  
  if(order == 'youngest'){
    ## order by creasing age
    res <- res[order(age)] 
  } 
  res <- res[1:100,]
  
  print(res)
}