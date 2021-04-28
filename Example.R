library("xlsx")
library("dplyr")
library("stringr")
library(tableHTML)
library(splitstackshape)
library(tibble)

#Main
source("Reader.R")
source("Played.R")
source("Wins.R")
source("Entries.R")
source("Timespan.R")
source("Season.R")
source("CounterSeason.R")
source("Age.R")
source("Counter.R")
source("AverageAge.R")
source("Percentage.R")
source("Consecutive.R")
source("AgeFormat.R")

#if(FALSE){


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
MostWinsNoSlammer <- function() {
  
  res <- db[tourney_level == 'G' & round == 'F']
  
  res <- res[, c("winner_name")]
  
  wins <- WinsCategory('G')
  
  wins <- subset(wins, !(wins$winner_name %in% res$winner_name))
  
  print(wins)
  
  ## order by decreasing
  setorder(wins, -N, na.last=FALSE)
  
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
PercentageInSlamsVsTop10 <- function() {
  
  db <- db[!str_detect(db$tourney_name, "Laver Cup")]
  
  db <- db[surface == 'Hard']
  
  ## 5th
  #db <- db[str_count(db$score, "-") == '5']
  
  #Decider
  #db <- db[(str_count(db$score, "-") == '5' & best_of == 5) | (str_count(db$score, "-") == '3' & best_of == 3)]
  
  dbm <- db[loser_rank < 11]
  
  #dbm <- db[winner_age < 23.45]
  
  #dbm <- dbm[tourney_level =='G']
  
  ## drop walkover matches (not countable)
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !str_detect(dbm$score, "WEA") & !str_detect(dbm$score, "ABN")]
  
  wins <- dbm[, .N, by = winner_name]
  
  ## losses
  dbm <- db[winner_rank < 11]
  
  #dbm <- db[loser_age < 23.45]
  
  #dbm <- dbm[tourney_level =='G']
  
  ## drop walkover matches (not countable)
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !str_detect(dbm$score, "WEA") & !str_detect(dbm$score, "ABN")]
  
  losses <- dbm[, .N, by = loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all = TRUE)
  
  ## sum the wins and losses into a new column played
  res <- res[, played := wins + losses]
  
  res <- res[played > 30]
  
  res <- res[, percentage := wins / played * 100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <-
    suppressWarnings(as.numeric(
      str_replace_all(res$percentage, pattern = ',', replacement = '.')
    ))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  print(res)
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
TimeToWinSlam <- function() {
  res <- db[round == 'F' & tourney_level == 'G']
  res <-
    res[, c("winner_name",
            "tourney_id",
            "tourney_name",
            "tourney_date",
            "winner_name")]
  
  db <- db[tourney_level == 'G']
  
  wins <- match_df(db, res)
  
  #calculate sum by edition
  timeoncourt <-
    aggregate(
      wins$minutes,
      by = list(tourney_id = wins$tourney_id),
      FUN = sum,
      na.rm = TRUE
    )
  
  names(timeoncourt)[2] <- "minutes"
  
  timeoncourt <- arrange(timeoncourt, minutes)
  
  res <- db[round == 'F' & tourney_level == 'G']
  officialName <-
    unique(res[, c('tourney_id', 'tourney_name', 'tourney_date', 'winner_name')])
  
  timeoncourt <-
    left_join(officialName, timeoncourt, by = "tourney_id")
  
  timeoncourt <- arrange(timeoncourt, desc(minutes))
  
  #extract year from tourney_date
  timeoncourt$year <- stringr::str_sub(timeoncourt$tourney_id, 0 ,4)
  
  timeoncourt <- timeoncourt[, c('tourney_name', 'year', 'winner_name', 'minutes')]
  
  
  print(timeoncourt)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
PercentageAsNumber1 <- function() {
  dbm <- db
  
  dbm <-
    dbm[!dbm$score == "W/O" &
          !dbm$score == "DEF" &
          !dbm$score == "(ABN)" & !dbm$score == "ABN"]
  
  dbm$tourney_id <- stringr::str_sub(dbm$tourney_id, 0 , 4)
  #dbm <- dbm[tourney_id == year]
  
  ## wins
  dbm1 <- dbm[winner_rank == '1']
  wins <- dbm1[, .N, by = winner_name]
  
  ## losses
  dbm1 <- dbm[loser_rank == '1']
  losses <- dbm1[, .N, by = loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all = TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played := wins + losses]
  res <- res[, percentage := (wins / played) * 100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <-
    suppressWarnings(as.numeric(
      str_replace_all(res$percentage, pattern = ',', replacement = '.')
    ))
  
  res$percentage <- paste(res$percentage, "%")
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  #res <- res[1:1,]
  print(res)
  
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
WinsAgainstNumber1 <- function() {
  dbm <- db
  
  #dbm <- dbm[loser_name == 'Roger Federer' & surface == 'Grass']
  
  dbm <-
    dbm[!dbm$score == "W/O" &
          !dbm$score == "DEF" &
          !dbm$score == "(ABN)" & !dbm$score == "ABN"]
  
  ## ranking #1
  dbm <- dbm[loser_rank == '1']
  
  losses <- dbm[, .N, by = winner_name]
  
  ## order by decreasing total matches
  setorder(losses, -N)
  
  losses <- losses[1:20,]
  
  print(losses)
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
Most5SetterPlayed <- function() {
  
  db <- db[tourney_level == 'G']
  
  ## drop walkover matches (not countable)
  db <- db[!db$score == "W/O" &
             !db$score == "DEF" & !db$score == "(ABN)"]
  
  wins <- db[, c('winner_name', 'tourney_id', 'score')]
  
  wins <-
    wins[lengths(regmatches(wins$score, gregexpr("-", wins$score))) == '5']
  
  losses <- db[, c('loser_name', 'tourney_id', 'score')]
  
  losses <-
    losses[lengths(regmatches(losses$score, gregexpr("-", losses$score))) == '5']
  
  names(wins)[1] <- names(losses)[1] <- "name"
  
  matches <- union(wins, losses)
  
  #extract year from tourney_date
  matches$tourney_id <- stringr::str_sub(matches$tourney_id, 0 , 4)
  
  names(matches)[2] <- "year"
  
  #matches <- matches[year == '2020']
  
  print(matches)
  
  season <- matches[, .N, by = list(matches$name, matches$year)]
  
  count <- matches[, .N, by = matches$name]
  
  count <- count[order(-N)]
  
  print(count)
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
NoBig3inQFs <- function() {
  
  db <- db[tourney_level == 'M']
  
  wins <- db[(winner_name == 'Rafael Nadal' | winner_name == 'Roger Federer' | winner_name == 'Novak Djokovic')]
  wins <-  unique(wins[, c('tourney_name', 'tourney_id', 'winner_name')])
  
  losses <- db[(loser_name == 'Rafael Nadal' | loser_name == 'Roger Federer' | loser_name == 'Novak Djokovic')]
  losses <- unique(losses[, c('tourney_name', 'tourney_id', 'loser_name')])
  
  names(wins)[3] <- names(losses)[3] <- "Player"
  
  ## merge the tables by "name"
  res <- rbind(wins, losses)
  

  res <- subset(db, !(db$tourney_id %in% res$tourney_id))
  
  #extract year from tourney_date
  res$year <- stringr::str_sub(res$tourney_id, 0 ,4)
  
  #print(res)
  
  res <-  unique(res[, c('tourney_name', 'year')])
  
  print(res)
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
RoundAtAge <- function() {
  
  db <- removeTeamEvents(db)
  
  db <- db[tourney_level == 'M'] 
  
  #stage <-  'F'
  
  #db <- db[round == stage]
  
  db <- db[winner_age < 24]
  
  #db <- db[winner_name == 'Bjorn Borg']
  
  #if(stage == 'F')
    #db <- db[round == 'F' & !str_detect(db$score, "WEA")]
  
  res <- db[, .N, by = list(db$winner_name)]
  
  setorder(res, -N)
  
  print(res)
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
PercentageEntryWinsinCategorySurfaceOverall <-  function() {
  
  category <- 'M'
  
  surface <- 'Clay'
  
  db <- removeTeamEvents(db)
  
  #entries <- EntriesCategory(category)
  
  #counter <- CountCategoryRound(category, "W")
  
  #entries <- EntriesOverall()
  
  #counter <- CountOverallRound('W')
  
  entries <- EntriesSurface(surface)
  
  counter <- CountSurfaceRound(surface, 'F')
  
  #counter <- subset(counter, wins > 10)
  
  percentage <- merge(entries, counter, by = "name", all = TRUE)
  
  percentage[is.na(percentage)] <- 0
  
  res <- percentage[, percentage := played / entries * 100]
  
  print(percentage)
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  print(res)
  
  res <- res[,c("name", "wins", "entries", "percentage")]
  
  return(res)
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
PercentageSameSeasonbyPlayer <- function() {
  
  player <- 'Roger Federer'
  
  dbm <- db
  dbm <-
    dbm[!dbm$score == "W/O" & !dbm$score == "DEF" &
          !dbm$score == "(ABN)"]
  
  #dbm <- dbm[surface == 'Clay']
  
  #extract year from tourney_date
  dbm$tourney_id <- stringr::str_sub(dbm$tourney_id, 0 , 4)
  
  ## wins
  dbm1 <- dbm[winner_name ==  player]
  wins <- dbm1[, .N, by = list(winner_name, tourney_id)]
  
  ## losses
  dbm1 <- dbm[loser_name == player]
  losses <- dbm1[, .N, by = list(loser_name, tourney_id)]
  
  dbm2 <- dbm[round == 'F']
  trophy <- dbm2[, .N, by = list(winner_name, tourney_id)]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- names(trophy)[1] <- "name"
  names(wins)[3] <- "wins"
  names(losses)[3] <- "losses"
  
  ## merge the tables by "name"
  res <-
    merge(wins,
          losses,
          by = c("name", "tourney_id"),
          all.x = TRUE)
  
  ## merge the tables by "name"
  res <-
    merge(res,
          trophy,
          by = c("name", "tourney_id"),
          all.x = TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played := wins + losses]
  
  totalLosses <- 0
  totalwins <- 0
  totalTitles <- 0
  
  library(foreach)
  foreach(i = 1:length(res$played)) %do%
    {
      totalLosses <- totalLosses + res$losses[i]
      res$totLosses[i] <-  totalLosses
      
      totalwins <- totalwins + res$wins[i]
      res$totalwins[i] <-  totalwins
      
      res$percTot[i] <- totalwins / (totalwins + totalLosses) * 100
      
      totalTitles <- totalTitles + res$N[i]
      res$totTitle[i] <- totalTitles
      
    }
  
  ## calculate winning percentage
  #res <- res[played > 50]
  
  res <- res[, percyear := wins / played * 100]
  
  res$percyear <- substr(res$percyear, 0, 5)
  res$percyear <-
    suppressWarnings(as.numeric(
      str_replace_all(res$percyear, pattern = ',', replacement = '.')
    ))
  
  res$percyear <- paste(res$percyear, "%")
  
  
  res$percTot <- substr(res$percTot, 0, 5)
  res$percTot <-
    suppressWarnings(as.numeric(str_replace_all(
      res$percTot, pattern = ',', replacement = '.'
    )))
  
  res$percTot <- paste(res$percTot, "%")
  
  
  names(res)[2] <- "year"
  
  ## order by decreasing total matches
  setorder(res, year)
  
  res <-
    res[, c(
      "name",
      "year",
      "wins",
      "losses",
      "percyear",
      "N",
      "totalwins",
      "totLosses",
      "percTot",
      "totTitle"
    )]
  
  
  #res <- res[1:100,]
  print(res)
  
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
MostEntriesNoTitle <- function() {
  
  stat <- SameTournamentEntries()
  
  res <- db[round == 'F']
  res <- res[, c("tourney_name", "winner_name")]
  
  names(res)[1] <- "Tournament"
  names(res)[2] <- "Player"
  
  print(res)
  
  res <- res[, c("Tournament", "Player")]
  
  stat <- anti_join(stat, res)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
Top10ToWinMasters <- function() {
  
  category <- 'M'
  
  stat <- db[tourney_level == category & loser_rank < 11]
  
  res <- db[round == 'F' & tourney_level == category]
  res <-
    res[, c("tourney_id", "tourney_name", "tourney_date", "winner_name")]
  
  dbm <- db[tourney_level == category & loser_rank < 11]
  wins <- match_df(dbm, res)
  
  library(plyr)
  stat <- aggregate(
    cbind(count = tourney_id) ~ tourney_id,
    data = wins,
    FUN = function(x) {
      NROW(x)
    }
  )
  
  res <- db[round == 'F' & tourney_level == category]
  officialName <-
    unique(res[, c('tourney_id', 'tourney_name', 'winner_name')])
  
  stat <- join(officialName, stat, by = "tourney_id")
  
  #extract year from tourney_id
  stat$tourney_id <- stringr::str_sub(stat$tourney_id, 0 , 4)
  
  #stat <- stat[,c("tourney_id", "tourney_name")]
  
  ## order by decreasing
  setorder(stat,-count, na.last = FALSE)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
LessWinsCategorybyWinner <- function(category) {
  
  res <- db[round == 'F' & tourney_level == category]
  
  res <- res[, c("winner_name")]
  
  dbm <- db[tourney_level == category]
  wins <- match_df(dbm, res)
  
  ## only select tournaments in the previously defined pool
  dbm <- wins
  
  ## drop walkover matches (not countable)
  dbm <-
    dbm[!dbm$score == "W/O" & !dbm$score == "DEF" &
          !dbm$score == "(ABN)"]
  
  ## count occurrences of won matches
  res <- dbm[, .N, by = winner_name]
  
  ## order by decreasing
  setorder(res, N, na.last = FALSE)
  
  #res <- res[1:20,]
  
  print(res)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
AgeOfNTitle <- function() {
  
  db <- removeTeamEvents(db)
  
  N <- '4'  

  ## drop walkover matches (not countable)
  #db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]
  
  #db <- db[surface == 'Clay']
  
  #db <- db[tourney_level == 'M']
  
  #db <- db[round == 'QF']
  
  #db <- db[winner_ioc == 'ITA']
  
  db <- db[loser_rank < 11]
  
  stat <- getanID(db, "winner_name")
  
  # #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 , 4)
  
  stat <- stat[.id == N]
  
  #stat$winner_age <- substr(stat$winner_age, 0, 5)
   
  #stat$winner_age <- gsub('\\.', ',', stat$winner_age)
  
  ## order by decreasing
  setorder(stat, winner_age, na.last = FALSE)
  
  #stat <- Formatwinner_age(stat)
  
  stat <-
    stat[, c(
      "tourney_name",
      "year",
      "round",
      "winner_name",
      "winner_age",
      ".id",
      "score"
    )]
}



#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
NWinsatAge <- function(){
  
  db <- removeTeamEvents(db)
  
  #db <- db[tourney_level == 'M']
  
  db <- db[surface == 'Hard']
  
  db <- db[round == 'SF']
  
  db <- db[winner_age < 25] 
  
  db <- db %>% add_count(winner_name)
  
  db <- unique(db[, c("winner_name","n")])
  
  #db <- db[winner_name == 'Novak Djokovic' | winner_name == 'Daniil Medvedev']
  
  setorder(db,-n, na.last = FALSE)
  
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
QualifiersinRound <- function(){
  
  stat <- db[(winner_seed == 'Q' | winner_entry == 'Q') & round == 'QF']
  
  # #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- stat[, .N, by = list(stat$tourney_name, stat$year)]
  
  stat <- setorder(stat, -N, na.last=FALSE)
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
RetirementsInASlam <- function(){
  stat <- db[grepl("RET", db$score) & tourney_level == 'G']
  
  ## count occurrences of won matches
  stat <- stat[,.N, by=tourney_id]
  
  
  print(stat)
  
  officialName <- unique(db[,c('tourney_id', 'tourney_name')])
  
  stat <- merge(officialName, stat, by="tourney_id")
  
  ## order by decreasing age
  #same <- same[order(-N)]
  
  # #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- setorder(stat, N, na.last=FALSE)
  
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
AgeWhoWillMakeARound <- function(){
  
  db <- removeTeamEvents(db)
  
  db <- db[round == 'F']
  
  #db <- db[winner_ioc == 'ITA']
  
  stat <- getanID(db, "winner_name")
  
  # #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 , 4)
  
  stat <- stat[.id == 1]
  
  stat$winner_age <- substr(stat$winner_age, 0, 5)
  
  stat$winner_age <- gsub('\\.', ',', stat$winner_age) 
  
  ## order by decreasing
  setorder(stat, winner_age, na.last = FALSE)
  
  stat <-
    stat[, c(
      "tourney_name",
      "year",
      "surface",
      "round",
      "winner_name",
      "winner_age",
      ".id",
      "score"
    )]
  stat <- stat[,c("tourney_name", "year", "winner_name", "winner_age")]
  
  print(stat)
  
  res <- db[round == 'F' & tourney_level == 'G']
  res$year <- stringr::str_sub(res$tourney_id, 0 ,4)
  res <- res[,c("winner_name")]
  
  print(res)
  
  stat <- match_df(stat, res)
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
AllTournamentsRoll <- function(){
  
  player <- 'Rafael Nadal'
  
  #Big tournaments
  #db <- db[tourney_level == 'G' | tourney_level == 'M' | tourney_level == 'F']
  
  #db <- db[tourney_level == 'G']
  
  db <- removeTeamEvents(db)
  
  db <- db[winner_name == player | loser_name == player]
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  db <- distinct(db, tourney_id, .keep_all = TRUE)
  
  print(db)
  
  tour <- dplyr::pull(db, tourney_id)
  
  tourney_name <- dplyr::pull(db, tourney_name)
  
  stat <-  PercentageTour(tour[1])
  
  stat <- stat[name == player]
  
  for (i in 2:(length(tour))) {
    
    stat2 <-  PercentageTour(tour[i])
    
    stat2 <- stat2[name == player]
    
    stat <- rbind(stat, stat2, fill = TRUE)
  }

  stat <- add_column(stat, tourney_name, .after = "name")
  
  stat <- setorder(stat, -percentage, na.last=FALSE)
  
  print(stat)
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
ConsecutiveWon1stSetWins <- function(){
  
  res <- WinsOverall()
  
  res <- res[N > 1200]
  
  players <- dplyr::pull(res, winner_name)
  
  stat <-  won1stWins(players[1])
  
  for (i in 2:(length(players))) {
    
    print(players[i])
    
    stat2 <-  won1stWins(players[i])
    
    stat <- rbind(stat, stat2)
  }
  stat <- as.data.frame(stat)
  
  ## order by decreasing
  setorder(stat, -N, na.last=FALSE)
  
  # #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- stat[,c("name", "N", "tourney_name", "year", "round", "winner_name", "loser_name", "score")]
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
won1stWins <- function(player){
  
  wins <- db[(winner_name == player | loser_name == player)]
  
  ## drop walkover matches (not countable)
  wins <- wins[!wins$score=="W/O" & !wins$score=="DEF" & !str_detect(wins$score, "WEA") & !str_detect(wins$score, "ABN") & !str_detect(wins$score, "NA")]
  
  library(foreach)
  foreach(i = 1:length(wins$score)) %do%
    {
      #split to catch the sets
      set <- strsplit(wins$score[i], " ")
      
      foreach(k = 1:length(set)) %do%
        {
          score <- strsplit(set[[k]], "-")
        }
      
      #sub for tiebreaks
      score[[1]][2] <-  sub("\\(.*", "", score[[1]][2])
      
      score[[1]][2][is.na(score[[1]][2])] <- 0
      
      if (score[[1]][1] > score[[1]][2])
        wins$won1st[i] <- '1st win'
      else
        wins$won1st[i] <- 'no 1st win'
    }
  
  wins <- wins[(won1st == '1st win' & winner_name == player) | (won1st == 'no 1st win' & loser_name == player)]
  
  wins1setWon <-  Streaks(wins, win=TRUE, cutoff=2, breaks=FALSE)
  
  wins1setWon$info <- head(wins1setWon$info, 1)
  
  wins1setWon <- cbind(wins1setWon$info, wins1setWon$matches[[1]])
  
  wins1setWonfirst <- head(wins1setWon, 1)
  
  wins1setWonlast <- tail(wins1setWon, 1)
  
  wins1setWon <- rbind(wins1setWonfirst, wins1setWonlast)
  
  print(wins1setWon)
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
Beat123intheSametournament <- function(){
  
  stat <- db[loser_rank < 4]
  
  require(dplyr)
  stat <- stat[, .N, by = list(tourney_id, winner_name)]
  
  stat <- stat[N == 3]
  
  officialName <- unique(db[,c('tourney_id', 'tourney_name')])
  stat <- right_join(officialName, stat, by="tourney_id")
  
  # #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- stat[,c("tourney_name", "year", "winner_name", "N")]
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
OldestPlayerstoWin1stTitle <- function(){
  
  #Remove or add team events matches
  db <- removeTeamEvents(db)
  
  stat <- db[round == 'F']
  
  stat <- getanID(stat, "winner_name")
  
  stat <- stat[.id == 1]
  
  #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  ## order by decreasing
  setorder(stat, -winner_age, na.last=FALSE)
  
  stat$winner_age <- substr(stat$winner_age, 0, 5)
  stat$winner_age <- suppressWarnings(as.numeric(str_replace_all(stat$winner_age,pattern=',',replacement='.')))
  
  stat <- stat[,c("tourney_name", "year", "winner_name", "winner_age")]
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
YoungestToWinAMatchinSlams <- function(){
  
  stat <- db[tourney_level == 'G']
  
  #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  ## order by decreasing
  setorder(stat, winner_age, na.last=FALSE)
  
  stat[is.na(stat$winner_age)] <- 0
  
  stat <- getanID(stat, "winner_name")
  
  stat <- stat[.id == 1]
  
  stat$dec <- stat$winner_age - floor(stat$winner_age)
  
  stat$dec <- stat$dec * 365.25
  
  stat$winner_age <- as.integer(stat$winner_age)
  
  stat$dec <- as.integer(stat$dec)
  
  stat$winner_age <- paste(stat$winner_age, "y", " ", stat$dec, "d", sep = "")
  
  stat <- stat[,c("tourney_name", "year", "round", "winner_name", "winner_age", "loser_name", "score")]
  
  print(stat)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
consecutiveSetsWon <- function(){

#db <- read.csv("Sets.csv", header = TRUE, sep = ',', fill = TRUE)
  
player <- 'Rafael Nadal'

#stat <- db[tourney_level == 'G']

stat <- db[surface == 'Clay']

stat <- stat[winner_name == player | loser_name == player]

## only select matches of a tournament
#db$tourney_id <- sub("^[^-]*", "", db$tourney_id)

#db <- db[tourney_id == '-580']

stat <- stat[!stat$score=="W/O" & !stat$score=="DEF" & !stat$score=="(ABN)" & !str_detect(stat$score, "WEA")]

stat$score <- gsub('RET', '', stat$score)

#split to catch the sets
stat$set <- strsplit(stat$score, " ")

#extract year from tourney_date
stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)

out2 <- NULL

stat <- stat[,c("tourney_name", "year", "winner_name", "loser_name", "set", "score")]

print(stat)

library(foreach)
foreach(i = 1:length(stat$set)) %do%
  {
    print(stat$year[i])

    score <- strsplit(stat$set[[i]], "-")

    #sub for tiebreaks
    if(str_detect(score[[1]][2], "\\)"))
    score[[1]][2] <-  sub("\\(.*", "", score[[1]][2])

    score[[1]][2][is.na(score[[1]][2])] <- 0

    winner <-stat$winner_name[i]
    loser <- stat$loser_name[i]

    #count lost games
    foreach(j = 1:length(score)) %do%
      {
        if(score[[j]][2] > score[[j]][1]){
          stat$loser_name[i] <- winner
          stat$winner_name[i] <- loser
        }

        out2 <- rbind(out2, stat[i])

        #record <- stat[i][,c("winner_name", "loser_name")]
        ##write.table(record, file = "Sets.csv", append=TRUE, na="", quote=F, row.names = FALSE, col.names = FALSE, sep = ',')
        
        stat$winner_name[i] <- winner
        stat$loser_name[i] <- loser
      }
  }

#out2 <- out2[,c("winner_name", "loser_name")]

winsStreak <-  Streaks(out2, win=TRUE, cutoff=1, breaks=TRUE)

print(winsStreak$info)

info <- head(winsStreak$info, 19)

print(winsStreak$matches[[1]])

matches <- winsStreak$matches[[1]]
winsStreakfirst <- head(matches, 1)
winsStreaklast <- tail(matches, 1)
matches <- rbind(winsStreakfirst, winsStreaklast)
streak <- cbind(info[1,], matches)

for(i in 2:19){
  
print(info[i,])
  
print(winsStreak$matches[[i]])

matches <- winsStreak$matches[[i]]
winsStreakfirst <- head(matches, 1)
winsStreaklast <- tail(matches, 1)
matches <- rbind(winsStreakfirst, winsStreaklast)
streak2 <- cbind(info[i,], matches)

streak <- rbind(streak, streak2)
}

print(streak)
# winsStreakfirst <- head(winsStreak, 1)
# winsStreaklast <- tail(winsStreak, 1)
# winsStreak <- rbind(winsStreakfirst, winsStreaklast)

}



#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
SameNation4Semifinalists <- function() {

  #Remove or add team events matches
  db <- removeTeamEvents(db)
  
  #db$winner_rank <- as.integer(db$winner_rank)
  #db$loser_rank <- as.integer(db$loser_rank)
  
  stat <- db[round == 'QF']
  
  stat <- with(stat, aggregate(list(count = winner_ioc), list(tourney_id = tourney_id, winner_ioc=winner_ioc), length))
  
  res <- db[round == 'QF']
  officialName <- unique(res[, c('tourney_id', 'tourney_name', 'tourney_date', 'winner_name')])
  
  stat <- left_join(officialName, stat, by = "tourney_id")
  
  #stat <- distinct(stat, tourney_id)
  
  #stat <- getanID(stat, "winner_name")
  
  #stat <- stat[.id == 1]
  
  ## get rid of NAs, have 0 instead
  stat[is.na(stat)] <- 0
  
  stat$count <- as.integer(stat$count)
  
  stat <- filter(stat, count == '4')
  
  stat <- stat[!duplicated(stat$tourney_id),]
  
  # #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 , 4)
  
  stat <- stat[,c("tourney_name", "year", "winner_ioc")]
  
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
Top10inATournament <- function(){
  
  stat <- db[tourney_level == 'M']
  
  winners <- stat[winner_rank < 11]
  
  winners <- unique(winners[, c('winner_rank', 'tourney_id')])
  
  losers <- stat[loser_rank < 11]
  
  losers <- unique(losers[, c('loser_rank', 'tourney_id')])
  
  names(winners)[1] <- "rank"
  names(losers)[1] <- "rank"
  
  stat <- rbind(winners, losers, by = c("tourney_id"), fill = TRUE)
  
  stat <- unique(stat[,c('rank', 'tourney_id')])
  
  stat <- stat[, .N, by = list(tourney_id)]
  
  
  res <- db[round == 'F' & tourney_level == 'M']
  officialName <-
    unique(res[, c('tourney_id', 'tourney_name', 'winner_name')])
  
  stat <- join(officialName, stat, by = "tourney_id")
  
  ## order by decreasing
  setorder(stat, N, na.last=FALSE)
  
  #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- stat[,c("tourney_name", "year", "N")]
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
RoundinCategoryWithNoFormerChampions <- function() {
  
  #Select all Masters 1000 tournaments
  db <- db[tourney_level == 'M']
  
  #Select id from tournaments
  tour <- unique(dplyr::pull(db, tourney_id))
  
  #1st former champion
  stat <-  unique(db[tourney_id == tour[1] & round == 'F'])
  formerChampions <- unique(stat[, c('winner_name')])
  
  #print(stat)
  
  #print(formerChampions)
  
  #round with no former champions
  stat2 <- db[tourney_id == tour[2] & round == 'QF']
  
  #print(stat2)
  
  roundyNoFormerChampions <- subset(stat2, !(stat2$winner_name  %in% formerChampions$winner_name))
  
  #print(roundyNoFormerChampions)
  
  
  for (i in 2:length((tour))) {

    stat <-  unique(db[tourney_id == tour[i] & round == 'F'])
    formerChampions2 <- unique(stat[, c('winner_name')])
    formerChampions <-  rbind(formerChampions, formerChampions2, fill = TRUE)
    
    #round with no former champions
    j <- i + 1
    stat2 <- db[tourney_id == tour[j] & round == 'QF']
    roundyNoFormerChampions2 <- subset(stat2, !(stat2$winner_name  %in% formerChampions$winner_name))
    
    roundyNoFormerChampions <-  rbind(roundyNoFormerChampions2, roundyNoFormerChampions, fill = TRUE)
      
  }
  #extract year from tourney_date
  roundyNoFormerChampions$year <- stringr::str_sub(roundyNoFormerChampions$tourney_id, 0 ,4)
  
  roundyNoFormerChampions <- roundyNoFormerChampions[,c("tourney_name", "year", "round", "winner_name", "loser_name", "score")]
  
  require(dplyr)
  roundyNoFormerChampions<- roundyNoFormerChampions %>% 
    group_by(tourney_name, year) %>%
    tally()
  
  ## order by decreasing
  setorder(roundyNoFormerChampions, -n, na.last=FALSE)
  
  
  print(roundyNoFormerChampions)
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
TournamentsPlayedToReachARound <- function(){
  
  turn <- 'F'
  
  category <- 'M'
  
  stat <- db[tourney_level == category & round == turn]

  players <- unique(dplyr::pull(stat, winner_name))
  
  print(players[1])
  
  stat <- db[winner_name == players[1] & tourney_level == category & round == turn]
  stat <- head(stat, 1)
  
  stat2 <- db[(winner_name == players[1] | loser_name == players[1]) & tourney_level == category]
  
  stat2$loser_name <- players[1]
    
  stat2 <- unique(stat2[, c('tourney_id', 'loser_name')])
  
  stat2 <- getanID(stat2, "loser_name")
  
  toreach <- subset(stat2, (stat2$tourney_id  %in% stat$tourney_id))
  
  print(toreach)
  
  
  for (i in 2:length((players))) {

  print(players[i])

    stat <- db[winner_name == players[i] & tourney_level == category & round == turn]
    stat <- head(stat, 1)

    stat2 <- db[(winner_name == players[i] | loser_name == players[i]) & tourney_level == category]

    stat2$loser_name <- players[i]
    
    stat2 <- unique(stat2[, c('tourney_id', 'loser_name')])

    stat2 <- getanID(stat2, "loser_name")

    toreach2 <- subset(stat2, (stat2$tourney_id %in% stat$tourney_id))
    
    print(toreach2)

    toreach <- rbind(toreach2, toreach, fill = TRUE)

  }

  res <- db[round == 'F' & tourney_level == category]
  officialName <- unique(res[, c('tourney_id', 'tourney_name')])

  toreach <- right_join(officialName, toreach, by = "tourney_id")

  #extract year from tourney_date
  toreach$year <- stringr::str_sub(toreach$tourney_id, 0 ,4)

  toreach <- toreach[,c("loser_name", "tourney_name", "year", ".id")]

  ## order by decreasing
  setorder(toreach, .id, na.last=FALSE)

  #print(toreach)
}



#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
####Find total breaks in a match
Totalbreaks <- function(){
  
  db[is.na(db)] <- 0
  
  stat<- db
  
  #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- stat[, totalbreakpoints:= (as.integer(stat$w_bpFaced) - as.integer(stat$w_bpSaved)) + (as.integer(stat$l_bpFaced) - as.integer(stat$l_bpSaved))]
  
  stat <- stat[totalbreakpoints > 16]
  
  ## order by decreasing
  setorder(stat, -totalbreakpoints, na.last=FALSE)
  
  stat <- stat[,c("tourney_name", "year", "round", "winner_name", "loser_name", "score", "totalbreakpoints")]
  
  
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
WinningPercentageClayOnTotal <- function(){
  
  
  stat <- winsSurface('Hard')
  
  stat2 <- WinsOverall()
  
  stat2 <- subset(stat2, N > 299)
  
  stat <- merge(stat, stat2, by = c("winner_name"))
  
  stat[is.na(stat)] <- 0
  
  res <- stat[, percentage:=N.x/N.y*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  stat <- res
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
TournamentWonByABig3WillAllInDraw <- function(){
  
  Nadal <- db[winner_name == 'Rafael Nadal' | loser_name == 'Rafael Nadal']
  Nadal <- unique(Nadal[, c('tourney_id')])
  
  Djokovic <- db[winner_name == 'Novak Djokovic' | loser_name == 'Novak Djokovic']
  Djokovic <- unique(Djokovic[, c('tourney_id')])
  
  Federer <- db[winner_name == 'Roger Federer' | loser_name == 'Roger Federer']
  Federer <- unique(Federer[, c('tourney_id')])
  
  NadalAndDjokovic <- subset(Djokovic, (Djokovic$tourney_id  %in% Nadal$tourney_id))
  
  NadalAndDjokovicAndFederer <- subset(Federer, (Federer$tourney_id  %in% NadalAndDjokovic$tourney_id))
  
  
  NadalSF <- db[winner_name == 'Rafael Nadal' & round == 'F']
  NadalSF <- unique(NadalSF[, c('tourney_id')])
  
  DjokovicSF <- db[winner_name == 'Novak Djokovic' & round == 'F']
  DjokovicSF <- unique(DjokovicSF[, c('tourney_id')])
  
  FedererSF <- db[winner_name == 'Roger Federer' & round == 'F']
  FedererSF <- unique(FedererSF[, c('tourney_id')])
  
  NadalAndDjokovicAndFedererSF <- rbind(NadalSF, DjokovicSF, FedererSF)
  NadalAndDjokovicAndFedererSF <- unique(NadalAndDjokovicAndFedererSF[, c('tourney_id')])
  
  
  NoDjokovicNoNadalandFederer <- subset(NadalAndDjokovicAndFederer, (NadalAndDjokovicAndFederer$tourney_id  %in% NadalAndDjokovicAndFedererSF$tourney_id))
  
  res <- db[round == 'F']
  officialName <- unique(res[, c('tourney_id', 'tourney_name')])
  
  toreach <- right_join(officialName, NoDjokovicNoNadalandFederer, by = "tourney_id")
  
  #extract year from tourney_date
  toreach$year <- stringr::str_sub(toreach$tourney_id, 0 ,4)
  
  print(toreach)
  
  res <- db[round == 'F']
  officialName <- unique(res[, c('tourney_id', 'winner_name')])
  
  toreach <- right_join(officialName, toreach, by = "tourney_id")
  
  stat <- toreach[,c("tourney_name", "year" ,"winner_name")]
  
  ## order by decreasing
  setorder(stat, winner_name, na.last=FALSE)
  
}


TournamentsPlayedToReachNARound <- function(){
  
  db <- removeTeamEvents(db)
  
  turn <- 'QF'
  
  N <- 6 
  
  stat <- db[round == turn]
  stat <- getanID(stat, "winner_name")
  stat <- stat[.id == N]
  
  players <- unique(dplyr::pull(stat, winner_name))
  
  Onlyplayer <- stat[winner_name == players[1]]
  
  stat2 <- db[(winner_name == players[1] | loser_name == players[1])]
  stat2$loser_name <- players[1]
  stat2 <- unique(stat2[, c('tourney_id', 'loser_name')])
  stat2 <- getanID(stat2, "loser_name")
  
  toreach <- subset(stat2, (stat2$tourney_id %in% Onlyplayer$tourney_id))
  
  
  for (i in 2:length((players))) {

    print(players[i])

    Onlyplayer <- stat[winner_name == players[i]]
    
    stat2 <- db[(winner_name == players[i] | loser_name == players[i])]
    stat2$loser_name <- players[i]
    stat2 <- unique(stat2[, c('tourney_id', 'loser_name')])
    stat2 <- getanID(stat2, "loser_name")
    
    toreach2 <- subset(stat2, (stat2$tourney_id %in% Onlyplayer$tourney_id))

    toreach <- rbind(toreach2, toreach, fill = TRUE)

  }
  
  res <- db[round == 'F']
  officialName <- unique(res[, c('tourney_id', 'tourney_name')])
  
  toreach <- right_join(officialName, toreach, by = "tourney_id")
  
  #extract year from tourney_date
  toreach$year <- stringr::str_sub(toreach$tourney_id, 0 ,4)
  
  toreach <- toreach[,c("loser_name", "tourney_name", "year", ".id")]
  
  ## order by decreasing
  setorder(toreach, .id, na.last=FALSE)
  
  #print(toreach)
}

AllATPWinsBySurface <- function(){
  
  stat <-  WinsOverall()
  
  stat2 <- winsSurface('Hard')
  
  coll <- left_join(stat, stat2, by = "winner_name")
  
  stat3 <-  winsSurface('Grass')
  
  coll <- left_join(coll, stat3, by = "winner_name")
  
  stat4 <-  winsSurface('Carpet')
  
  coll <- left_join(coll, stat4, by = "winner_name")
  
  stat5 <-  winsSurface('Clay')
  
  coll <- left_join(coll, stat5, by = "winner_name")
  
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
####Find total breaks in a match
Totalbreakpoints <- function(){
  
  db[is.na(db)] <- 0
  
  stat<- db
  
  #stat <- subset(stat, best_of == 3)
  
  #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  stat <- stat[, totalbreakpoints:= as.integer(stat$w_bpFaced) + as.integer(stat$l_bpFaced)]
  
  stat <- stat[totalbreakpoints > 16]
  
  ## order by decreasing
  setorder(stat, -totalbreakpoints, na.last=FALSE)
  
  stat <- stat[,c("tourney_name", "year", "round", "winner_name", "loser_name", "score", "totalbreakpoints")]
  
}