library("xlsx")
library("dplyr")
library("stringr")
library(tableHTML)
library(splitstackshape)

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

#if(FALSE){

############################################################# MOST WINS BY No SLAMMER #####################################################

MostWinsNoSlammer <- function() {
  res <- db[round == 'F' & tourney_level == 'M']
  res <- res[, c("winner_name")]
  
  wins <- WinsCategory('M')
  wins <- subset(wins,!(wins$winner_name %in% res$winner_name))
  
  print(wins)
}


############################################################# % WINS AGAINST TOP 10 IN SLAMS #####################################################

PercentageInSlamsVsTop10 <- function() {
  ## wins
  dbm <- db[loser_rank < 11]
  
  #dbm <- dbm[tourney_level =='G']
  
  dbm <-
    dbm[!dbm$score == "W/O" & !dbm$score == "DEF" &
          !dbm$score == "(ABN)"]
  
  wins <- dbm[, .N, by = winner_name]
  
  ## losses
  dbm <- db[winner_rank < 11]
  #dbm <- dbm[tourney_level =='G']
  dbm <-
    dbm[!dbm$score == "W/O" & !dbm$score == "DEF" &
          !dbm$score == "(ABN)"]
  losses <- dbm[, .N, by = loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all = TRUE)
  
  ## sum the wins and losses into a new column played
  res <- res[, played := wins + losses]
  
  res <- res[played > 19]
  
  res <- res[, percentage := wins / played * 100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <-
    suppressWarnings(as.numeric(
      str_replace_all(res$percentage, pattern = ',', replacement = '.')
    ))
  
  ## order by decreasing total matches
  setorder(res,-percentage)
  res <- res[1:30, ]
  print(res)
  
}



###################################################################### TIME TO WIN A SLAM ##################################################################

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
  
  timeoncourt <- left_join(officialName, timeoncourt, by = "tourney_id")
  
  timeoncourt <- arrange(timeoncourt, desc(minutes))
  
  print(timeoncourt)
}


##################################################################### VIRGIN H2H ###########################################################################

virginH2H <- function() {
  db <- db[!db$score == "W/O" & !db$score == "DEF" &
             !db$score == "(ABN)"]
  
  #db <- db[(winner_name == 'Roger Federer' & loser_name == 'Novak Djokovic') | winner_name == 'Novak Djokovic' & loser_name == 'Roger Federer']
  
  h2h <-  db[, c('winner_name', 'loser_name', 'tourney_id')]
  
  # #extract year from tourney_date
  h2h$tourney_id <- stringr::str_sub(h2h$tourney_id, 0 ,4)
  
  h2h <- h2h[tourney_id > 1999]
  
  #h2h <- h2h[winner_name == 'Roger Federer' | loser_name == 'Roger Federer']
  
  h2h$match <- paste(h2h$winner_name, "-", h2h$loser_name, "-", h2h$tourney_id)
  
  h2h$reverse <- paste(h2h$loser_name, "-", h2h$winner_name, "-", h2h$tourney_id)
  
  
  match <- h2h[,c("match")]
  reverse <- h2h[,c("reverse")]
  
  total <- mapply(c, match, reverse, SIMPLIFY=FALSE)
  
  #total<- total[!is.na(total)]
  
  print(typeof(total))
  
  #total <- rbindlist(total)
  
  print(typeof(total))
  
  total <- setDT(total)
  
  total <- total[,.N, by=match]

  ## order by decreasing
  setorder(total, -N, na.last=FALSE)
  
  #total <- ddply(total, .(total), nrow)
  
  
  print(total)
}

##################################################################### Percentage As Number 1 ###########################################################################

PercentageAsNumber1 <- function() {
  dbm <- db
  
  dbm <-
    dbm[!dbm$score == "W/O" &
          !dbm$score == "DEF" & !dbm$score == "(ABN)" & !dbm$score == "ABN"]
  
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
  res <- res[, percentage := (wins / played)*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  res$percentage <- paste(res$percentage, "%")

  ## order by decreasing total matches
  setorder(res,-percentage)
  #res <- res[1:1,]
  print(res)
  
}

##################################################################### Wins Against #1 ###########################################################################

WinsAgainstNumber1 <- function() {
  dbm <- db
  
  dbm <- dbm[loser_name == 'Roger Federer' & surface == 'Grass']
  
  dbm <-
    dbm[!dbm$score == "W/O" &
          !dbm$score == "DEF" & !dbm$score == "(ABN)" & !dbm$score == "ABN"]
  
  ## ranking #1
  dbm <- dbm[loser_rank == '1']
  
  losses <- dbm[, .N, by = winner_name]
  
  ## order by decreasing total matches
  setorder(losses,-N)
  
  losses <- losses[1:20, ]
  
  print(losses)
  
}


##################################################################### MOST ACES IN A TOURNAMENT ###########################################################################


MostAcesinTour <- function() {
  dbm <- db
  
  dbm <-
    dbm[!dbm$score == "W/O" &
          !dbm$score == "DEF" & !dbm$score == "(ABN)" & !dbm$score == "ABN"]
  
  db <- db[tourney_id == '2020-8888']
  
  
  winner_aces <- db[, c("winner_name", "w_ace")]
  
  loser_aces <- db[, c("loser_name", "l_ace")]
  
  names(winner_aces)[1] <- names(loser_aces)[1] <- "name"
  names(winner_aces)[2] <- names(loser_aces)[2] <- "aces"
  
  res <- union(winner_aces,
               loser_aces,
               by = c("name"),
               all = TRUE)
  
  res[is.na(res)] <- 0
  
  totalAces <-
    aggregate(as.numeric(res$aces),
              by = list(name = res$name),
              FUN = sum)
  
  setorder(totalAces,-x)
  
  print(totalAces)
  
}

##################################################################### AVERAGE AGE IN ROUND ON A TOURNAMENT #################################################
averageAgeRoundinTour <- function() {
  dbm <- db
  
  dbm <- dbm[round == 'F' & grepl("2019-", dbm$tourney_id)]
  
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
  
  setorder(average,-tourney_id)
  average <- average[1:100, ]
  
  print(average)
  
  
}

#################################################################################### WINNER IN A TOURNAMENT BY NATION #######################################
WinnerInATourByNation <- function() {
  db <- db[winner_ioc == 'ITA']
  
  db <- db[grepl("-580", db$tourney_id)]
  
  
  db <- unique(db[, c('winner_name')])
  
}

##################################################################################### MOST 5 SETTER #########################################################

Most5Setter <- function() {
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

###################################################################################### NO BIG 3 in SLAMS QFs ######################################################


NoBig3inQFs <- function() {
  db <- db[tourney_level == 'G']
  db <- db[round == 'R16']
  db <-
    db[winner_name == 'Rafael Nadal' |
         winner_name == 'Roger Federer' | winner_name == 'Novak Djokovic']
  
  #extract year from tourney_date
  db$tourney_id <- stringr::str_sub(db$tourney_id, 0 , 4)
  
  db <-  db[, c('tourney_name', 'tourney_id', 'winner_name')]
  
  print(db)
  
}

###################################################################################### TITLE BY NATION ######################################################


titlesByNation <- function() {
  db <- db[round == 'F']
  
  db <-
    db[score != 'ABN' &
         score != '(ABN)' & !str_detect(db$score, "(WEA)")]
  
  #db <- db[winner_ioc == 'ARG']
  
  db <- removeTeamEvents(db)
  
  res <- db[, .N, by = list(db$winner_ioc)]
  
  setorder(res,-N)
  
}

################################################################################## ROUNDS COLLECTED AT AGE ########################################################

RoundAtAge <- function(){
  
  db <- removeTeamEvents(db)
  
  db <- db[round == 'SF']
  
  db <- db[winner_age < 21.549]
  
  #db <- db[tourney_level == 'M']
  
  res <- db[, .N, by = list(db$winner_name)]
  
  setorder(res,-N)
  
  print(res)
}


################################################################################## ROUNDS COLLECTED AT AGE ########################################################



BeatSamePlayer <- function(){
  
  db <- db[loser_name == 'Novak Djokovic']
  
  res <- db[, .N, by = list(db$winner_name)]
  
  setorder(res,-N)
  
  print(res)
}



PercentageEntryWinsinCategory <-  function(){
  
  entry <- EntriesCategory("G")
  
  counter <- CountCategoryRound("G", "F")
  
  percentage <- merge(entry, counter, by = "name")
  
  res <- percentage[, percentage:=played/entries*100]
  
  res$percentage <- substr(res$percentage, 0, 7)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  return(res)
  
}



PercentageDrop1stSet <- function(){
  
  winnerlist <- db[,c("winner_name")]
  loserlist <- db[,c("loser_name")]
  
  names(winnerlist)[1] <- 'player'
  names(loserlist)[1] <- 'player'
  
  playerslist <- unique(union(winnerlist, loserlist, by = 'player'))
  
  #print(playerslist)
  
  wins <- db
  
  wins <- db[(winner_name == 'Roger Federer' | loser_name == 'Roger Federer') & round == 'F' & tourney_level == 'G']
  

  library(foreach)
  foreach(i = 1:length(wins$score)) %do%  
    {
      print(wins[i])
      #change walkover with 0 games
      wins$score[i] <- gsub('W/O', '0-0 0-0', wins$score[i])
      
      #count played sets
      #library(dplyr)
      #setnumber  <- str_count(wins$score[i], " ")
      
      #print(setnumber)
      
      #split to catch the sets
      set <- strsplit(wins$score[i], " ")
      
      
      foreach(k = 1:length(set)) %do%
        {
          score <- strsplit(set[[k]], "-")
        }
      
        #sub for tiebreaks
        score[[1]][2] <-  sub("\\(.*", "", score[[1]][2])
          
        score[[1]][2][is.na(score[[1]][2])] <- 0
          
        if(score[[1]][2] > score[[1]][1])
          wins$score[i]<- '1st drop'
        else 
          wins$score[i]<- 'no 1st drop'

      
    }
  
  print(wins)
}


PercentageSameSeasonbyPlayer <- function() {
  
  dbm <- db
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  
  dbm <- dbm[surface == 'Clay']
  
  #extract year from tourney_date
  dbm$tourney_id <- stringr::str_sub(dbm$tourney_id, 0 ,4)
  
  ## wins
  dbm1 <- dbm[winner_name == 'Dominic Thiem']
  wins <- dbm1[,.N, by=list(winner_name, tourney_id)]

  ## losses
  dbm1 <- dbm[loser_name == 'Dominic Thiem']
  losses <- dbm1[,.N, by=list(loser_name, tourney_id)]
  
  dbm2 <- dbm[round == 'F']
  trophy <- dbm2[,.N, by=list(winner_name, tourney_id)]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- names(trophy)[1] <- "name"
  names(wins)[3] <- "wins"
  names(losses)[3] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name", "tourney_id"), all.x=TRUE)
  
  ## merge the tables by "name"
  res <- merge(res, trophy, by = c("name", "tourney_id"), all.x=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
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
      
      res$percTot[i] <- totalwins/ (totalwins +totalLosses) * 100
      
      totalTitles <- totalTitles + res$N[i]
      res$totTitle[i] <- totalTitles
      
    }
  
  ## calculate winning percentage
  #res <- res[played > 50]
  
  res <- res[, percyear:=wins/played*100]
  
  res$percyear <- substr(res$percyear, 0, 5)
  res$percyear <- suppressWarnings(as.numeric(str_replace_all(res$percyear,pattern=',',replacement='.')))
  
  res$percyear <- paste(res$percyear, "%")
  
  
  res$percTot <- substr(res$percTot, 0, 5)
  res$percTot <- suppressWarnings(as.numeric(str_replace_all(res$percTot,pattern=',',replacement='.')))
  
  res$percTot <- paste(res$percTot, "%")
  
  
  names(res)[2] <- "year"
  
  ## order by decreasing total matches
  setorder(res, year)
  
  res <- res[,c("name", "year", "wins", "losses", "percyear", "N", "totalwins", "totLosses", "percTot", "totTitle")]
  
  
  #res <- res[1:100,]
  print(res)
  
}



MostEntriesNoTitle <- function(){
stat <- SameTournamentEntries()

stat <- stat[ Player == 'Roger Federer' | Player == 'Rafael Nadal' | Player == 'Novak Djokovic']

res <- db[round =='F']
res <- res[,c("tourney_name", "winner_name")]

names(res)[1] <- "Tournament"
names(res)[2] <- "Player"

print(res)

res <- res[,c("Tournament", "Player")]



stat <- anti_join(stat, res)
}


LIstTop10 <- function(){
wins <- db[winner_rank < 11]
losses <-  db[loser_rank < 11]

wins <- wins[,c('winner_name')]
losses <- losses[,c('loser_name')]

names(wins)[1] <- "name"
names(losses)[1] <- "name"

res <- merge(wins, losses, by = c("name"), allow.cartesian=TRUE)

stat<- unique(res[,c('name')])
}


Top10ToWinMasters <- function(){
  
  category <- 'G'
  
  stat <- db[tourney_level == category & loser_rank < 11]
  
  res <- db[round =='F' & tourney_level== category]
  res <- res[,c("tourney_id", "tourney_name", "tourney_date", "winner_name")]
  
  dbm <- db[tourney_level== category & loser_rank < 11]
  wins <- match_df(dbm, res)
  
  stat <- count(wins, "tourney_id")
  
  res <- db[round =='F' & tourney_level == category]
  officialName <- unique(res[,c('tourney_id', 'tourney_name', 'winner_name')])
  
  stat <- join(officialName, stat, by="tourney_id")
  
  #extract year from tourney_id
  stat$tourney_id <- stringr::str_sub(stat$tourney_id, 0 ,4)
  
  #stat <- stat[,c("tourney_id", "tourney_name")]
  
  ## order by decreasing
  setorder(stat, -freq, na.last=FALSE)
  

}
