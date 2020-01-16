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

#Read database from csv
db <- ReadData(file)
category <- "G"
surface <- "Hard"
round <- c("0", "R32", "R16", "QF", "SF" , "F", "W")

#if(FALSE){

############################################################# MOST WINS BY No SLAMMER #####################################################

MostWinsNoSlammer <- function() {
res <- db[round =='F' & tourney_level == 'M']
res <- res[,c("winner_name")]

wins <- WinsCategory('G')
wins <- subset(wins, !(wins$winner_name %in% res$winner_name))

print(wins)
}


############################################################# % WINS AGAINST TOP 10 IN SLAMS #####################################################

PercentageInSlamsVsTop10 <- function() {
  ## wins
  dbm <- db[loser_rank < 11]
  #dbm <- dbm[tourney_level =='G']
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  wins <- dbm[,.N, by=winner_name]
  
  ## losses
  dbm <- db[winner_rank < 11]
  #dbm <- dbm[tourney_level =='G']
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)"]
  losses <- dbm[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  res <- res[played > 19]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  res <- res[1:30,]
  print(res)
  
}



###################################################################### TIME TO WIN A SLAM ##################################################################

TimeToWinSlam <- function() {
res <- db[round =='F' & tourney_level == 'G']
res <- res[,c("winner_name", "tourney_id", "tourney_name", "tourney_date", "winner_name")]

db <- db[tourney_level == 'G']
wins <- match_df(db, res)

#calculate sum by edition
timeoncourt <- aggregate(wins$minutes, by=list(tourney_id=wins$tourney_id), FUN=sum, na.rm=TRUE)

names(timeoncourt)[2] <- "minutes"

timeoncourt <- arrange(timeoncourt, minutes)

res <- db[round =='F' & tourney_level == 'G']
officialName <- unique(res[,c('tourney_id', 'tourney_name', 'tourney_date', 'winner_name')])

timeoncourt <- left_join(officialName, timeoncourt, by="tourney_id")

timeoncourt <- arrange(timeoncourt, desc(minutes))

print(timeoncourt)
}


###################################################################### LEAST GAME TO WIN A SLAM ##################################################################

LeastGameToWinSlam <- function() {
res <- db[round =='F' & tourney_level == 'G']
res <- res[,c("winner_name", "tourney_id", "tourney_name", "tourney_date", "winner_name")]

db <- db[tourney_level == 'G']
wins <- match_df(db, res)

#print(wins)
#print(length(wins$score))

for(i in 1:length(wins$score))
{
#print("Score")
#print(wins$score[i])  
  
wins$score[i] <- gsub('W/O', '0-0 0-0', wins$score[i])

set <- strsplit(wins$score[i], " ")

#print("SET")
#print(set)

for(k in 1:length(set))
{
score <- strsplit(set[[k]], "-")
}


#print("Score")
#print(score)

total <- 0
for (j in 1:length(score)) {
  score[[j]][2] <-  sub("\\(.*", "", score[[j]][2])
  #print(score[[j]][2])
  
  score[[j]][2][is.na(score[[j]][2])] <- 0
  total<-total+as.numeric(score[[j]][2])
}
#print("Total")
#print(total)

wins$score[i]<- unlist(total)
}

wins$score <- as.numeric(as.character(unlist(wins$score)))

#calculate sum by edition
lostgame <- aggregate(wins$score, by=list(tourney_id=wins$tourney_id), FUN=sum, na.rm=TRUE)

names(lostgames)[2] <- "games"

res <- db[round =='F' & tourney_level == 'G']
officialName <- unique(res[,c('tourney_id', 'tourney_name', 'winner_name')])

lostgame <- left_join(officialName, lostgame, by="tourney_id")

#extract year from tourney_id
lostgame$tourney_id <- stringr::str_sub(lostgame$tourney_id, 0 ,4)

lostgame <- lostgame[,c("tourney_name", "tourney_id", "winner_name", "x")]

lostgame <- arrange(lostgame, lostgame$x)

print(lostgame)

}
#}


##################################################################### VIRGIN H2H ###########################################################################

virginH2H <- function() {
db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
h2h <-  db[,c('winner_name','loser_name')]

h2h$match <- paste(h2h$winner_name, "-", h2h$loser_name)
h2h$reverse <- paste(h2h$loser_name, "-", h2h$winner_name)


#h2h <- h2h[!(h2h$reverse %in% h2h$match),]


out <- h2h[,.N, by=match]

out <- arrange(out, desc(out$N))

out <- out[1:30,]

print(out)
}

##################################################################### Percentage As Number 1 ###########################################################################

PercentageAsNumber1 <- function() {
  dbm <- db
  
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)" & !dbm$score=="ABN"]
  
  dbm$tourney_id <- stringr::str_sub(dbm$tourney_id, 0 ,4)
  #dbm <- dbm[tourney_id == year]
  
  ## wins
  dbm1 <- dbm[winner_rank =='1']
  wins <- dbm1[,.N, by=winner_name]
  
  ## losses
  dbm1 <- dbm[loser_rank =='1']
  losses <- dbm1[,.N, by=loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  res <- res[, percentage:=wins/played]
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  #res <- res[1:1,]
  print(res)

}

##################################################################### Wins Against #1 ###########################################################################

WinsAgainstNumber1 <- function() {
  dbm <- db
  
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)" & !dbm$score=="ABN"]
  
  ## ranking #1
  dbm <- dbm[loser_rank =='1']
  
  losses <- dbm[,.N, by=winner_name]
  
  ## order by decreasing total matches
  setorder(losses, -N)
  losses <- losses[1:20,]
  print(losses)
  
}


##################################################################### MOST ACES IN A YTOURNAMENT ###########################################################################


MostAcesinTour <- function() {
  
  dbm <- db
  
  dbm <- dbm[!dbm$score=="W/O" & !dbm$score=="DEF" & !dbm$score=="(ABN)" & !dbm$score=="ABN"]
  
  db <- db[tourney_id == '2020-8888']
  
  
  winner_aces <- db[,c("winner_name", "w_ace")]
  
  loser_aces <- db[,c("loser_name", "l_ace")]
  
  names(winner_aces)[1] <- names(loser_aces)[1] <- "name"
  names(winner_aces)[2] <- names(loser_aces)[2] <- "aces"
  
  res <- union(winner_aces, loser_aces, by = c("name"), all=TRUE)

  res[is.na(res)] <- 0
  
  totalAces <- aggregate(as.numeric(res$aces), by=list(name=res$name), FUN=sum)

  setorder(totalAces, -x)
  
  print(totalAces)
  
}


averageAgeRoundinTour <- function(){
  
  dbm <- db
  
  dbm <- dbm[round == 'SF' & grepl("2019", dbm$tourney_id)]
  
  wins <- dbm[,c('tourney_id', 'winner_age')]
  
  losses <- dbm[,c('tourney_id', 'loser_age')]
  
  names(wins)[2] <- "age"
  names(losses)[2] <- "age"
  
  res <- rbind(wins, losses)
  
  #setorder(res, -tourney_id)
  
  res[is.na(res)] <- 0
  
  officialName <- unique(dbm[,c('tourney_id', 'tourney_name')])
  
  res <- left_join(res, officialName, by="tourney_id")
  
  print(res)
  
  average <- aggregate(res$age, by=list(tourney_id=res$tourney_id, tourney_name=res$tourney_name), FUN=mean, na.rm=TRUE)
  
  average$x <- gsub("\\.", ",", average$x)
  
  setorder(average, -tourney_id)
  average <- average[1:100,]
  
  print(average)
  
  
}
