library(tableHTML)
library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(NLP)
library(plyr)
library(xlsx)
library(splitstackshape)
library(lubridate)
library(foreach)
library(doParallel)
library(parallel)


source("1stSetWon.R")
source("Aces.R")
source("Age.R")
source("AtAge.R")
source("AverageAge.R")
source("Bagel.R")
source("BreakPoints.R")
source("Counter.R")
source("CounterSeason.R")
source("Entries.R")
source("Example.R")
source("Games.R")
source("H2H.R")
#source("Incomplete.R")
source("Least.R")
source("Length.R")
source("Lopside.R")
source("Losses.R")
source("Nationality.R")
source("Percentage/Percentage.R")
source("Played.R")
source("Ranking.R")
source("Reader.R")
source("Remover.R")
source("Retirement.R")
source("RoundsOnEntries.R")
source("RouteTo.R")
source("Same.R")
source("Season.R")
source("Tiebreak.R")
source("Timespan.R")
source("ToReach.R")
source("Wins.R")


#Read database from csv
db <- ParallelReaderATP()
# #extract year from tourney_date
db$year <- stringr::str_sub(db$tourney_id, 0 ,4)
#db <- subset(db, year >= 1991 & year <= 2022)
#db <- db[is.na(db$surface), ]
#print(righe_na)

#player <- 'Casper Ruud'

#Remove or add team events matches
#db <- removeTeamEvents(db)

#db$winner_rank <- as.integer(db$winner_rank)
#db$loser_rank <- as.integer(db$loser_rank)
#db$w_bpSaved <- as.integer(db$w_bpSaved)
#db$w_bpFaced <- as.integer(db$w_bpFaced)
#db$l_bpFaced <- as.integer(db$l_bpFaced)
#db$winner_seed <- as.integer(db$winner_seed)
#db$loser_seed <- as.integer(db$loser_seed)

## get rid of NAs, have 0 instead
#db[is.na(db)] <- 0

player <- 'Bjorn Borg'
id <- 'SL28'

#stat <- as.data.frame(unique(db$w_ace))

#stat <- db[winner_ioc == 'ITA' & round == 'QF' & tourney_level == 'G']

stat <- MinutesSpentRouteTo()

#stat <- db[round == 'F' & tourney_level == 'G' & winner_ioc == 'ITA']

#stat <- db[(winner_name == player & winner_id == id) | (loser_name == player & loser_id == id)]

#stat <- db[((winner_name == player  & winner_rank == 1) | (loser_name == player & loser_rank == 1))]

#stat <- db[is.na(round)]

#stat <- db[grepl("[a-zA-Z]", db$score), ]

#stat <- stat[tourney_level == 'G' & round == 'R128']

#stat <- WinsTour('-580')

#stat <- db[(str_count(db$score, "-") == '5' & best_of == 5) | (str_count(db$score, "-") == '3' & best_of == 3)]

#stat <- stat[winner_name == player | loser_name == player]
#stat <- db[winner_ht == 'NA']
 
# player <- 'Albert Ramos-Vinolas-Vinolas'
# 
# stat <- db[winner_name == player | loser_name == player]
#stat <- subset(db, winner_name == player | loser_name == player)

#setorder(stat, -.id, na.last=FALSE)
#stat <- stat[.id == 100]

#stat <- db[(winner_name == 'Roger Federer' | loser_name == 'Roger Federer') & round == 'F' & tourney_level == 'G']

#stat <- stat[, c("tourney_name", "year", "round", "winner_name", "loser_name")]

## drop walkover matches (not countable)
#require(stringr)
#db <- db[!db$score=="W/O" & !db$score=="DEF" & !str_detect(db$score, "WEA") & !str_detect(db$score, "ABN")]

#stat <- stat[tourney_level == 'G']

#stat <- db[round == 'QF' & winner_age < 20.44079398 & tourney_level == 'G']

## drop walkover matches (not countable)
#stat <- stat[!stat$score=="W/O" & !stat$score=="DEF" & !str_detect(stat$score, "WEA") & !str_detect(stat$score, "ABN")]

# stat <- stat %>%
#   group_by(tourney_id)  %>%
#    tally()

# res <- db[round == 'F']
# officialName <-unique(res[, c('tourney_id', 'tourney_name', 'tourney_date')])
# stat <- right_join(officialName, stat, by = "tourney_date")

 # require(dplyr)
 # stat <- stat %>%
 #  group_by(winner_name) %>%
 #   tally()

#res <- db[round == 'QF']
#officialName <- unique(res[, c('tourney_id', 'tourney_name')])

#stat <- right_join(officialName, stat, by = "tourney_id") 

#stat[is.na(stat)] <- 0

#stat$winner_age<- suppressWarnings(as.numeric(str_replace_all(stat$winner_age,pattern=',',replacement='.')))
#stat$winner_age <- substr(stat$winner_age, 0, 5)
#stat$mean_age <- gsub("\\.", ",", stat$mean_age)

## count occurrences of won matches
#stat <- stat[,.N, by=winner_name]

## sum the wins and losses into a new column played
#stat <- stat[, diff:=winner_age - loser_age]

#stat <- stat[, .(count = .N), by = .(winner_name)]

## order by decreasing
#setorder(stat, -count, na.last=FALSE)

#extract year from tourney_date
#stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)

#set range years
#stat <- subset(stat, year =='2022')

#stat <- stat[n == '2']
#stat <- FormatwinnerAge(stat)
#stat <- FormatLoserAge(stat)
#stat <- FormatAge(stat)

#stat <- stat[, c("tourney_name", "year", "surface", "round", "winner_name", "winner_rank", "loser_name", "loser_rank", "score")]

#stat <- stat[, c("tourney_name", "year", "surface", "round", "winner_name","loser_name", "score", "winner_age", "loser_age")]

write_tableHTML(tableHTML(stat), file = paste("Test.html"))

