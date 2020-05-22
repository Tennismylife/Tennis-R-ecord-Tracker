library("xlsx")
library("dplyr")
library("stringr")
library(tableHTML)

#Main
source("Age.R")
source("AverageAge.R")
source("Counter.R")
source("CounterSeason.R")
source("Entries.R")
source("Example.R")
source("Least.R")
source("Ranking.R")
source("Reader.R")
source("Percentage.R")
source("Played.R")
source("Player.R")
source("Same.R")
source("Season.R")
source("Wins.R")
source("Timespan.R")
source("Nationality.R")

#install.packages('splitstackshape')
#Read database from csv
db <- ParallelReader()

# db <- db[tourney_level == 'G' & round == 'F']
# 
# 
# #extract year from tourney_date
# db$tourney_id <- stringr::str_sub(db$tourney_id, 0 ,4)
# 
# stat <- unique(db[,c("tourney_id","winner_name")])
# 
# library(plyr)
# stat <- ddply(stat, .(winner_name), nrow)
# 
# print(stat)
# ## order by decreasing
# setorder(stat, -V1, na.last=FALSE)

# #extract year from tourney_date

#player <- 'Novak Djokovic'
#db <- removeTeamEvents(db)

#player <- 'Novak Djokovic'
#stat <- db[(winner_name == 'Rafael Nadal' & loser_name == 'Roger Federer')  | (winner_name == 'Roger Federer' & loser_name == 'Rafael Nadal')]


stat <- LeastSetToWintour()

#stat <- stat[, winner_points:=w_1stWon + w_2ndWon + (l_svpt - l_1stWon - l_2ndWon)]

#stat <- stat[, loser_points:=l_1stWon + l_2ndWon + (w_svpt - w_1stWon - w_2ndWon)]


#stat <- stat[, diff:=winner_points - loser_points]


# #extract year from tourney_date
#stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)

#extract id from tourney_id
#stat$tourney_id <- sub("^[^-]*", "", stat$tourney_id)

#stat <- stat[tourney_id > 1989]

#stat[is.na(stat$minutes)] <- 0

#calculate sum by edition
#stat <- aggregate(stat$minutes, by = list(stat$tourney_id), FUN=sum)

#stat <- stat[,c("tourney_id", "tourney_name")]

#officialName <- unique(stat[,c('tourney_id', 'tourney_name')])
#officialName$tourney_id <- sub("^[^-]*", "", officialName$tourney_id)

#same <- left_join(officialName, stat, by="tourney_id")

## order by decreasing
#setorder(stat, minutes, na.last=FALSE)

#print(same)


#stat <- same[,c("winner_name", "tourney_name.x", "year")]

## order by decreasing
#stat <- setorder(stat, -winner_name, na.last=FALSE)

write_tableHTML(tableHTML(stat), file = paste("Test.html"))




