library(tableHTML)

#Main
source("Aces.R")
source("Age.R")
source("Bagel.R")
source("AverageAge.R")
source("Consecutive.R")
source("Counter.R")
source("CounterSeason.R")
source("Entries.R")
source("Example.R")
source("H2H.R")
source("Least.R")
source("Length.R")
source("Ranking.R")
source("Reader.R")
source("Remover.R")
source("Percentage.R")
source("Played.R")
source("Same.R")
source("Season.R")
source("Wins.R")
source("Timespan.R")
source("Nationality.R")
source("Tiebreak.R")

#Read database from csv
db <- ParallelReader()

#player <- 'Novak Djokovic'

#Remove or add team events matches
#db <- removeTeamEvents(db)

#db$winner_rank <- as.integer(db$winner_rank)
#db$loser_rank <- as.integer(db$loser_rank)

## get rid of NAs, have 0 instead
#db[is.na(db)] <- 0

stat <- SeasonPercentage()

#stat <- db[round == 'SF' & tourney_level == 'M']

#stat <- db[winner_name == 'Roger Federer' & tourney_level == 'M' & round == 'SF']

#stat <- db[(winner_name == 'Roger Federer' | loser_name == 'Roger Federer') & tourney_level == 'M']

#stat <- db[loser_rank == 1]

## only select matches of a tournament
#stat$id <- sub("^[^-]*", "", stat$tourney_id)

#stat <- stat[id == '-410']

#stat <- getanID(stat, "winner_name")

#stat <- stat[.id == 1]

## drop walkover matches (not countable)
#stat <- stat[!stat$score=="W/O" & !stat$score=="DEF" & !str_detect(stat$score, "WEA") & !str_detect(stat$score, "ABN")]

#extract year from tourney_date
#stat$year <- stringr::str_sub(stat$tourney_id, 0 ,4)

# require(dplyr)
# stat<- stat %>%
#   group_by(year) %>%
#   tally()

#stat$winner_age<- suppressWarnings(as.numeric(str_replace_all(stat$winner_age,pattern=',',replacement='.')))
#stat$winner_age <- substr(stat$winner_age, 0, 5)

## order by decreasing
#setorder(stat, -w_bpFaced, na.last=FALSE)

#stat <- Formatwinner_age(stat)

#set range years
#stat <- stat[year == 1982]

#stat <- stat[,c("tourney_name", "year", "round", "winner_name", "loser_name", "score")]

write_tableHTML(tableHTML(stat), file = paste("Test.html"))
