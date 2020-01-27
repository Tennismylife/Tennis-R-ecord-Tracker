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


#Read database from csv
db <- ReadData(file)

category <- "G"
surface <- c("Hard", "Clay", "Grass", "Carpet")
round <- c("0", "R32", "R16", "QF", "SF" , "F", "W")

stat <- CountRoundSeason()
write_tableHTML(tableHTML(stat), file = paste("Test.html"))