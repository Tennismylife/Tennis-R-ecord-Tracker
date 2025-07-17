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
source("Same.R")
source("Season.R")
source("Wins.R")
source("Timespan.R")
source("Nationality.R")
source("Remover.R")
source("Losses.R")

#Read database from csv
db <- ParallelReaderATP()

category <- "G"
tour <- '-540'
surface <- c("Hard", "Clay", "Grass", "Carpet")
round <- c("0", "R64","R32", "R16", "QF", "SF" , "F", "W")

##################################################################### PLAYED ######################################################
stat<- PlayedCategory(category)
stat <- stat[1:20,]
#write.xlsx(stat, file = "PlayedTracker.xlsx", sheetName="PlayedCategory", append=FALSE)
write_tableHTML(tableHTML(stat), file = 'Data/Played/PlayedCategory.html')

stat<- PlayedTour(tour)
stat <- stat[1:20,]
#write.xlsx(stat, file = "PlayedTracker.xlsx", sheetName="PlayedTour", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Played/PlayedTour.html')

for (i in 1:length(surface)) {
  stat <-  PlayedSurface(surface[i])
  stat <- stat[1:20,]
  #write.xlsx(stat, file = "PlayedTracker.xlsx", sheetName="PlayedSurface", append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Played/Played",surface[i],".html"))
}

stat <- PlayedOverall()
stat <- stat[1:20,]
#write.xlsx(stat, file = "PlayedTracker.xlsx", sheetName="PlayedOverall", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Played/PlayedOverall.html')

#################################################################### WINS ##########################################################
stat <- WinsCategory(category)
stat <- stat[1:20,]
#write.xlsx(stat, file = "WinsTracker.xlsx", sheetName="WinsCategory", append=FALSE)
write_tableHTML(tableHTML(stat), file = 'Data/Wins/WinsCategory.html')

stat <- WinsTour(tour)
stat <- stat[1:20,]
#write.xlsx(stat, file = "WinsTracker.xlsx", sheetName="WinsTour", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Wins/WinsTour.html')

for (i in 1:length(surface)) {
  stat <- winsSurface(surface[i])
  stat <- stat[1:20,]
  #write.xlsx(stat, file = "WinsTracker.xlsx", sheetName="winsSurface", append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data//Wins/WinsSurface",surface[i],".html"))
}

stat <- WinsOverall()
stat <- stat[1:20,]
#write.xlsx(stat, file = "WinsTracker.xlsx", sheetName="WinsOverall", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Wins/WinsOverall.html')

stat <- LossesOverall()
stat <- stat[1:20,]
#write_tableHTML(tableHTML(stat), file = 'Data/LossesOverall.html')


################################################################### Entries Counter ###########################################
stat <- EntriesCategory(category)
stat <- stat[1:20,]
#write.xlsx(stat, file = "EntriesTracker.xlsx", sheetName="EntriesCategory", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Entries/EntriesCategory.html')

stat <- EntriesTournament(tour)
stat <- stat[1:20,]
#write.xlsx(stat, file = "EntriesTracker.xlsx", sheetName="EntriesTournament", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Entries/EntriesTournament.html')

for (i in 1:length(surface)) {
stat <- EntriesSurface(surface[i])
stat <- stat[1:20,]
#write.xlsx(stat, file = "EntriesTracker.xlsx", sheetName="EntriesSurface", append=TRUE)
write_tableHTML(tableHTML(stat), file = paste("Data//Entries/EntriesSurface",surface[i],".html"))
}

stat <- EntriesOverall()
stat <- stat[1:20,]
#write.xlsx(stat, file = "EntriesTracker.xlsx", sheetName="EntriesOverall", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Entries/EntriesOverall.html')

#################################################################################### TIMESPAN #############################################################################

library(foreach)
foreach(i=1:length(round)) %do%
{
  #Entries
  stat <- TimespanOverallEntry(round[i])
  stat <- stat[1:20,]
  #write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanOverallEntry"), append=FALSE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Timespan/TimespanOverall",round[i],".html"))
  
  for (j in 1:length(surface)) {
    stat <- TimespaSurfaceEntry(surface[j], round[i])
    stat <- stat[1:20,]
    #write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespaSurfaceEntry"), append=FALSE)
    write_tableHTML(tableHTML(stat), file = paste("Data/Timespan/TimespanSurface",surface[j],round[i],".html"))
  }
  
  stat <- TimespanCategoryEntry(category, round[i])
  stat <- stat[1:20,]
  #write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanCategoryEntry"), append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Timespan/TimespanCategoryEntry",round[i],".html"))
  
  stat <- TimespanTournamentEntry(tour, round[i])
  stat <- stat[1:20,]
  #write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanTournamentEntry"), append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Timespan/TimespanTournament",round[i],".html"))
}

#Wins
stat <- TimespanOverallWins()
stat <- stat[1:20,]
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanOverallWins"), append=TRUE) 
write_tableHTML(tableHTML(stat), file = 'Data/Timespan/TimespanOverallWins.html')

stat <- TimespanCategoryWins(category)
stat <- stat[1:20,]
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespaCategoryWins"), append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Timespan/TimespaCategoryWins.html')

for (j in 1:length(surface)) {
stat <- TimespaSurfaceWins(surface[j])
stat <- stat[1:20,]
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespaSurfaceWins"), append=TRUE)
write_tableHTML(tableHTML(stat), file = paste("Data/Timespan/TimespaSurface", surface[j], "Wins.html"))
}

stat <- TimespanTournamentWins(tour)
stat <- stat[1:20,]
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanTournamentWins"), append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Timespan/TimespanTournamentWins.html')

########################################################################################## ROUND CoUNTER ##################################################################


for (i in 1:length(round)) {
  stat <- CountOverallRound(round[i])
  stat <- stat[1:20,]
  #write.xlsx(stat, file = "CounterTracker.xlsx", sheetName=paste("Overall",round[i]), append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Count/CountOverall",round[i],".html"))
  
  stat <- CountTourRound(tour, round[i])
  stat <- stat[1:20,]
  #write.xlsx(stat, file = "CounterTracker.xlsx", sheetName=paste("Tour",round[i]), append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Count/CountTour",round[i],".html"))
  
  for (j in 1:length(surface)) {
    stat <- CountSurfaceRound(surface[j], round[i])
    stat <- stat[1:20,]
    #write.xlsx(stat, file = "CounterTracker.xlsx", sheetName=paste("Surf",round[i]), append=TRUE)
    write_tableHTML(tableHTML(stat), file = paste("Data/Count/CountSurface",surface[j],round[i],".html"))
  }
  
  stat <- CountCategoryRound(category, round[i])
  stat <- stat[1:20,]
  #write.xlsx(stat, file = "CounterTracker.xlsx", sheetName=paste("Cat",round[i]), append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Count/CountCategory",round[i],".html"))
}

############################################################################### OLDEST ######################################################################################
for (i in 1:length(round)) 
{
  stat <- EntryOverallByAge('oldest', round[i])
  stat <- stat[1:20,]
  write_tableHTML(tableHTML(stat), file = paste("Data/Oldest/OldestEntrieOverallByAge",round[i],".html"))
  
  stat <- EntrycategoryByAge(category, 'oldest', round[i])
  stat <- stat[1:100,]
  write_tableHTML(tableHTML(stat), file = paste("Data/Oldest/OldestEntriecategoryByAge",round[i],".html"))
  
  for (j in 1:length(surface)) {
  stat <- EntrySurfaceByAge(surface[j], 'oldest', round[i])
  stat <- stat[1:20,]
  write_tableHTML(tableHTML(stat), file = paste("Data/Oldest/OldestEntrieSurfaceByAge",surface[j],round[i],".html"))
  }
  
  stat <-EntryTourByAge(tour, 'oldest', round[i])
  stat <- stat[1:20,]
  write_tableHTML(tableHTML(stat), file = paste("Data/Oldest/OldestEntrieTourByAge",round[i],".html"))
}

############################################################################### YOUNGEST ######################################################################################
for (i in 1:length(round)) 
{
  stat <- EntryOverallByAge('youngest', round[i])
  stat <- stat[1:20,]
  write_tableHTML(tableHTML(stat), file = paste("Data/Youngest/YoungestEntrieOverallByAge",round[i],".html"))
  
  stat <- EntrycategoryByAge(category, 'youngest', round[i])
  stat <- stat[1:20,]
  write_tableHTML(tableHTML(stat), file = paste("Data/Youngest/YoungestEntriecategoryByAge",round[i],".html"))
  
  for (j in 1:length(surface)) {
  stat <- EntrySurfaceByAge(surface[j], 'youngest', round[i])
  stat <- stat[1:120,]
  write_tableHTML(tableHTML(stat), file = paste("Data/Youngest/YoungestEntrieSurfaceByAge",surface[j],round[i],".html"))
  }
  
  stat <-EntryTourByAge(tour, 'youngest', round[i])
  stat <- stat[1:20,]
  write_tableHTML(tableHTML(stat), file = paste("Data/Youngest/YoungestEntrieTourByAge",round[i],".html"))
}

##################################################################################### AVERAGE AGE #################################################################################
for (i in 1:length(round)) 
{
  stat <- AverageAgeTour(tour, round[i])
  write_tableHTML(tableHTML(stat), file = paste("Data/Average/AverageAgeTour",round[i],".html"))
}

#################################################################################### PERCENTAGE #############################################################

stat <- PercentageOverall()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageOverall.html')

for (j in 1:length(surface)) {
stat <- PercentageSurface(surface[j])
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = paste("Data/Percentage/PercentageSurface", surface[j], ".html"))
}

stat <- PercentageCategory(category)
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageCategory.html')

stat <- PercentageTour(tour)
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageTour.html')


##################################################################################### SAME ##################################################################
for (i in 1:length(round)) 
{
  stat <- SameTournamentRound(round[i])
  stat <- stat[1:20,]
  write_tableHTML(tableHTML(stat), file = paste("Data/SameTournament/SameTournamentRound",round[i],".html"))
}

for (i in 1:length(round)) 
{
  stat <- SameSurfaceRound(round[i])
  stat <- stat[1:20,]
  write_tableHTML(tableHTML(stat), file = paste("Data/SameSurface/SameSurfaceRound",round[i],".html"))
}

stat <- SameTournamentPlayed()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = paste("Data/SameTournament/SameTournamentPlayed.html"))

stat <- SameTournamentWins()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = paste("Data/SameTournament/SameTournamentWins.html"))

stat <- SameTournamentEntries()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = paste("Data/SameTournament/SameTournamentEntries.html"))

stat <-SameSurfaceEntries()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = 'Data/SameSurface/SameSurfaceEntries.html')

stat <- SameSurfacePlayed()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = paste("Data/SameSurface/SameSurfacePlayed.html"))

stat <- SameSurfaceWins()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = paste("Data/SameSurface/SameSurfaceWins.html"))

stat <- SameSeasonPlayed()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = paste("Data/SameSeason/SameSeasonPlayed.html"))

stat <-PercentageSameSurface()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageSameSurface.html')

stat <-PercentageSameTour()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageSameTour.html')


##################################################################################### SEASON #####################################################################################
for (i in 1:length(round)) 
{
  stat <- SameSeasonRound(round[i])
  stat <- stat[1:20,]
  write_tableHTML(tableHTML(stat), file = paste("Data/SameSeason/SameSeasonRound",round[i],".html"))
}

stat <- EntriesSeason()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = 'Data/SameSeason/SameSeasonEntries.html')

stat <- SameSeasonWins()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = 'Data/SameSeason/SameSeasonWins.html')

stat <- PercentageSameSeason()
stat <- stat[1:20,]
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageSameSeason.html')

#stat <- titlesSeason()

################################################################################ COUnT IN A SEASON #########################################################################à###

#stat <- CountRoundSeason()

#stat <- LowestRankingRound('QF')

