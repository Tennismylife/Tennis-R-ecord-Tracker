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


source("Player.R")

#Read database from csv
db <- ReadData(file)

category <- "G"
surface <- "Hard"
round <- c("0", "R32", "R16", "QF", "SF" , "F", "W")

if(FALSE){
##################################################################### PLAYED ######################################################
stat<- PlayedCategory(category)
#write.xlsx(stat, file = "PlayedTracker.xlsx", sheetName="PlayedCategory", append=FALSE)
write_tableHTML(tableHTML(stat), file = 'Data/Played/PlayedCategory.html')

stat<- PlayedTour("Australian Open","Australian Open", "Australian Open")
#write.xlsx(stat, file = "PlayedTracker.xlsx", sheetName="PlayedTour", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Played/PlayedTour.html')

stat <-  PlayedSurface(surface)
#write.xlsx(stat, file = "PlayedTracker.xlsx", sheetName="PlayedSurface", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Played/PlayedSurface.html')

stat <- PlayedOverall()
#write.xlsx(stat, file = "PlayedTracker.xlsx", sheetName="PlayedOverall", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Played/PlayedOverall.html')

#################################################################### WINS ##########################################################
stat <- WinsCategory(category)
#write.xlsx(stat, file = "WinsTracker.xlsx", sheetName="WinsCategory", append=FALSE)
write_tableHTML(tableHTML(stat), file = 'Data/Wins/WinsCategory.html')

stat <- WinsTour("Australian Open","Australian Open", "Australian Open")
#write.xlsx(stat, file = "WinsTracker.xlsx", sheetName="WinsTour", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Wins/WinsTour.html')

stat <- winsSurface(surface)
#write.xlsx(stat, file = "WinsTracker.xlsx", sheetName="winsSurface", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Wins/WinsSurface.html')

stat <- WinsOverall()
#write.xlsx(stat, file = "WinsTracker.xlsx", sheetName="WinsOverall", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Wins/WinsOverall.html')

stat <- LossesOverall()
write_tableHTML(tableHTML(stat), file = 'Data/LossesOverall.html')


################################################################### Entries Counter ###########################################
stat <- EntriesCategory(category)
#write.xlsx(stat, file = "EntriesTracker.xlsx", sheetName="EntriesCategory", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Entries/EntriesCategory.html')

stat <- EntriesTournament("Australian Open","Australian Open", "Australian Open")
#write.xlsx(stat, file = "EntriesTracker.xlsx", sheetName="EntriesTournament", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Entries/EntriesTournament.html')

stat <- EntriesSurface(surface)
#write.xlsx(stat, file = "EntriesTracker.xlsx", sheetName="EntriesSurface", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Entries/EntriesSurface.html')

stat <- EntriesOverall()
#write.xlsx(stat, file = "EntriesTracker.xlsx", sheetName="EntriesOverall", append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Entries/EntriesOverall.html')

#################################################################################### TIMESPAN #############################################################################


for (i in 1:length(round)) {
#Entries
stat <- TimespanOverallEntry(round[i])
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanOverallEntry"), append=FALSE)
write_tableHTML(tableHTML(stat), file = paste("Data/Timespan/TimespanOverall",round[i],".html"))

stat <- TimespaSurfaceEntry(surface, round[i])
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespaSurfaceEntry"), append=FALSE)
write_tableHTML(tableHTML(stat), file = paste("Data/Timespan/TimespanSurface",round[i],".html"))

stat <- TimespanCategoryEntry(category, round[i])
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanCategoryEntry"), append=TRUE)
write_tableHTML(tableHTML(stat), file = paste("Data/Timespan/TimespanCategoryEntry",round[i],".html"))

stat <- TimespanTournamentEntry("Australian Open", "Australian Open", "Australian Open", round[i])
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanTournamentEntry"), append=TRUE)
write_tableHTML(tableHTML(stat), file = paste("Data/Timespan/TimespanTournament",round[i],".html"))
}

#Wins
stat <- TimespanOverallWins()
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanOverallWins"), append=TRUE) 
write_tableHTML(tableHTML(stat), file = 'Data/Timespan/TimespanOverallWins.html')

stat <- TimespaCategoryWins(category)
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespaCategoryWins"), append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Timespan/TimespaCategoryWins.html')

stat <- TimespaSurfaceWins(surface)
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespaSurfaceWins"), append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Timespan/TimespaSurfaceWins.html')

stat <- TimespanTournamentWins("Australian Open", "Australian Open", "Australian Open")
#write.xlsx(stat, file = "TimespanTracker.xlsx", sheetName=paste("TimespanTournamentWins"), append=TRUE)
write_tableHTML(tableHTML(stat), file = 'Data/Timespan/TimespanTournamentWins.html')

########################################################################################## ROUND CoUNTER ##################################################################


for (i in 1:length(round)) {
  stat <- CountOverallRound(round[i])
  #write.xlsx(stat, file = "CounterTracker.xlsx", sheetName=paste("Overall",round[i]), append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Count/CountOverall",round[i],".html"))
  
  stat <- CountTourRound("Australian Open", round[i])
  #write.xlsx(stat, file = "CounterTracker.xlsx", sheetName=paste("Tour",round[i]), append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Count/CountTour",round[i],".html"))
  
  stat <- CountSurfaceRound(surface, round[i])
  #write.xlsx(stat, file = "CounterTracker.xlsx", sheetName=paste("Surf",round[i]), append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Count/CountSurface",round[i],".html"))
  
  stat <- CountCategoryRound(category, round[i])
  #write.xlsx(stat, file = "CounterTracker.xlsx", sheetName=paste("Cat",round[i]), append=TRUE)
  write_tableHTML(tableHTML(stat), file = paste("Data/Count/CountCategory",round[i],".html"))
}

############################################################################### OLDEST ######################################################################################
for (i in 1:length(round)) 
{
stat <- EntrieOverallByAge('oldest', round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/Oldest/OldestEntrieOverallByAge",round[i],".html"))

stat <- EntriecategoryByAge(category, 'oldest', round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/Oldest/OldestEntriecategoryByAge",round[i],".html"))

stat <- EntrieSurfaceByAge(surface, 'oldest', round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/Oldest/OldestEntrieSurfaceByAge",round[i],".html"))

stat <-EntrieTourByAge("Australian Open", 'oldest', round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/Oldest/OldestEntrieTourByAge",round[i],".html"))
}

############################################################################### YOUNGEST ######################################################################################
for (i in 1:length(round)) 
{
stat <- EntrieOverallByAge('youngest', round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/Youngest/YoungestEntrieOverallByAge",round[i],".html"))

stat <- EntriecategoryByAge(category, 'youngest', round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/Youngest/YoungestEntriecategoryByAge",round[i],".html"))

stat <- EntrieSurfaceByAge(surface, 'youngest', round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/Youngest/YoungestEntrieSurfaceByAge",round[i],".html"))

stat <-EntrieTourByAge("Australian Open", 'youngest', round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/Youngest/YoungestEntrieTourByAge",round[i],".html"))
}

##################################################################################### AVERAGE AGE #################################################################################
for (i in 1:length(round)) 
{
stat <- AverageAgeTour('Australian Open','Australian Open-2', 'Australian Open', round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/Average/AverageAgeTour",round[i],".html"))
}

#################################################################################### PERCENTAGE #############################################################

stat <- PercentageOverall()
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageOVerall.html')

stat <- PercentageSurface(surface)
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageSurface.html')

stat <- PercentageCategory(category)
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageCategory.html')

stat <- PercentageTour("Australian Open")
write_tableHTML(tableHTML(stat), file = 'Data/Percentage/PercentageTour.html')


##################################################################################### SAME ##################################################################
for (i in 1:length(round)) 
{
stat <- SameTournamentRound(round[i])
write_tableHTML(tableHTML(stat), file = paste("Data/SameTournament/SameTournamentRound",round[i],".html"))
}

for (i in 1:length(round)) 
{
  stat <- SameSurfaceRound(round[i])
  write_tableHTML(tableHTML(stat), file = paste("Data/SameSurface/SameSurfaceRound",round[i],".html"))
}

for (i in 1:length(round)) 
{
  stat <- SameSurfaceRound(round[i])
  write_tableHTML(tableHTML(stat), file = paste("Data/SameSurface/SameSurfaceRound",round[i],".html"))
}


#stat <- SameTournamentEntries('0')

##################################################################################### SEASON #####################################################################################
#stat <- WinsSeason()

#stat <- titlesSeason()

#stat <- EntriesSeason()

################################################################################ COUnT IN A SEASON #########################################################################à###

#stat <- CountRoundSeason()

#stat <- LowestRankingRound('QF')

}
#############################################################################################################################################################################

#stat <-PlayedPlayer('Andre Agassi', '2000')
#write_tableHTML(tableHTML(stat), file = 'Test.html')


#stat <-PlayerStats('Andre Agassi')
#write_tableHTML(tableHTML(stat), file = 'Test.html')


stat <- SameSurfaceRound('W')
write_tableHTML(tableHTML(stat), file = 'Test.html')

#stat <-PlayerStats('Guillermo Vilas')
#write_tableHTML(tableHTML(stat), file = 'Test.html')
