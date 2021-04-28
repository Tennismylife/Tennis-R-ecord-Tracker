### Functions to interrogate the databases of matches

library(data.table)
library(dplyr)
library(plyr)

#Function to read all database from csv

ParallelReader <- function(){
  years  <- (1968:2021)
  atp <- "C:/Users/Andrea/Documents/GitHub/TML-Database/"
  wta <- "C:/Users/Andrea/Documents/GitHub/tennis_wta-master/wta_matches_"

  #wta <- "https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_"
  #atp <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_"
  
  files <- paste0(atp, years, ".csv")
  
  files <- append(files, "C:/Users/Andrea/Documents/GitHub/TML-Database/ongoing_tourneys.csv")
  
  #files <- append(files, "C:/Users/Andrea/Documents/GitHub/Tennis-R-ecord-Tracker.csv")
  
  #files <- 'C:/Users/Andrea/Documents/GitHub/Tennis-R-ecord-Tracker/Sets.csv'
  
  print(files)
  
  #create f, which is a list of data frames
  f <- lapply(files, function(m) df <- fread(m, na="", quote=F, fill = TRUE))
  
  #stick them all together with do.call-rbind
  f_combine <- do.call("rbind", f)
}
