### Functions to interrogate the databases of matches

library(data.table)
library(dplyr)
library(plyr)

#Function to read all database from csv

ParallelReaderATP <- function(){
  
  library(lubridate)
  years  <- (1968:2025)
  
  atp <- "C:/Users/andre/TML-Database/"

  #atp <- "https://raw.githubusercontent.com/Tennismylife/TML-Database/master/"
  
  files <- paste0(atp, years, ".csv")
  
  files <- append(files, "C:/Users/andre/TML-Database/ongoing_tourneys.csv")
  
  #files <- append(files, "https://raw.githubusercontent.com/Tennismylife/TML-Database/master/ongoing_tourneys.csv")
  
  print(files)
  
  #create f, which is a list of data frames
  f <- lapply(files, function(m) df <- fread(m, na="", quote=F, fill = TRUE))
  
  #stick them all together with do.call-rbind
  f_combine <- do.call("rbind", f)
}

ParallelReaderPreOpen <- function(){
  
  library(lubridate)
  years  <- (1968:year(now()))
  
  atp <- "C:/Users/andre/TML-Database/"
  
  #atp <- "https://raw.githubusercontent.com/Tennismylife/TML-Database/master/"
  
  filesPreOpen <- "C:/Users/andre/TML-Database/atp_matches_amateur.csv"
  
  files <- paste0(atp, years, ".csv")
  
  files <- append(files, "C:/Users/andre/TML-Database/ongoing_tourneys.csv")
  
  files <- append(filesPreOpen, files)
  
  #files <- append(files, "https://raw.githubusercontent.com/Tennismylife/TML-Database/master/ongoing_tourneys.csv")
  
  print(files)
  
  #create f, which is a list of data frames
  f <- lapply(files, function(m) df <- fread(m, na="", quote=F, fill = TRUE))
  
  #stick them all together with do.call-rbind
  f_combine <- do.call("rbind", f)
}


ParallelReaderWTA <- function(){
  
  library(lubridate)
  years  <- (1968:2025)
  
  wta <- "C:/Users/andre/WTA/wta_matches_"
  
  #wta <- "https://raw.githubusercontent.com/Tennismylife/TML-Database/master/"
  
  #wta <- "https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_"
  
  files <- paste0(wta, years, ".csv")
  
  print(files)
  
  #create f, which is a list of data frames
  f <- lapply(files, function(m) df <- fread(m, na="", quote=F, fill = TRUE))
  
  #stick them all together with do.call-rbind
  f_combine <- do.call("rbind", f)
}


ParallelReaderATPChallenger <- function(){
  
  library(lubridate)
  years  <- (1978:year(now()))
  
  atp <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_qual_chall_"
  
  files <- paste0(atp, years, ".csv")
  
  print(files)
  
  #create f, which is a list of data frames
  f <- lapply(files, function(m) df <- fread(m, na="", quote=F, fill = TRUE))
  
  #stick them all together with do.call-rbind
  f_combine <- do.call("rbind", f)
}


ParallelReaderWTAAllTime <- function(){
  
  library(lubridate)
  years  <- (1923:year(now()))
  
  atp <- "https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_"
  
  files <- paste0(atp, years, ".csv")
  
  print(files)
  
  #create f, which is a list of data frames
  f <- lapply(files, function(m) df <- fread(m, na="", quote=F, fill = TRUE))
  
  #stick them all together with do.call-rbind
  f_combine <- do.call("rbind", f)
}
