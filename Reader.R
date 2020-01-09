### Functions to interrogate the databases of matches

library(data.table)
library(dplyr)
library(plyr)

file="C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/newdb3.csv"

#Function to read all database from csv
ReadData <- function(file) {
  require(data.table)
  
  ## read the data
  read_timing <- system.time(data <- data.table::fread(file, fill=TRUE)) ## removed fill=TRUE since I corrected the database!
  cat(paste(":: Read", nrow(data),"matches done in", round(read_timing[3], 5), "s\n"))
  return(data)
}