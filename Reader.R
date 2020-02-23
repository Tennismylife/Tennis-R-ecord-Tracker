### Functions to interrogate the databases of matches

library(data.table)
library(dplyr)
library(plyr)

#Function to read all database from csv

ParallelReader <- function(){
  years  <- (1968:2020)
  
  files <- paste0("C:\\Users\\Andrea\\Documents\\GitHub\\TML-Database\\", years, ".csv")
  
  files <- append(files, "https://raw.githubusercontent.com/Tennismylife/TML-Database/master/ongoing_tourneys.csv")
  
  print(files)
  
  #create f, which is a list of data frames
  f <- lapply(files, function(m) df <- fread(m, na="", quote=F, fill = TRUE))
  
  #stick them all together with do.call-rbind
  f_combine <- do.call("rbind", f)
}
