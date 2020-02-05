### Functions to interrogate the databases of matches

library(data.table)
library(dplyr)
library(plyr)

#Function to read all database from csv

ParallelReader <- function(){
  years  <- (1968:2020)
  
  files <- paste0("https://raw.githubusercontent.com/Tennismylife/TML-Database/master/", years, ".csv")
  
  print(files)
  
  #create f, which is a list of data frames
  f <- lapply(files, function(m) df <- fread(m, na="", quote=F, fill = TRUE))
  
  #stick them all together with do.call-rbind
  f_combine <- do.call("rbind", f)
}
