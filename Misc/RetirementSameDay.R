library(tableHTML)
source("Reader.R")
source("Remover.R")

# read database from csv using parallel reader for atp
db <- ParallelReaderATP()

# extract year from tourney_id (first 4 characters)
db$year <- stringr::str_sub(db$tourney_id, 0, 4)
db$year <- as.numeric(db$year)

# remove team events from the data
db <- removeTeamEvents(db)

# read wta database
db_wta <- ParallelReaderWTA()
db_wta$year <- stringr::str_sub(db_wta$tourney_id, 0, 4)

# use wta data only for analysis
data <- db_wta

# filter data for finals (round == "F") where the score contains a retirement ("RET")
finals_retirement <- subset(data, round == "F" & grepl("RET", score))

# order the filtered matches by tournament date
finals_retirement <- finals_retirement[order(finals_retirement$tourney_date), ]

# calculate difference in days between consecutive tournament dates
finals_retirement$diff_days <- c(NA, diff(as.Date(finals_retirement$tourney_date)))

# print the filtered and processed results
print(finals_retirement)

# select relevant columns for output
finals_retirement <- finals_retirement[, c("tourney_name", "year", "tourney_date", "diff_days", "surface", "round", "winner_name", "loser_name", "score")]

# export the results to an html file
write_tableHTML(tableHTML(finals_retirement), file = paste("Test.html"))
