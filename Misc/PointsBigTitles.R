library(tableHTML)


source("Reader.R")
source("Wins.R")

#Read database from csv
db <- ParallelReaderATP()

db$winner_rank <- as.integer(db$winner_rank)
db$loser_rank <- as.integer(db$loser_rank)

## get rid of NAs, have 0 instead
db[is.na(db)] <- 0

## only select matches of a tournament
db <- db[tourney_level == 'M']
db$id <- sub("^[^-]*", "", db$tourney_id)
tour <- unique(dplyr::pull(db, id))

stat3 <- NULL

for (j in 1:length(tour)) {
  
  players <- WinsTour(tour[j])
  players <- unique(dplyr::pull(players, winner_name))
  players <- head(players, 100)
  
  stat <- NULL
  
  for(i in 1:length(players)){
    
    print(paste0(tour[j], " ", players[i]))
    
    losses <- db[str_detect(db$tourney_id, tour[j]) & (loser_name==players[i] ),]
    wins <- db[str_detect(db$tourney_id, tour[j]) & (winner_name==players[i] ) & round=="F",]
    
    #points <- c(R128=10, R64=45, R32=90, R16=180, QF=360, SF=720, F=1200, W=2000)
    
    points <- c(R128=10, R64=10, R32=45, R16=90, QF=180, SF=360, F=600, W=1000)
    
    points[match(losses$round , names(points))]
    
    losses[, points:=points[match(losses$round , names(points))] ]
    
    wins[,points:=2000]
    
    tots <- rbind(losses, wins)
    
    stat2 <- tots[ , .(points=sum(points), .N ), by=.(tourney_name)]
    
    stat2 <- cbind(stat2, players[i])
    
    stat <- rbind(stat2, stat)
  }
  
  setorder(stat, -points)
  
  stat3 <- rbind(stat3, stat)
  
}
stat <- stat3[, c("tourney_name","V2", "points", "N")]


write_tableHTML(tableHTML(stat), file = paste("Test.html"))