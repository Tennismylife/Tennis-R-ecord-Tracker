#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
MostRecurrentH2HinASeason <- function() {
  
  db <- db[!db$score == "W/O" & !db$score == "DEF" & !db$score == "(ABN)"]
  
  #db <- db[(winner_name == 'Roger Federer' & loser_name == 'Novak Djokovic') | winner_name == 'Novak Djokovic' & loser_name == 'Roger Federer']
  
  h2h <-  db[, c('winner_name', 'loser_name', 'tourney_id')]
  
  # #extract year from tourney_date
  h2h$year <- stringr::str_sub(h2h$tourney_id, 0 , 4)
  
  #h2h <- h2h[year > 1999]
  
  #h2h <- h2h[winner_name == 'Roger Federer' | loser_name == 'Roger Federer']
  
  h2h$match <-
    paste(h2h$winner_name, "-", h2h$loser_name, "-", h2h$year)
  
  h2h$reverse <-
    paste(h2h$loser_name, "-", h2h$winner_name, "-", h2h$year)
  
  match <- h2h[, c("match")]
  
  reverse <- h2h[, c("reverse")]
  
  total <- mapply(c, match, reverse, SIMPLIFY = FALSE)
  
  library (plyr)
  total <- ldply (total, data.frame)
  
  names(total)[2] <- "match"
  
  total <- ddply(total, .(total$match), nrow)
  
  ## order by decreasing
  setorder(total,-V1, na.last = FALSE)
  
  names(total)[2] <- "N"
  names(total)[1] <- "match"
  
  total <-subset(total, N > 5)
  
  library(splitstackshape)
  h2h <- cSplit(total, "match", " - ")
  
  total <- cbind(h2h, total)
  
  total <-
    total[, c(
      "match_1",
      "match_2",
      "match_3",
      "N"
    )]
  
  total <- total[!duplicated(t(apply(total, 1, sort))),]

  print(total)
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
virginH2H2 <- function() {
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  h2h <-  db[,c('winner_name','loser_name')]
  
  h2h$match <- paste(h2h$winner_name, "-", h2h$loser_name)
  
  h2h$reverse <- paste(h2h$loser_name, "-", h2h$winner_name)
  
  h2h <- h2h[!(h2h$reverse %in% h2h$match),]
  
  out <- h2h[,.N, by=match]
  
  out <- arrange(out, desc(out$N))
  
  out <-  out[grepl("Rafael Nadal", out$match)]
  
  #out <- out[1:50,]
  
  print(out)
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
H2HTimespan <- function(){
  
  #extract id from tourney_id
  #db$tourid <- sub("^[^-]*", "", db$tourney_id)
  
  #db <- db[tourid == '-96']
  
  ## only select tournaments in the previously defined pool
  dbm <- db
  
  
  dbm <- dbm %>%
    mutate(h2h = paste(pmin(winner_name, loser_name), 
                       pmax(winner_name, loser_name), sep= " - "))
  print(dbm)
  
  #tournaments won
  wins <- unique(dbm[,c('h2h','tourney_name','tourney_date', 'winner_age')])
  
  ## common name to merge with
  names(wins)[1] <- "name"
  names(wins)[4] <- "age"
  
  ## merge the tables by "name"
  res <- wins
  
  #transform date format
  res$tourney_date <- lubridate::ymd(res$tourney_date)
  
  #order by date in subgroup by player
  res<-res[order(res$name,res$tourney_date),]
  
  #Select first and last element in date by name
  firstandlastdate<- res[, .SD[c(1,.N)], by=name]
  
  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=name]
  
  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=name]
  
  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_name,lastdate$tourney_date)
  
  #rename columns
  names(timespan)[2] <- "first_tournament"
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_tournament"
  names(timespan)[6] <- "last_date"
  
  #erase age column
  timespan$age <- NULL
  
  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))
  
  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]
  
  #timespan <- timespan[1:100,]
  print(timespan)
}

#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
BeatSamePlayer <- function() {
  db <- db[loser_name == 'Novak Djokovic']
  
  res <- db[, .N, by = list(db$winner_name)]
  
  setorder(res, -N)
  
  print(res)
}