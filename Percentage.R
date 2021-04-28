PercentageOverall <- function() {

  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "WEA")]
  
  ## wins
  wins <- db[,.N, by=winner_name]
  
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 20]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  print(res)

}


PercentageSurface <- function(court) {
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "WEA")]
  
  db <- db[surface == court]
  
  ## wins
  wins <- db[,.N, by=winner_name]
  
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 20]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  print(res)
}


PercentageCategory <- function(category) {

  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "WEA")]
  
  db <- db[tourney_level == category]
  
  ## wins
  wins <- db[,.N, by=winner_name]
  
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  #res <- res[played > 19]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  print(res)
}

PercentageTour <- function(id) {
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "WEA")]
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  db <- db[tourney_id == id]
  
  ## wins
  wins <- db[,.N, by=winner_name]
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  #res <- res[played > 20]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  print(res)
}



PercentageSameSeason <- function() {
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "WEA")]
  
  #extract year from tourney_date
  db$tourney_id <- stringr::str_sub(db$tourney_id, 0 ,4)
  
  ## wins
  #db1 <- db[winner_name == 'Rafael Nadal']
  wins <- db[,.N, by=list(winner_name, tourney_id)]
  
  ## losses
  #db1 <- db[loser_name == 'Rafael Nadal']
  losses <- db[,.N, by=list(loser_name, tourney_id)]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[3] <- "wins"
  names(losses)[3] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name", "tourney_id"), allow.cartesian=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 50]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  names(res)[2] <- "year"
  
  print(res)
  
}


PercentageSameSurface <- function() {
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)"]
  
  ## wins
  wins <- db[,.N, by=list(winner_name, surface)]
  
  ## losses
  losses <- db[,.N, by=list(loser_name, surface)]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[3] <- "wins"
  names(losses)[3] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name", "surface"), allow.cartesian=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 50]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  res <- res[1:100,]
  
  print(res)
  
}

PercentageSameTour <- function() {
  
  db <- removeTeamEvents(db)
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "WEA")]
  
  #extract id from tourney
  db$tourney_id <- stringr::str_sub(db$tourney_id, 5 ,nchar(db$tourney_id))
  
  ## wins
  wins <- db[,.N, by=list(winner_name, tourney_id)]

  ## losses
  losses <- db[,.N, by=list(loser_name, tourney_id)]

  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[3] <- "wins"
  names(losses)[3] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, all = TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]

  ## calculate winning percentage
  res <- res[played > 19]
  
  res <- res[, percentage:=wins/played*100]
  
  #res <- res[percentage == 0]
  
  setorder(res, -percentage)
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))

  res$percentage <- paste(res$percentage, "%")
  
  
  #get the official tournament name
  officialName <- unique(db[,c('tourney_id', 'tourney_name')])
  officialName <-officialName %>%
    mutate_all(funs(str_replace(., "WCT", "")))
  officialName <-  officialName %>%
    arrange(tourney_id, tourney_name) %>% #Arranging according ID and NUM
    group_by(tourney_id) %>% #Grouping by ID
    summarise_all(funs(last(.))) #Selecting the last rows of all variables
  
  print(officialName)
  
  res <- right_join(officialName, res, by="tourney_id")
  
  res <- res[,c("name", "tourney_name", "wins", "losses", "played", "percentage")]
  
  print(res)
  
}



PercentageDeciderSet <- function() {
  
  #player <- 'Roger Federer'
  
  #db <- db[winner_name == player | loser_name == player]
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "(WEA)")]
  
  #db <- db[tourney_level == 'G']
  
  #db <- db[round == 'SF']
  
  db <- db[(str_count(db$score, "-") == '5' & best_of == 5) | (str_count(db$score, "-") == '3' & best_of == 3)]
  
  # #extract year from tourney_date
  db$year <- stringr::str_sub(db$tourney_id, 0 ,4)
  
  #db <- db[year == '2020' | year == '2019' | year == '2018' | year == '2017' | year == '2016']
  
  ## wins
  wins <- db[,.N, by=winner_name]
  
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 50]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  res <- res[1:100,]
  print(res)
  
}


Percentage5thSet <- function() {
  
  #player <- 'Roger Federer'
  
  #db <- db[winner_name == player | loser_name == player]
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "(WEA)")]
  
  #db <- db[tourney_level == 'G']
  
  #db <- db[round == 'SF']
  
  db <- db[(str_count(db$score, "-") == '5' & best_of == 5)]
  
  # #extract year from tourney_date
  db$year <- stringr::str_sub(db$tourney_id, 0 ,4)
  
  #db <- db[year == '2020' | year == '2019' | year == '2018' | year == '2017' | year == '2016']
  
  ## wins
  wins <- db[,.N, by=winner_name]
  
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 15]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  res <- res[1:100,]
  print(res)
  
}


Percentage3rdSet <- function() {
  
  #player <- 'Roger Federer'
  
  #db <- db[winner_name == player | loser_name == player]
  
  db <- db[!db$score=="W/O" & !db$score=="DEF" & !db$score=="(ABN)" & !str_detect(db$score, "(WEA)")]
  
  #db <- db[tourney_level == 'G']
  
  #db <- db[round == 'SF']
  
  db <- db[(str_count(db$score, "-") == '3' & best_of == 3)]
  
  # #extract year from tourney_date
  db$year <- stringr::str_sub(db$tourney_id, 0 ,4)
  
  #db <- db[year == '2020' | year == '2019' | year == '2018' | year == '2017' | year == '2016']
  
  ## wins
  wins <- db[,.N, by=winner_name]
  
  ## losses
  losses <- db[,.N, by= loser_name]
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  ## merge the tables by "name"
  res <- merge(wins, losses, by = c("name"), all=TRUE)
  
  ## get rid of NAs, have 0 instead
  res[is.na(res)] <- 0
  
  ## sum the wins and losses into a new column played
  res <- res[, played:=wins+losses]
  
  ## calculate winning percentage
  res <- res[played > 15]
  
  res <- res[, percentage:=wins/played*100]
  
  res$percentage <- substr(res$percentage, 0, 5)
  res$percentage <- suppressWarnings(as.numeric(str_replace_all(res$percentage,pattern=',',replacement='.')))
  
  ## order by decreasing total matches
  setorder(res, -percentage)
  
  res$percentage <- paste(res$percentage, "%")
  
  res <- res[1:100,]
  print(res)
  
}