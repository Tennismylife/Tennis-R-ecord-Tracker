EntriesOverall <- function() {
  
  db <- removeTeamEvents(db)
  
  # Combine player names
  players <- c(db$winner_name, db$loser_name)
  
  # Create a dataframe with players and tournament IDs
  participations <- data.frame(
    name = players,
    tourney_id = c(db$tourney_id, db$tourney_id)
  )
  
  # Remove duplicates to have each player's participation in a tournament only once
  unique_participations <- unique(participations)
  
  # Count unique tournaments for each player
  tournaments_count <- table(unique_participations$name)
  
  # Convert table to a data frame
  result <- data.frame(
    name = names(tournaments_count),
    number_of_tournaments = as.vector(tournaments_count)
  )
  
  # Sort the dataframe by number_of_tournaments in descending order
  sorted_result <- result[order(-result$number_of_tournaments), ]
  
  return(sorted_result)
}

EntriesSurface <- function(court) {
  
  db <- removeTeamEvents(db)
  
  db <- db[surface == court]
  
  # Combine player names
  players <- c(db$winner_name, db$loser_name)
  
  # Create a dataframe with players and tournament IDs
  participations <- data.frame(
    name = players,
    tourney_id = c(db$tourney_id, db$tourney_id)
  )
  
  # Remove duplicates to have each player's participation in a tournament only once
  unique_participations <- unique(participations)
  
  # Count unique tournaments for each player
  tournaments_count <- table(unique_participations$name)
  
  # Convert table to a data frame
  result <- data.frame(
    name = names(tournaments_count),
    number_of_tournaments = as.vector(tournaments_count)
  )
  
  # Sort the dataframe by number_of_tournaments in descending order
  sorted_result <- result[order(-result$number_of_tournaments), ]
  
  return(sorted_result)
}


EntriesCategory <- function(category) {
  
  db <- db[tourney_level == category]
  
  # Combine player names
  players <- c(db$winner_name, db$loser_name)
  
  # Create a dataframe with players and tournament IDs
  participations <- data.frame(
    name = players,
    tourney_id = c(db$tourney_id, db$tourney_id)
  )
  
  # Remove duplicates to have each player's participation in a tournament only once
  unique_participations <- unique(participations)
  
  # Count unique tournaments for each player
  tournaments_count <- table(unique_participations$name)
  
  # Convert table to a data frame
  result <- data.frame(
    name = names(tournaments_count),
    number_of_tournaments = as.vector(tournaments_count)
  )
  
  # Sort the dataframe by number_of_tournaments in descending order
  sorted_result <- result[order(-result$number_of_tournaments), ]
  
  return(sorted_result)
}

EntriesTournament <- function(id) {
  
  ## only select matches of a tournament
  db$id_to_search <- sub("^[^-]*", "", db$tourney_id)
  
  db <- db[id_to_search == id]
  
  # Combine player names
  players <- c(db$winner_name, db$loser_name)
  
  # Create a dataframe with players and tournament IDs
  participations <- data.frame(
    name = players,
    tourney_id = c(db$tourney_id, db$tourney_id)
  )
  
  # Remove duplicates to have each player's participation in a tournament only once
  unique_participations <- unique(participations)
  
  # Count unique tournaments for each player
  tournaments_count <- table(unique_participations$name)
  
  # Convert table to a data frame
  result <- data.frame(
    name = names(tournaments_count),
    number_of_tournaments = as.vector(tournaments_count)
  )
  
  # Sort the dataframe by number_of_tournaments in descending order
  sorted_result <- result[order(-result$number_of_tournaments), ]
  
  return(sorted_result)
}


EntriesCategoryByAge <- function(category) {
  # Extract data for winners in the specified tournament category and round 'F' (Finals)
  wins <- unique(db[tourney_level == category & round == 'F', 
                    .(tourney_name, tourney_date, flag = winner_ioc, name = winner_name, age = winner_age)])
  
  # Extract data for losers in the specified tournament category
  losses <- unique(db[tourney_level == category, 
                      .(tourney_name, tourney_date, flag = loser_ioc, name = loser_name, age = loser_age)])
  
  # Combine winners and losers into a single dataset
  res <- rbindlist(list(wins, losses), fill = TRUE)
  
  # Extract only the year from the tournament date
  res[, tourney_date := substr(tourney_date, 1, 4)]
  
  # Clean and convert the 'age' column to numeric format
  res[, age := suppressWarnings(as.numeric(str_replace_all(substr(age, 1, 5), ',', '.')))]
  
  # Sort the data by age in ascending order
  res <- res[order(age)]
  
  # Print the final result
  print(res)
}



EntrieSurfaceByAge <- function(court, order, stage) {
  
  db <- removeTeamEvents(db)
  
  db <- db[surface == court]
  
  if(stage == "W") {
    # If stage is "W", extract only wins from the final round
    res <- unique(db[surface == court & round == "F", 
                     .(tourney_name, tourney_date, flag = winner_ioc, name = winner_name, age = winner_age)])
  } else if(stage == 0) {
    # If stage is 0, take all data for the surface regardless of round
    wins <- unique(db[, 
                      .(tourney_name, tourney_date, flag = winner_ioc, name = winner_name, age = winner_age)])
    
    losses <- unique(db[, 
                        .(tourney_name, tourney_date, flag = loser_ioc, name = loser_name, age = loser_age)])
    
    res <- rbindlist(list(wins, losses), fill = TRUE)
  } else {
    # Extract data for winners in the specified round
    wins <- unique(db[surface == court & round == stage, 
                      .(tourney_name, tourney_date, flag = winner_ioc, name = winner_name, age = winner_age)])
    
    # Extract data for losers in the specified round
    losses <- unique(db[surface == court & round == stage, 
                        .(tourney_name, tourney_date, flag = loser_ioc, name = loser_name, age = loser_age)])
    
    res <- rbindlist(list(wins, losses), fill = TRUE)
  }
  
  # Extract only the year from the tournament date
  res[, tourney_date := substr(tourney_date, 1, 4)]
  
  # Clean and convert the 'age' column to numeric format
  res[, age := suppressWarnings(as.numeric(str_replace_all(substr(age, 1, 5), ',', '.')))]
  
  if(order == "oldest")
    res <- res[order(-age)] 
  
  if(order == "youngest")
    res <- res[order(age)]
  
  # Print the final result
  print(res)
}


EntriesByPlayer <- function() {
  
  db <- removeTeamEvents(db)
  
  player <- 'Jan-Lennard Struff'
  
  #tournaments won
  wins <- unique(db[,c('winner_name','tourney_name','tourney_id')])
  wins <- dplyr::distinct(wins)
  wins <- subset(wins, winner_name == player)
  
  
  #tournaments lost
  losses <- unique(db[,c('loser_name','tourney_name','tourney_id')])
  losses <- dplyr::distinct(losses)
  losses <- subset(losses, loser_name == player)
  
  
  ## common name to merge with
  names(wins)[1] <- names(losses)[1] <- "name"
  names(wins)[2] <-  names(losses)[2] <- "tournament"
  
  ## merge the tables by "name"
  res <- rbind(wins, losses, fill = TRUE)
  res <- dplyr::distinct(res)
  
  #extract year from tourney_date
  res$year <- stringr::str_sub(res$tourney_id, 0 ,4)
  
  res <- res[,c("name", "tournament", "year")]
  
  print(res)
}


MostEntriesNoTitle <- function() {
  # 1. Find winners of each tournament (who won the final round)
  final_matches <- db[db$round == "F", ]
  final_winners <- unique(final_matches[, c("tourney_id", "winner_name")])
  
  # 2. Find all players and the tournaments they played in (as winner or loser)
  winners_part <- unique(db[, c("winner_name", "tourney_id")])
  losers_part <- unique(db[, c("loser_name", "tourney_id")])
  
  colnames(winners_part) <- c("player", "tourney_id")
  colnames(losers_part) <- c("player", "tourney_id")
  
  all_participations <- unique(rbind(winners_part, losers_part))
  
  # 3. Count how many unique tournaments each player participated in
  tournaments_count <- aggregate(tourney_id ~ player, data = all_participations, FUN = function(x) length(unique(x)))
  colnames(tournaments_count)[2] <- "#Ts"  # rename column
  
  # 4. List of players who won at least one tournament (won the final)
  winners <- unique(final_winners$winner_name)
  
  # 5. Players who never won a tournament
  non_winners <- tournaments_count[!(tournaments_count$player %in% winners), ]
  
  # 6. Filter players who participated in more than one tournament
  result <- non_winners[non_winners$`#Ts` > 1, ]
  
  # 7. Sort descending by number of tournaments
  result <- result[order(-result$`#Ts`), ]
  
  return(result)
}
