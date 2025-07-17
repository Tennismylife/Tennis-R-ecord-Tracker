ListMatchesBagel <- function(){
  
  player <- 'Novak Djokovic'
  
  # Count occurrences of "6-0" and "0-6" in score for winner and loser bagels
  db[, winnerbagel := stringr::str_count(score, "6-0")]
  db[, loserbagel := stringr::str_count(score, "0-6")]
  
  # Filter matches where player either won with a bagel set or lost with a bagel set
  db_filtered <- db[
    (winner_name == player & winnerbagel > 0) | (loser_name == player & loserbagel > 0)
  ]
  
  # Further filter by tournament
  db_filtered <- db_filtered[tourney_name == 'Roland Garros']
  
  # Create a new column bagel = total bagels in match for the player
  db_filtered[, bagel := winnerbagel + loserbagel]
  
  # Extract year from tourney_id (assuming tourney_id starts with year)
  db_filtered[, year := substr(tourney_id, 1, 4)]
  
  # Select relevant columns to return
  result <- db_filtered[, .(tourney_name, year, round, winner_name, loser_name, score, bagel)]
  
  return(result)
}

MostBagel <- function() {
  library(stringr)
  
  # Extract the year from the first 4 characters of 'tourney_id'
  db$year <- substr(db$tourney_id, 1, 4)
  
  # Filter the dataset to keep only tournaments of level 'G'
  db <- db[db$tourney_level == 'G', ]
  
  # Count how many times "6-0" appears in the score (bagel sets) for winners
  db$winnerbagel <- str_count(db$score, fixed("6-0"))
  
  # Count how many times "0-6" appears in the score for losers
  db$loserbagel  <- str_count(db$score, fixed("0-6"))
  
  # Adjust loser bagel count in case of suspicious "0-6]" pattern in the score
  db$loserbagel <- ifelse(str_detect(db$score, fixed("0-6]")),
                          db$loserbagel - 1,
                          db$loserbagel)
  
  # Combine winner and loser bagel counts into one data frame
  all_bagels <- rbind(
    data.frame(Player = db$winner_name, Bagel = db$winnerbagel),
    data.frame(Player = db$loser_name,  Bagel = db$loserbagel)
  )
  
  # Replace any NA bagel counts with zero
  all_bagels$Bagel[is.na(all_bagels$Bagel)] <- 0
  
  # Aggregate total bagels per player by summing
  bagel_summary <- aggregate(Bagel ~ Player, data = all_bagels, sum)
  
  # Order the result by decreasing number of bagels
  bagel_summary <- bagel_summary[order(-bagel_summary$Bagel), ]
  
  # Return the summary table
  return(bagel_summary)
}




MostBagelInSameTournamentByPlayer <- function() {
  # Define score patterns
  score <- "6-0"
  reverse_score <- "0-6"
  bad_score <- "0-6]"
  
  # Count bagels (6-0 sets) won and lost
  db$winner_bagels <- stringr::str_count(db$score, score)
  db$loser_bagels <- stringr::str_count(db$score, reverse_score)
  
  # Adjust lost bagels if a malformed score (e.g., tiebreak) is found
  db$loser_bagels <- ifelse(stringr::str_detect(db$score, fixed(bad_score)),
                            db$loser_bagels - 1,
                            db$loser_bagels)
  
  # Keep only the part of tourney_id after the first dash
  db$tourney_id <- sub("^[^-]*-", "", db$tourney_id)
  
  # Prepare winner and loser data
  wins <- db[, c("winner_name", "winner_bagels", "tourney_id")]
  names(wins) <- c("Player", "Bagels", "tourney_id")
  
  losses <- db[, c("loser_name", "loser_bagels", "tourney_id")]
  names(losses) <- c("Player", "Bagels", "tourney_id")
  
  # Replace NA with 0
  wins$Bagels[is.na(wins$Bagels)] <- 0
  losses$Bagels[is.na(losses$Bagels)] <- 0
  
  # Combine winner and loser rows
  all_bagels <- rbind(wins, losses)
  
  # Aggregate total bagels per player per tournament
  bagel_totals <- aggregate(Bagels ~ Player + tourney_id, data = all_bagels, sum)
  
  # Get official tournament names from final matches (round == 'F')
  finals <- db[db$round == "F", ]
  finals$tourney_id <- sub("^[^-]*-", "", finals$tourney_id)
  
  # Get last occurrence per tourney_id to ensure consistent naming
  final_names <- finals[!duplicated(finals$tourney_id, fromLast = TRUE), c("tourney_name", "tourney_id")]
  
  # Merge official names with bagel totals
  merged <- merge(bagel_totals, final_names, by = "tourney_id", all.x = TRUE)
  
  # Reorder by descending bagels
  merged <- merged[order(-merged$Bagels), ]
  
  # Filter: only show players with more than 10 bagels in one tournament
  result <- merged[merged$Bagels > 10, c("Player", "tourney_name", "Bagels")]
  
  # Show result
  print(result)
  
  return(result)
}




MostBagelInSingleTournamentEdition <- function(tournament_names = c("French Open", "Roland Garros")) {
  # Remove team events from the database
  db <- removeTeamEvents(db)
  
  # Count bagels (6-0 wins and losses)
  db$winner_bagels <- stringr::str_count(db$score, "6-0")
  db$loser_bagels <- stringr::str_count(db$score, "0-6")
  
  # Fix malformed scores (e.g., '0-6]' instead of '0-6')
  db$loser_bagels <- ifelse(stringr::str_detect(db$score, "0-6]"),
                            db$loser_bagels - 1, db$loser_bagels)
  
  # Prepare data for aggregation
  winner_data <- db[, c("winner_bagels", "tourney_id")]
  loser_data  <- db[, c("loser_bagels", "tourney_id")]
  colnames(winner_data)[1] <- colnames(loser_data)[1] <- "bagels"
  
  # Replace NAs with 0
  winner_data$bagels[is.na(winner_data$bagels)] <- 0
  loser_data$bagels[is.na(loser_data$bagels)] <- 0
  
  # Combine winner and loser data
  all_bagels <- rbind(winner_data, loser_data)
  
  # Sum bagels per tournament edition
  bagel_summary <- aggregate(bagels ~ tourney_id, data = all_bagels, sum)
  
  # Extract year and simplify tourney_id
  bagel_summary$year <- substr(bagel_summary$tourney_id, 1, 4)
  bagel_summary$tourney_id <- sub("^[^-]*", "", bagel_summary$tourney_id)
  
  # Get official tournament name from final round only
  final_rounds <- db[db$round == "F", ]
  final_rounds$tourney_id <- sub("^[^-]*", "", final_rounds$tourney_id)
  
  official_names <- unique(final_rounds[, c("tourney_name", "tourney_id")])
  official_names <- dplyr::group_by(official_names, tourney_id) |> dplyr::slice(n())
  
  # Merge bagel data with official tournament names
  result <- dplyr::right_join(official_names, bagel_summary, by = "tourney_id")
  
  # Filter by desired tournament names and sort by year
  result <- subset(result, tourney_name %in% tournament_names)
  result <- data.table::setorder(result, year, na.last = FALSE)
  
  # Select and return relevant columns
  result <- result[, c("tourney_name", "year", "bagels")]
  return(result)
}



MostBagelInSingleTournamentEditionByPlayer <- function() {
  library(data.table)
  library(stringr)
  
  # Remove team events and keep only Grand Slam matches
  db <- removeTeamEvents(db)
  db <- as.data.table(db)
  db <- db[tourney_level == 'G']
  
  # Count bagels (6-0 sets) for winners and losers
  db[, winner_bagels := str_count(score, '6-0')]
  db[, loser_bagels := str_count(score, '0-6')]
  
  # Adjust for possible retirement/unfinished matches (e.g., "0-6]")
  db[str_detect(score, '0-6\\]'), loser_bagels := loser_bagels - 1]
  
  # Prepare separate tables for winners and losers
  wins <- db[, .(Player = winner_name, Bagels = winner_bagels, tourney_id)]
  losses <- db[, .(Player = loser_name, Bagels = loser_bagels, tourney_id)]
  
  # Combine winner and loser bagels
  all_bagels <- rbind(wins, losses)
  all_bagels[is.na(Bagels), Bagels := 0]
  
  # Total bagels per player per tournament edition
  bagel_summary <- all_bagels[, .(Bagels = sum(Bagels)), by = .(Player, tourney_id)]
  
  # Extract year and short tournament ID
  bagel_summary[, year := substr(tourney_id, 1, 4)]
  bagel_summary[, short_tourney_id := sub("^[^-]*", "", tourney_id)]
  
  # Get official tournament names from finals
  finals <- db[round == 'F']
  finals[, short_tourney_id := sub("^[^-]*", "", tourney_id)]
  official_names <- unique(finals[, .(tourney_name, short_tourney_id)])
  official_names <- official_names[,
                                   .SD[.N], by = short_tourney_id]  # Get the last name per tourney ID
  
  # Join official tournament names
  bagel_summary <- merge(bagel_summary, official_names, by = "short_tourney_id", all.x = TRUE)
  
  # Filter for players with more than 2 bagels in a single edition
  result <- bagel_summary[Bagels > 2, .(Player, tourney_name, year, Bagels)]
  
  # Order by descending bagel count
  setorder(result, -Bagels, na.last = FALSE)
  
  return(result)
}