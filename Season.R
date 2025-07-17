library(stringr)

source("Entries.R")


EntriesSeason <- function() {
  
  # Remove team events from the dataset
  db <- removeTeamEvents(db)
  
  # Get unique tournament wins per player, including tournament name and ID
  wins <- unique(db[, c('winner_name', 'tourney_name', 'tourney_id')])
  wins <- dplyr::distinct(wins)
  
  # Get unique tournament losses per player, including tournament name and ID
  losses <- unique(db[, c('loser_name', 'tourney_name', 'tourney_id')])
  losses <- dplyr::distinct(losses)
  
  # Rename player columns to 'name' for merging
  names(wins)[1] <- names(losses)[1] <- "name"
  
  # Rename tournament name columns for clarity before merging
  names(wins)[2] <- "wins"
  names(losses)[2] <- "losses"
  
  # Merge wins and losses data by player name; full outer join to keep all players
  # 'allow.cartesian=TRUE' is a data.table option, but merge here is base R; this arg will be ignored
  res <- merge(wins, losses, all = TRUE)
  
  # Remove duplicates from merged data
  res <- dplyr::distinct(res)
  
  # Extract year from tournament ID by taking the first 4 characters (assuming ID starts with year)
  res$tourney_id <- stringr::str_sub(res$tourney_id, 1, 4)  # changed 0 to 1 for proper substring
  
  # Rename columns to player name and year for aggregation
  names(res)[1] <- "name"
  names(res)[2] <- "year"
  
  # Count the number of entries per player per year (season)
  season <- res[, .N, by = list(name, year)]
  
  # Rename columns for output clarity
  names(season) <- c("Player", "Season", "Entries")
  
  # Order the result by descending number of entries
  entry <- season[order(-Entries)]
  
  # Print the final table showing player entries by season
  print(entry)
}


SameSeasonWins <- function() {
  
  # Filter out matches that are walkovers, defaults, or abandoned (not countable wins)
  db <- db[!db$score %in% c("W/O", "DEF", "(ABN)")]
  
  # Extract the year from the tournament ID by taking the first 4 characters
  # Note: stringr::str_sub indexing starts at 1, not 0
  db$year <- stringr::str_sub(db$tourney_id, 1, 4)
  
  # Select winner name and tournament year
  wins <- db[, c('winner_name', 'tourney_id')]
  
  # Extract the year from tourney_id in the wins data as well
  wins$tourney_id <- stringr::str_sub(wins$tourney_id, 1, 4)
  
  # Rename columns for clarity
  names(wins) <- c("name", "year")
  
  # Count the number of wins per player per season
  sameseason <- wins[, .N, by = .(name, year)]
  
  # Rename columns for output
  names(sameseason) <- c("Player", "Season", "Wins")
  
  # Order by descending number of wins
  entry <- sameseason[order(-Wins)]
  
  # Print the results
  print(entry)
}


SameSeasonRound <- function(stage) {
  
  # Remove team events
  db <- removeTeamEvents(db)
  
  # Extract year from tourney_id (first 4 chars)
  db$year <- stringr::str_sub(db$tourney_id, 1, 4)
  
  # Filter matches based on the stage
  if (stage == 'W') {
    # For winner stage, select finals excluding withdrawn matches
    db <- db[round == 'F' & !str_detect(score, "WEA")]
    
    # Use only winners
    res <- db[, .(name = winner_name, year)]
  } else if (stage != '0') {
    # For other rounds (except '0'), select matches in that round
    db <- db[round == stage]
    
    # Combine winners and losers from that round
    winners <- db[, .(name = winner_name, year)]
    losers  <- db[, .(name = loser_name, year)]
    res <- rbind(winners, losers)
  } else {
    # If stage is '0', use all matches (or handle differently if needed)
    res <- db[, .(name = winner_name, year)]
  }
  
  # Count occurrences of players per season
  summary <- res[, .N, by = .(name, year)]
  
  # Keep only players with more than 1 occurrence
  summary <- summary[N > 1]
  
  # Order descending by count
  setorder(summary, -N)
  
  # Rename columns
  setnames(summary, c("name", "year", "N"), c("Player", "Season", "Count"))
  
  print(summary)
}



SeasonPercentage <- function() {
  # Extract year (first 4 chars) from tourney_id
  db$year <- stringr::str_sub(db$tourney_id, 1, 4)
  
  # Initialize empty list to collect results
  stats_list <- list()
  
  # Loop through each unique year and calculate percentages
  for (yr in unique(db$year)) {
    print(yr)
    stat <- PercentageInASeason(yr, db)
    stat <- add_column(stat, year = yr, .after = "name")
    stats_list[[yr]] <- stat
  }
  
  # Combine all yearly stats into one data.table
  result <- data.table::rbindlist(stats_list, fill = TRUE)
  
  # Order by descending percentage
  data.table::setorder(result, -percentage)
  
  return(result)
}

