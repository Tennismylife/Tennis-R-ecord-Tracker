source("Same.R")
source("AgeFormat.R")

EntryOverallByAge <- function(order = "oldest", stage = "0") {
  
  # Remove team events from the dataset
  db <- removeTeamEvents(db)
  
  # Extract the year from the tourney ID
  db$Year <- substr(db$tourney_id, 1, 4)
  
  # Filter by tournament stage
  if (stage == "W") {
    db <- db[db$round == "F", ]  # Treat 'W' (winner) as final round
  } else if (stage != "0") {
    db <- db[db$round == stage, ]  # Filter by specified round
  }
  
  # Extract winner information
  wins <- unique(db[, c("tourney_name", "tourney_id", "winner_ioc", "winner_name", "winner_age")])
  colnames(wins) <- c("tournament", "year", "nat", "player", "age")
  
  # Extract loser information if applicable
  if (stage != "W") {
    losses <- unique(db[, c("tourney_name", "tourney_id", "loser_ioc", "loser_name", "loser_age")])
    colnames(losses) <- c("tournament", "year", "nat", "player", "age")
    
    # Combine winner and loser data
    res <- data.table::rbindlist(list(wins, losses), fill = TRUE)
  } else {
    res <- wins
  }
  
  # Normalize the year and clean the age column
  res$year <- substr(res$year, 1, 4)
  
  # Sort players by age
  if (tolower(order) == "oldest") {
    res <- res[order(-res$age), ]
  } else if (tolower(order) == "youngest") {
    res <- res[order(res$age), ]
  }
  
  # Optionally format age if the FormatAge function exists
  if (exists("FormatAge")) {
    res <- FormatAge(res)
  }
  
  return(res)
}




EntrySurfaceByAge <- function(court, order, stage) {
  
  # Remove team events (e.g., doubles, mixed) from the dataset
  db <- removeTeamEvents(db)
  
  # Filter matches by court surface (e.g., Clay, Hard, Grass)
  db <- db[surface == court]
  
  # Filter by tournament stage
  if(stage == 'W') {
    # If stage is 'W' (winners only), keep only final matches
    db <- db[round == 'F']
  } else if(stage != '0') {
    # Otherwise, filter by the specified round
    db <- db[round == stage]
  }
  
  # Extract winners of finals
  db_winners <- db[round == 'F']
  wins <- unique(db_winners[, c('tourney_name', 'tourney_id', 'winner_ioc', 'winner_name', 'winner_age')])
  
  # Extract losers only if not focusing exclusively on winners
  if(stage != 'W') {
    losses <- unique(db[, c('tourney_name', 'tourney_id', 'loser_ioc', 'loser_name', 'loser_age')])
  }
  
  # Rename columns for consistency
  names(wins) <- c("Tournament", "Year", "Nat", "Player", "age")
  wins$source <- "win"  # Optional: indicate that this is from winner data
  
  if(stage != 'W') {
    names(losses) <- c("Tournament", "Year", "Nat", "Player", "age")
    losses$source <- "loss"
    
    # Combine winners and losers
    res <- rbind(wins, losses)
  } else {
    res <- wins
  }
  
  # Extract year from tournament ID
  res$Year <- substr(res$Year, 1, 4)
  
  # Clean and convert age to numeric
  res$age <- substr(res$age, 1, 5)
  res$age <- suppressWarnings(as.numeric(gsub(",", ".", res$age)))
  
  # Order by age as specified
  if(order == "oldest") {
    res <- res[order(-res$age), ]
  } else if(order == "youngest") {
    res <- res[order(res$age), ]
  }
  
  # Format the age using a custom formatting function
  res <- FormatAge(res)
  
  # Return the final result
  return(res)
}


EntryCategoryByAge <- function(category, order, stage) {
  
  # Filter the main dataset by tournament level
  data <- db[tourney_level == category]
  
  # Filter by round (stage)
  if (stage == 'W') {
    # If stage is 'W', only take finals (to get winners)
    data <- data[round == 'F']
  } else if (stage != '0') {
    # If a specific round is provided (not 0), filter it
    data <- data[round == stage]
  }
  
  # Extract winner data with standardized column names
  wins <- unique(data[, .(
    tourney_name,
    tourney_id,
    flag = winner_ioc,
    name = winner_name,
    age = winner_age
  )])
  
  # If not just winners, extract loser data too
  if (stage != 'W') {
    losses <- unique(data[, .(
      tourney_name,
      tourney_id,
      flag = loser_ioc,
      name = loser_name,
      age = loser_age
    )])
    
    # Combine winners and losers, removing duplicates
    res <- rbind(wins, losses, fill = TRUE)
    res <- unique(res)
  } else {
    res <- wins
  }
  
  # Extract year from tourney_id
  res[, Year := substr(tourney_id, 1, 4)]
  
  # Clean age values and convert to numeric
  res[, age := substr(age, 1, 5)]
  res[, age := suppressWarnings(as.numeric(str_replace_all(age, ",", ".")))]
  
  # Sort players by age (oldest or youngest)
  if (order == 'oldest') {
    res <- res[order(-age)]
  } else if (order == 'youngest') {
    res <- res[order(age)]
  }
  
  # Rename columns for the final output
  setnames(res, old = c("tourney_name", "Year", "flag", "name"), 
           new = c("Tournament", "Year", "Nat", "Player"))
  
  # Apply custom age formatting (if function is defined)
  res <- FormatAge(res)
  
  res <- res[, .(Tournament, Year, Nat, Player, age)]
  
  # Print the final table
  print(res)
}




EntryTourByAge <- function(id, order, stage) {
  library(data.table)
  library(stringr)
  
  # Extract only the numeric part of the tourney_id (e.g., from "ATP-2023" to "2023")
  db$tid <- sub("^[^-]*-", "", db$tourney_id)
  
  # Filter matches for the selected tournament ID
  db_filtered <- db[db$tid == id]
  
  # Filter by stage/round (if specified and not 'W' or '0')
  if (stage != 'W' & stage != '0') {
    db_filtered <- db_filtered[round == stage]
  }
  
  # If stage is 'W', assume it means final ('F')
  if (stage == 'W') {
    db_filtered <- db_filtered[round == 'F']
  }
  
  # Extract winners' info and rename columns to common names
  wins <- unique(db_filtered[, .(
    tourney_name,
    tourney_id,
    flag = winner_ioc,
    name = winner_name,
    age = winner_age
  )])
  
  # If not only final winner, also extract losers' info
  if (stage != 'W') {
    losses <- unique(db_filtered[, .(
      tourney_name,
      tourney_id,
      flag = loser_ioc,
      name = loser_name,
      age = loser_age
    )])
    
    # Combine winners and losers, remove duplicates
    res <- rbind(wins, losses, fill = TRUE)
    res <- unique(res)
  } else {
    res <- wins
  }
  
  # Extract the year from the tourney_id
  res[, Year := substr(tourney_id, 1, 4)]
  
  # Clean and convert age column
  res[, age := substr(age, 1, 5)]  # Keep only the first few digits
  res[, age := suppressWarnings(as.numeric(str_replace_all(age, ",", ".")))]  # Replace commas with dots and convert to numeric
  
  # Sort the table based on age
  if (order == 'oldest') {
    res <- res[order(-age)]  # Descending
  } else if (order == 'youngest') {
    res <- res[order(age)]   # Ascending
  }
  
  # Rename columns for final output
  setnames(res, c("tourney_name", "flag", "name"), c("Tournament", "Nat", "Player"))
  
  # Apply formatting to age if FormatAge function exists
  if (exists("FormatAge", mode = "function")) {
    res <- FormatAge(res)
  }
  
  res <- res[, .(Tournament, Year, Nat, Player, age)]
  
  # Print the final result
  print(res)
}

