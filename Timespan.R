TimespanTournamentEntry <- function(id, stage) {
  library(data.table)
  library(lubridate)
  
  # Modify tourney_id column in the original 'db' by removing prefix before '-'
  db[, tourney_id := sub("^[^-]*", "", tourney_id)]
  
  # Filter rows where tourney_id matches the given id
  filtered_db <- db[tourney_id == id]
  
  # Filter by stage if provided and not 'W' or '0'
  if (length(stage) > 0 & stage != 'W' & stage != '0') {
    filtered_db <- filtered_db[round == stage]
  }
  
  # If stage is 'W', filter only finals round ('F')
  if (stage == 'W') {
    filtered_db <- filtered_db[round == 'F']
  }
  
  # Get unique wins data (winner_name, tournament info, winner_age)
  wins <- unique(filtered_db[, .(name = winner_name, tourney_name, tourney_date, age = winner_age)])
  
  # If stage is not 'W', also get unique losses data (loser_name, tournament info, loser_age)
  if (stage != 'W') {
    losses <- unique(db[, .(name = loser_name, tourney_name, tourney_date, age = loser_age)])
    
    # Merge wins and losses by player name, tournament, date, and age, including all rows
    results <- merge(wins, losses, by = c("name", "tourney_name", "tourney_date", "age"), all = TRUE)
  } else {
    results <- wins
  }
  
  # Convert tournament date to Date format
  results[, tourney_date := ymd(tourney_date)]
  
  # Select first and last tournament date per player
  first_and_last <- results[, .SD[c(1, .N)], by = name]
  
  # Extract the first tournament date per player
  first_date <- first_and_last[, .SD[1], by = name]
  
  # Extract the last tournament date per player
  last_date <- first_and_last[, .SD[.N], by = name]
  
  # Combine first and last date into one table
  timespan <- cbind(first_date, last_date$tourney_date)
  
  # Rename columns for clarity
  setnames(timespan, c("name", "tourney_name", "first_date", "age", "last_date"))
  
  # Remove the age column as it's not needed
  timespan[, age := NULL]
  
  # Calculate difference in days between last and first tournament date
  timespan[, days := as.numeric(difftime(last_date, first_date, units = "days"))]
  
  # Order the table by descending number of days (longest span first)
  setorder(timespan, -days)
  
  # Rename columns for final output
  setnames(timespan, c("name", "tourney_name", "1st date", "last date", "days"))
  setnames(timespan, "name", "Player")
  setnames(timespan, "tourney_name", "Tournament")
  
  # Return the resulting table
  return(timespan)
}


TimespanTournamentWins <- function(id) {
  
  ## only select matches of a tournament
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  db <- db[tourney_id == id]
  
  #tournaments won
  wins <- unique(db[,c('winner_name','tourney_name','tourney_date', 'winner_age')])
  
  #transform date format
  wins$tourney_date <- lubridate::ymd(wins$tourney_date)
  
  #order by date in subgroup by player
  wins<-wins[order(wins$winner_name,wins$tourney_date),]
  
  #Select first and last element in date by name
  firstandlastdate<- wins[, .SD[c(1,.N)], by=winner_name]
  
  #Select first date for entry
  firstdate <- firstandlastdate[, .SD[c(1)], by=winner_name]
  
  #Select last date for entry
  lastdate <-firstandlastdate[, .SD[c(.N)], by=winner_name]
  
  #merge first and last date
  timespan<-cbind(firstdate,lastdate$tourney_date)
  
  #rename columns
  names(timespan)[3] <- "first_date"
  names(timespan)[5] <- "last_date"
  
  #erase age column
  timespan$winner_age <- NULL
  
  #calculate date diff by days
  timespan$Days<- difftime(timespan$last_date ,timespan$first_date , units = c("days"))
  
  #order the stat by age
  timespan <- timespan[order(timespan$Days, decreasing = TRUE),]
  
  timespan <- timespan[1:20,]
  
  print(timespan)
}


TimespanOverallWins <- function() {
  library(data.table)
  library(lubridate)
  
  # Extract unique wins with relevant columns, renaming for consistency
  wins <- unique(db[, .(name = winner_name, tourney_name, tourney_date, age = winner_age)])
  
  # Convert tournament date to Date format
  wins[, tourney_date := ymd(tourney_date)]
  
  # Order wins by player name and tournament date
  setorder(wins, name, tourney_date)
  
  # Select the first and last tournament entry per player
  first_and_last <- wins[, .SD[c(1, .N)], by = name]
  
  # Separate first and last entries
  first_entry <- first_and_last[, .SD[1], by = name]
  last_entry <- first_and_last[, .SD[.N], by = name]
  
  # Combine first and last entries into one table
  timespan <- cbind(
    first_entry,
    last_tournament = last_entry$tourney_name,
    last_date = last_entry$tourney_date
  )
  
  # Rename columns for clarity
  setnames(timespan, c("name", "first_tournament", "first_date", "age", "last_tournament", "last_date"))
  
  # Remove age column as it’s not needed for timespan calculation
  timespan[, age := NULL]
  
  # Calculate days between first and last tournament win
  timespan[, days := as.numeric(difftime(last_date, first_date, units = "days"))]
  
  # Order by timespan descending
  setorder(timespan, -days)
  
  # Select top 20 players with longest winning timespan
  top20 <- timespan[1:20]
  
  return(top20)
}


TimespanCategoryWins <- function(category) {
  library(data.table)
  library(lubridate)
  
  # Filter the dataset for tournaments of the given category
  category_db <- db[tourney_level == category]
  
  # Extract unique wins: winner name, tournament name, date, and winner age
  wins <- unique(category_db[, .(name = winner_name, tourney_name, tourney_date, age = winner_age)])
  
  # Convert tournament date to Date format
  wins[, tourney_date := ymd(tourney_date)]
  
  # Order by player name and tournament date
  setorder(wins, name, tourney_date)
  
  # Select first and last tournament entry per player
  first_and_last <- wins[, .SD[c(1, .N)], by = name]
  
  # Separate first and last entries
  first_entry <- first_and_last[, .SD[1], by = name]
  last_entry <- first_and_last[, .SD[.N], by = name]
  
  # Combine first and last entries into one table
  timespan <- cbind(
    first_entry,
    last_tournament = last_entry$tourney_name,
    last_date = last_entry$tourney_date
  )
  
  # Rename columns for clarity
  setnames(timespan, c("name", "first_tournament", "first_date", "age", "last_tournament", "last_date"))
  
  # Remove age column
  timespan[, age := NULL]
  
  # Calculate days difference between first and last tournament win
  timespan[, days := as.numeric(difftime(last_date, first_date, units = "days"))]
  
  # Order descending by days difference
  setorder(timespan, -days)
  
  # Select top 20 players with longest winning timespan in the category
  top20 <- timespan[1:20]
  
  # Return the result instead of printing for better usability
  return(top20)
}


TimespanSurfaceWins <- function(court) {
  library(data.table)
  library(lubridate)
  
  # Filter tournaments played on the specified surface
  surface_db <- db[surface == court]
  
  # Extract unique wins: winner name, tournament name, date, and winner age
  wins <- unique(surface_db[, .(name = winner_name, tourney_name, tourney_date, age = winner_age)])
  
  # Convert tournament dates to Date format
  wins[, tourney_date := ymd(tourney_date)]
  
  # Order wins by player name and tournament date
  setorder(wins, name, tourney_date)
  
  # Select first and last tournament win for each player
  first_and_last <- wins[, .SD[c(1, .N)], by = name]
  
  # Separate first and last entries
  first_entry <- first_and_last[, .SD[1], by = name]
  last_entry <- first_and_last[, .SD[.N], by = name]
  
  # Combine first and last entries
  timespan <- cbind(
    first_entry,
    last_tournament = last_entry$tourney_name,
    last_date = last_entry$tourney_date
  )
  
  # Rename columns for clarity
  setnames(timespan, c("name", "first_tournament", "first_date", "age", "last_tournament", "last_date"))
  
  # Remove the age column (not needed here)
  timespan[, age := NULL]
  
  # Calculate difference in days between last and first tournament win
  timespan[, days := as.numeric(difftime(last_date, first_date, units = "days"))]
  
  # Order descending by days difference (longest winning span first)
  setorder(timespan, -days)
  
  # Return top 20 players with longest winning timespan on the surface
  return(timespan[1:20])
}



############## ENTRY #################

TimespanOverallEntry <- function(stage) {
  library(data.table)
  library(lubridate)
  
  # Assuming removeTeamEvents is defined elsewhere and returns a filtered data.table
  db_filtered <- removeTeamEvents(db)
  
  # Filter by stage if specified and not 'W' or '0'
  if (length(stage) > 0 & stage != 'W' & stage != '0') {
    db_filtered <- db_filtered[round == stage]
  }
  
  # If stage is 'W', select only finals round ('F')
  if (stage == 'W') {
    db_filtered <- db_filtered[round == 'F']
  }
  
  # Get unique wins with relevant columns
  wins <- unique(db_filtered[, .(name = winner_name, tourney_name, tourney_date, age = winner_age)])
  
  # Get unique losses if stage is not 'W'
  if (stage != 'W') {
    losses <- unique(db_filtered[, .(name = loser_name, tourney_name, tourney_date, age = loser_age)])
    
    # Merge wins and losses on common keys (name, tournament, date, age)
    results <- merge(wins, losses, by = c("name", "tourney_name", "tourney_date", "age"), all = TRUE)
  } else {
    results <- wins
  }
  
  # Convert tournament dates to Date class
  results[, tourney_date := ymd(tourney_date)]
  
  # Order by player name and tournament date
  setorder(results, name, tourney_date)
  
  # Select first and last tournament date per player
  first_and_last <- results[, .SD[c(1, .N)], by = name]
  
  # Separate first and last entries
  first_entry <- first_and_last[, .SD[1], by = name]
  last_entry <- first_and_last[, .SD[.N], by = name]
  
  # Combine first and last entries into one data.table
  timespan <- cbind(
    first_entry,
    last_tournament = last_entry$tourney_name,
    last_date = last_entry$tourney_date
  )
  
  # Rename columns for clarity
  setnames(timespan, 
           old = c("name", "tourney_name", "tourney_date", "age", "last_tournament", "last_date"),
           new = c("Player", "1st tournament", "1st date", "age", "last tournament", "last date"))
  
  # Remove age column
  timespan[, age := NULL]
  
  # Calculate difference in days between last and first tournament entry
  timespan[, days := as.numeric(difftime(`last date`, `1st date`, units = "days"))]
  
  # Order descending by days
  setorder(timespan, -days)
  
  # Return the final timespan data.table
  return(timespan)
}



TimespanCategoryEntry <- function(category, stage) {
  library(data.table)
  library(lubridate)
  
  # Filter dataset for the given category
  category_db <- db[tourney_level == category]
  
  # Filter by stage if specified (and not 'W' or '0')
  if (length(stage) > 0 & stage != 'W' & stage != '0') {
    category_db <- category_db[round == stage]
  }
  
  # If stage is 'W', select only finals round ('F')
  if (stage == 'W') {
    category_db <- category_db[round == 'F']
  }
  
  # Unique tournaments won by players in the category and stage
  wins <- unique(category_db[, .(name = winner_name, tourney_name, tourney_date, age = winner_age)])
  
  # Unique tournaments lost (only if stage is not 'W')
  if (stage != 'W') {
    losses <- unique(category_db[, .(name = loser_name, tourney_name, tourney_date, age = loser_age)])
    
    # Merge wins and losses by player, tournament, date, and age
    results <- merge(wins, losses, by = c("name", "tourney_name", "tourney_date", "age"), all = TRUE)
  } else {
    results <- wins
  }
  
  # Convert tourney_date to Date format
  results[, tourney_date := ymd(tourney_date)]
  
  # Order by player name and tournament date
  setorder(results, name, tourney_date)
  
  # Select first and last tournament date per player
  first_and_last <- results[, .SD[c(1, .N)], by = name]
  
  # Extract first and last entries separately
  first_entry <- first_and_last[, .SD[1], by = name]
  last_entry <- first_and_last[, .SD[.N], by = name]
  
  # Combine first and last entries into one data.table
  timespan <- cbind(
    first_entry,
    last_tournament = last_entry$tourney_name,
    last_date = last_entry$tourney_date
  )
  
  # Rename columns for clarity
  setnames(timespan, 
           old = c("name", "tourney_name", "tourney_date", "age", "last_tournament", "last_date"),
           new = c("Player", "1st tournament", "1st date", "age", "last tournament", "last date"))
  
  # Remove age column as it is not used here
  timespan[, age := NULL]
  
  # Calculate days difference between last and first tournament entry
  timespan[, days := as.numeric(difftime(`last date`, `1st date`, units = "days"))]
  
  # Order by days descending (longest span first)
  setorder(timespan, -days)
  
  # Return the resulting data.table
  return(timespan)
}




TimespanSurfaceEntry <- function(court, stage) {
  library(data.table)
  library(lubridate)
  
  # Remove team events from db
  clean_db <- removeTeamEvents(db)
  
  # Filter dataset by surface (court)
  filtered_db <- clean_db[surface == court]
  
  # Filter by stage if provided and not 'W' or '0'
  if(length(stage) > 0 & stage != 'W' & stage != '0') {
    filtered_db <- filtered_db[round == stage]
  }
  
  # If stage is 'W', filter to finals ('F')
  if(stage == 'W') {
    filtered_db <- filtered_db[round == 'F']
  }
  
  # Unique tournaments won by players on this surface/stage
  wins <- unique(filtered_db[, .(name = winner_name, tourney_name, tourney_date, age = winner_age)])
  
  # Unique tournaments lost by players (only if stage not 'W')
  if(stage != 'W') {
    losses <- unique(filtered_db[, .(name = loser_name, tourney_name, tourney_date, age = loser_age)])
    
    # Merge wins and losses by player, tournament, date and age to get all matches
    results <- merge(wins, losses, by = c("name", "tourney_name", "tourney_date", "age"), all = TRUE)
  } else {
    results <- wins
  }
  
  # Convert tournament date to Date format
  results[, tourney_date := ymd(tourney_date)]
  
  # Order by player and tournament date ascending
  setorder(results, name, tourney_date)
  
  # Select first and last tournament entries per player
  first_and_last <- results[, .SD[c(1, .N)], by = name]
  
  # Separate first and last entries
  first_entry <- first_and_last[, .SD[1], by = name]
  last_entry <- first_and_last[, .SD[.N], by = name]
  
  # Combine first and last tournament info side by side
  timespan <- cbind(
    first_entry,
    last_tournament = last_entry$tourney_name,
    last_date = last_entry$tourney_date
  )
  
  # Rename columns for clarity
  setnames(timespan,
           old = c("name", "tourney_name", "tourney_date", "age", "last_tournament", "last_date"),
           new = c("Player", "1st tournament", "1st date", "age", "last tournament", "last date"))
  
  # Remove age column
  timespan[, age := NULL]
  
  # Calculate days difference between last and first tournament entries
  timespan[, days := as.numeric(difftime(`last date`, `1st date`, units = "days"))]
  
  # Order descending by days (longest timespan first)
  setorder(timespan, -days)
  
  # Return the resulting table
  return(timespan)
}


SameTournamentTimespan <- function() {
  require(dplyr)
  require(parallel)
  require(data.table)
  
  
  # Unique `tourney_id`
  tour <- db %>%
    mutate(id = sub("^[^\\-]*", "", tourney_id)) %>%
    pull(id) %>%
    unique()
  
  # Configure cluster
  n_cores <- max(1, detectCores() - 1)
  cl <- makeCluster(n_cores)
  
  # Expport variables
  clusterExport(cl, c("db", "TimespanTournamentEntry"), envir = environment())
  clusterEvalQ(cl, {
    library(dplyr)
    library(lubridate)
    library(data.table)
  })
  
  # Parallel execution
  results <- tryCatch(
    parLapply(cl, tour, function(tour_id) {
      TimespanTournamentEntry(tour_id, 'W')
    }),
    error = function(e) {
      stop("Error during parallel processing: ", e$message)
    }
  )
  
  # Close cluster
  stopCluster(cl)
  
  # Combine results
  stat <- do.call(rbind, results)
  
  # Order and filter
  if (!is.null(stat) && "days" %in% names(stat)) {
    stat <- stat[order(-stat$days), ]
    stat <- subset(stat, days > 5000)
  } else {
    warning("No valid results or 'days' column not found.")
    stat <- NULL
  }
  
  return(stat)
}
