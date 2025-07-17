AverageAgeTour <- function(id, stage) {
  # Check if data.table package is installed, stop if not
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package is required.")
  }
  library(data.table)
  
  # Create a new column 'tid' by removing the year and the first hyphen from 'tourney_id'
  # For example, '2019-US-Open' becomes 'US-Open'
  db$tid <- sub("^[^-]*-", "", db$tourney_id)
  
  # Filter rows where 'tid' matches the given 'id'
  filtered_db <- db[tid == id]
  
  # Further filter based on 'stage':
  # If stage is not 'W' (winner) or '0', filter by the specific round equal to 'stage'
  # If stage is 'W', select only finals (round == 'F')
  if (stage != 'W' && stage != '0') {
    filtered_db <- filtered_db[round == stage]
  } else if (stage == 'W') {
    filtered_db <- filtered_db[round == 'F']
  }
  
  # Calculate the average age combining winner and loser ages for each tournament edition
  # NA values are ignored in the mean calculation
  average <- filtered_db[, .(
    mean_age = mean(c(winner_age, loser_age), na.rm = TRUE)
  ), by = tourney_id]
  
  # Extract the year from 'tourney_id' assuming the first 4 characters represent the year
  average[, year := substr(tourney_id, 1, 4)]
  
  # Order the results by year in descending order
  setorder(average, -year)
  
  # Return the resulting table with average ages per tournament edition
  return(average)
}



AverageAgeH2HRound <- function() {
  
  # Filter for final round and Grand Slam level
  db_filtered <- db[round == 'F' & tourney_level == 'G']
  
  # Copy to avoid modifying original data.table
  res <- copy(db_filtered)
  
  # Calculate average age
  res[, averageage := (winner_age + loser_age) / 2]
  
  # Extract year from tourney_id (first 4 characters)
  res[, tourney_id := stringr::str_sub(tourney_id, 1, 4)]
  
  # Order by average age ascending (or descending if you prefer)
  setorder(res, averageage) 
  
  # Select relevant columns
  res <- res[, .(tourney_name, tourney_id, round, winner_ioc, winner_name, winner_age,
                 loser_ioc, loser_name, loser_age, averageage)]
  
  return(res)
}

AverageAgeinTournamentOfASeason <- function() {
  # Filter the dataset for finals ('F' round) in tournaments from 2024
  finals_2024 <- db[round == 'F' & grepl("1984-", tourney_id)]
  
  # Extract winner ages and label column as 'age'
  wins <- finals_2024[, .(tourney_id, age = winner_age)]
  
  # Extract loser ages and label column as 'age'
  losses <- finals_2024[, .(tourney_id, age = loser_age)]
  
  # Combine winners and losers into one data table
  res <- rbind(wins, losses)
  
  # Remove rows where age is NA to avoid bias in average calculation
  res <- res[!is.na(age)]
  
  # Get unique tournament IDs and their official names
  officialName <- unique(finals_2024[, .(tourney_id, tourney_name)])
  
  # Join the ages with the official tournament names
  res <- merge(res, officialName, by = "tourney_id", all.x = TRUE)
  
  # Calculate the average age per tournament
  average <- res[, .(average_age = mean(age)), by = .(tourney_id, tourney_name)]
  
  # Order tournaments by descending average age
  setorder(average, -average_age)
  
  # Format average age to two decimals and replace decimal point with comma
  average[, average_age := sub("\\.", ",", format(round(average_age, 2), nsmall=2))]
  
  # Print the resulting table
  print(average)
  
  # Return the table with average ages
  return(average)
}




#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
AverageAgeSeedingInATournament <- function() {
  # Filter only 'G' level tournaments
  stat <- db[tourney_level == 'G']
  
  # Convert seeds to integer, suppress warnings if any NAs introduced
  stat[, winner_seed := as.integer(winner_seed)]
  stat[, loser_seed := as.integer(loser_seed)]
  
  # Extract top 4 seeded winners and losers with their age and tournament ID
  winners <- unique(stat[winner_seed < 5, .(age = winner_age, seed = winner_seed, tourney_id)])
  losers  <- unique(stat[loser_seed < 5, .(age = loser_age,  seed = loser_seed,  tourney_id)])
  
  # Combine winners and losers into one table
  combined <- rbind(winners, losers)
  
  # Remove duplicates and order by tournament
  combined <- unique(combined)
  setorder(combined, tourney_id)
  
  # Calculate average age of top seeds per tournament
  avg_age <- combined[, .(age = mean(age, na.rm = TRUE)), by = tourney_id]
  
  # Get tournament names from final rounds of 'G' level tournaments
  final_rounds <- db[round == 'F' & tourney_level == 'G', .(tourney_id, tourney_name)]
  official_names <- unique(final_rounds)
  
  # Join average age with tournament names
  result <- merge(official_names, avg_age, by = "tourney_id")
  
  # Extract year from tournament ID (first 4 characters)
  result[, year := substr(tourney_id, 1, 4)]
  
  # FormatAge is assumed to format 'age' column (rounding, etc.)
  result <- FormatAge(result)
  
  # Select and order relevant columns by ascending average age
  result <- result[, .(tourney_name, year, age)]
  setorder(result, age)
  
  print(result)
}