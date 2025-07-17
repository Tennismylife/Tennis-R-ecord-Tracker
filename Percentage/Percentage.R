PercentageOverall <- function(min_played = 20) {
  library(data.table)
  library(stringr)
  
  # Filter out matches with invalid or incomplete scores
  filtered_db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "WEA")]
  
  # Calculate total wins and losses by player
  wins <- filtered_db[, .N, by = winner_name]
  losses <- filtered_db[, .N, by = loser_name]
  
  # Rename columns to enable merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge wins and losses; fill missing values with zero
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total matches played and win percentage
  res[, played := wins + losses]
  res[, percentage_num := round((wins / played) * 100, 2)]
  
  # Filter players with zero losses and fewer than min_played matches
  res <- res[losses > 0 & played >= min_played]
  
  # Add a space before the % sign
  res[, percentage := paste0(percentage_num, " %")]
  
  # Sort by descending winning percentage (using numeric percentage)
  setorder(res, -percentage_num)
  
  # Drop the numeric helper column
  res[, percentage_num := NULL]
  
  return(res)
}






PercentageSurface <- function(court, min_played = 20) {
  library(data.table)
  library(stringr)
  
  # Replace all NA values in db with 0
  db[is.na(db)] <- 0
  
  # Filter out matches with walkovers, defaults, abandonments, or weak endings
  filtered_db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "WEA")]
  
  # Filter matches played on the specified surface
  filtered_db <- filtered_db[surface == court]
  
  # Calculate wins and losses by player
  wins <- filtered_db[, .N, by = winner_name]
  losses <- filtered_db[, .N, by = loser_name]
  
  # Rename columns for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge wins and losses tables, replacing NAs with 0
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total matches played
  res[, played := wins + losses]
  
  # Filter players with at least one loss and at least min_played matches
  res <- res[losses > 0 & played >= min_played]
  
  # Calculate winning percentage and format with a space before '%'
  res[, percentage_num := round((wins / played) * 100, 2)]
  res[, percentage := paste0(percentage_num, " %")]
  
  # Sort by descending winning percentage
  setorder(res, -percentage_num)
  
  # Drop numeric helper column
  res[, percentage_num := NULL]
  
  print(res)
}




PercentageCategory <- function(category, min_played = 20) {
  library(data.table)
  library(stringr)
  
  # Filter out walkovers, defaults, abandonments, and weak endings
  filtered_db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "WEA")]
  
  # Filter matches by category (tourney_level)
  filtered_db <- filtered_db[tourney_level == category]
  
  # Count wins and losses by player
  wins <- filtered_db[, .N, by = winner_name]
  losses <- filtered_db[, .N, by = loser_name]
  
  # Rename columns for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge wins and losses tables; replace NAs with 0
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total matches played
  res[, played := wins + losses]
  
  # Filter players with at least min_played matches played
  res <- res[played >= min_played]
  
  # Calculate winning percentage and format with space before '%'
  res[, percentage_num := round((wins / played) * 100, 2)]
  res[, percentage := paste0(percentage_num, " %")]
  
  # Sort by descending winning percentage
  setorder(res, -percentage_num)
  
  # Remove helper column
  res[, percentage_num := NULL]
  
  print(res)
}


PercentageTour <- function(id, min_played = 1) {
  library(data.table)
  library(stringr)
  
  # Filter out walkovers, defaults, abandonments, and weak endings
  filtered_db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "WEA")]
  
  # Remove prefix before "-" in tourney_id to keep only suffix (e.g. "-580")
  filtered_db[, tourney_id := sub("^[^-]*", "", tourney_id)]
  
  # Filter by tournament id
  filtered_db <- filtered_db[tourney_id == id]
  
  # Count wins and losses by player
  wins <- filtered_db[, .N, by = winner_name]
  losses <- filtered_db[, .N, by = loser_name]
  
  # Rename columns for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge wins and losses tables, replace NAs with 0
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total matches played
  res[, played := wins + losses]
  
  # Filter players who played at least min_played matches
  res <- res[played >= min_played]
  
  # Calculate winning percentage and format with space before "%"
  res[, percentage_num := round((wins / played) * 100, 2)]
  res[, percentage := paste0(percentage_num, " %")]
  
  # Sort by descending winning percentage
  setorder(res, -percentage_num)
  
  # Remove helper column
  res[, percentage_num := NULL]
  
  print(res)
}




PercentageSameSeason <- function(min_played = 10) {
  library(data.table)
  library(stringr)
  
  # Filter out walkovers, defaults, abandonments, and weak endings
  filtered_db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "WEA")]
  
  # Extract year from tourney_id (first 4 characters)
  filtered_db[, year := str_sub(tourney_id, 1, 4)]
  
  # Filter matches on Hard surface
  filtered_db <- filtered_db[surface == "Hard"]
  
  # Count wins by player and year
  wins <- filtered_db[, .N, by = .(winner_name, year)]
  # Count losses by player and year
  losses <- filtered_db[, .N, by = .(loser_name, year)]
  
  # Rename columns for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge wins and losses by player and year, fill NAs with 0
  res <- merge(wins, losses, by = c("name", "year"), all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total matches played
  res[, played := wins + losses]
  
  # Filter players with at least min_played matches
  res <- res[played >= min_played]
  
  # Calculate winning percentage
  res[, percentage_num := round((wins / played) * 100, 2)]
  
  # Format percentage with space before "%"
  res[, percentage := paste0(percentage_num, " %")]
  
  # Order by decreasing percentage
  setorder(res, -percentage_num)
  
  # Remove helper column
  res[, percentage_num := NULL]
  
  # Rename year column to ensure consistency
  setnames(res, "year", "year")
  
  return(res)
}



PercentageSameSurface <- function(min_played = 20) {
  library(data.table)
  library(stringr)
  
  # Filter out walkovers, defaults, abandonments
  filtered_db <- db[!score %in% c("W/O", "DEF", "(ABN)")]
  
  # Count wins by player and surface
  wins <- filtered_db[, .N, by = .(winner_name, surface)]
  
  # Count losses by player and surface
  losses <- filtered_db[, .N, by = .(loser_name, surface)]
  
  # Rename columns for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge wins and losses by player and surface
  # Use all=TRUE for full outer join, cartesian=FALSE (default)
  res <- merge(wins, losses, by = c("name", "surface"), all = TRUE)
  
  # Replace NAs with 0
  res[is.na(res)] <- 0
  
  # Calculate total matches played
  res[, played := wins + losses]
  
  # Filter players with at least min_played matches
  res <- res[played >= min_played]
  
  # Calculate winning percentage
  res[, percentage_num := round((wins / played) * 100, 2)]
  
  # Format percentage with space before '%'
  res[, percentage := paste0(percentage_num, " %")]
  
  # Order by decreasing percentage
  setorder(res, -percentage_num)
  
  # Select top 100 players by percentage
  res <- res[1:100]
  
  # Drop helper column
  res[, percentage_num := NULL]
  
  print(res)
}


PercentageSameTour <- function() {
  
  library(data.table)
  library(stringr)
  library(dplyr)
  
  # Remove team events from the dataset
  db <- removeTeamEvents(db)
  
  # Filter out matches with certain score statuses (walkovers, defaults, abandonments, weather issues)
  db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "WEA")]
  
  # Extract numeric part of the tournament ID by removing the first 4 characters
  db[, tourney_id := str_sub(tourney_id, 5)]
  
  # Calculate the number of wins per player and tournament
  wins <- db[, .N, by = .(winner_name, tourney_id)]
  
  # Calculate the number of losses per player and tournament
  losses <- db[, .N, by = .(loser_name, tourney_id)]
  
  # Rename columns to have a common player name column for merging
  setnames(wins, c("winner_name", "tourney_id", "N"), c("name", "tourney_id", "wins"))
  setnames(losses, c("loser_name", "tourney_id", "N"), c("name", "tourney_id", "losses"))
  
  # Merge wins and losses tables by player name and tournament, including all records
  res <- merge(wins, losses, by = c("name", "tourney_id"), all = TRUE)
  
  # Replace NA values with zero to handle players with only wins or only losses
  res[is.na(res)] <- 0
  
  # Calculate total matches played per player and tournament
  res[, played := wins + losses]
  
  # Keep only players who played more than 19 matches to ensure statistical relevance
  res <- res[played > 19]
  
  # Calculate winning percentage as wins divided by matches played (rounded to 2 decimals)
  res[, percentage := round((wins / played) * 100, 2)]
  
  # Extract the official tournament names, removing 'WCT' from names for clarity
  officialName <- unique(db[, .(tourney_id, tourney_name)])
  officialName[, tourney_name := str_replace(tourney_name, "WCT", "")]
  
  # Order the tournament names by ID and name, then keep the last occurrence per tournament ID
  officialName <- officialName[order(tourney_id, tourney_name)]
  officialName <- officialName[, .SD[.N], by = tourney_id]
  
  # Merge the tournament names with the player stats results by tournament ID
  res <- merge(officialName, res, by = "tourney_id", all.y = TRUE)
  
  # Sort the final results by descending winning percentage
  setorder(res, -percentage)
  
  # Append a percent sign to the winning percentage for display
  res[, percentage := paste0(percentage, " %")]
  
  # Select and order relevant columns for the output
  res <- res[, .(name, tourney_name, wins, losses, played, percentage)]
  
  # Return the final result table
  return(res)
}

  



PercentageDeciderSet <- function() {
  
  # Remove walkovers, defaults, abandoned matches, and weather affected matches
  db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "(WEA)")]
  
  # Filter matches where number of sets matches best_of format
  db <- db[(str_count(score, "-") == 5 & best_of == 5) | (str_count(score, "-") == 3 & best_of == 3)]
  
  # Extract year from tourney_id (assumed first 4 characters)
  db$year <- stringr::str_sub(db$tourney_id, 1, 4)
  
  # Calculate wins and losses by player
  wins <- db[, .N, by = winner_name]
  losses <- db[, .N, by = loser_name]
  
  # Rename columns for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge and fill NAs
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total played and filter players with >100 matches
  res[, played := wins + losses]
  res <- res[played > 100]
  
  # Calculate winning percentage and round
  res[, percentage := round(wins / played * 100, 2)]
  
  # Order by winning percentage descending
  setorder(res, -percentage)
  
  # Add percentage string with % symbol
  res[, percentage := paste0(percentage, " %")]
  
  # Return top 100
  return(res[1:100, .(name, wins, losses, played, percentage)])
}



Percentage5thSet <- function() {
  
  # Filter out invalid or incomplete matches
  db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "\\(WEA\\)")]
  
  # Keep only completed 5-set matches
  db <- db[str_count(score, "-") == 5 & best_of == 5]
  
  # Extract year from tourney_id
  db[, year := str_sub(tourney_id, 1, 4)]
  
  # Count wins and losses per player
  wins <- db[, .N, by = winner_name]
  losses <- db[, .N, by = loser_name]
  
  # Rename for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge wins and losses
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total played and win percentage
  res[, played := wins + losses]
  res <- res[played > 10]
  res[, percentage := round(wins / played * 100, 2)]
  
  # Sort and format
  setorder(res, -percentage)
  res[, percentage := paste0(percentage, " %")]
  
  # Return top 100
  return(res[1:100])
}


Percentage3rdSet <- function() {
  
  # Remove invalid score entries
  db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "\\(WEA\\)")]
  
  # Filter: best-of-3 matches that went to 3 sets (i.e., 3 hyphens in score)
  db <- db[str_count(score, "-") == 3 & best_of == 3]
  
  # Extract year from tourney_id
  db[, year := str_sub(tourney_id, 1, 4)]
  
  # Count wins and losses
  wins <- db[, .N, by = winner_name]
  losses <- db[, .N, by = loser_name]
  
  # Rename columns for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge tables
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total matches and winning percentage
  res[, played := wins + losses]
  res <- res[played > 15]
  res[, percentage := round(wins / played * 100, 2)]
  
  # Sort by percentage
  setorder(res, -percentage)
  
  # Format percentage as string with %
  res[, percentage := paste0(percentage, " %")]
  
  # Return top 100
  return(res[1:100])
}



PercentageBestOf5 <- function() {
  
  # Filter out invalid scores
  db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "\\(WEA\\)")]
  
  # Keep only best-of-5 matches
  db <- db[best_of == 5]
  
  # Extract year
  db[, year := str_sub(tourney_id, 1, 4)]
  
  # Count wins and losses
  wins <- db[, .N, by = winner_name]
  losses <- db[, .N, by = loser_name]
  
  # Rename for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Compute total and filter
  res[, played := wins + losses]
  res <- res[played > 15]
  
  # Calculate and format win percentage
  res[, percentage := round(wins / played * 100, 2)]
  
  # Sort and format percentage string
  setorder(res, -percentage)
  res[, percentage := paste0(percentage, " %")]
  
  # Return top 100
  return(res[1:100])
}


PercentageBestOf3 <- function() {
  
  # Filter out invalid matches
  db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "\\(WEA\\)")]
  
  # Filter to best-of-3 matches only
  db <- db[best_of == 3]
  
  # Extract year
  db[, year := str_sub(tourney_id, 1, 4)]
  
  # Count wins and losses
  wins <- db[, .N, by = winner_name]
  losses <- db[, .N, by = loser_name]
  
  # Rename for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge results
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Compute total matches and filter
  res[, played := wins + losses]
  res <- res[played > 15]
  
  # Calculate winning percentage
  res[, percentage := round(wins / played * 100, 2)]
  
  # Sort by descending percentage
  setorder(res, -percentage)
  
  # Format percentage
  res[, percentage := paste0(percentage, " %")]
  
  # Return top 100
  return(res[1:100])
}



PercentageInASeason <- function(season, db) {
  
  # Remove invalid scores
  db <- db[!score %in% c("W/O", "DEF") & 
             !str_detect(score, "\\(WEA\\)") & 
             !str_detect(score, "\\(ABN\\)")]
  
  # Extract year from tourney_id
  db[, year := str_sub(tourney_id, 1, 4)]
  
  # Filter by given season
  stat <- db[year == season]
  
  # Count wins and losses
  wins <- stat[, .N, by = winner_name]
  losses <- stat[, .N, by = loser_name]
  
  # Rename columns for merge
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge tables
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total matches and percentage
  res[, played := wins + losses]
  res <- res[played > 10]
  res[, percentage := round((wins / played) * 100, 2)]
  
  # Order by descending percentage
  setorder(res, -percentage)
  res[, percentage := paste0(percentage, " %")]
  
  return(res)
}




PercentageRound <- function(stage, min_matches = 5) {
  
  # Remove team events
  db <- removeTeamEvents(db)
  
  # Filter for the round and Grand Slam matches
  db <- db[round == stage & tourney_level == 'G']
  
  # Exclude walkovers, defaults, abandonments, and weather-related
  db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "\\(WEA\\)")]
  
  # Count wins and losses
  wins <- db[, .N, by = winner_name]
  losses <- db[, .N, by = loser_name]
  
  # Standardize names for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge, replacing NAs with 0
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Compute matches played and win percentage
  res[, played := wins + losses]
  res <- res[played >= min_matches]
  res[, percentage := round(wins / played * 100, 2)]
  
  # Sort and format
  setorder(res, -percentage)
  res[, percentage := paste0(percentage, " %")]
  
  return(res)
}


PercentageVsTop10 <- function(min_matches = 2) {
  
  # Convert rankings to integers
  db[, loser_rank := as.integer(loser_rank)]
  db[, winner_rank := as.integer(winner_rank)]
  
  # Filter valid matches (exclude walkovers, DEF, ABN, WEA)
  db <- db[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "\\(WEA\\)")]
  
  # Wins vs top 10: when opponent (loser) is ranked < 11
  wins <- db[loser_rank < 11, .N, by = winner_name]
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  
  # Losses vs top 10: when opponent (winner) is ranked < 11
  losses <- db[winner_rank < 11, .N, by = loser_name]
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge and handle NA
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Total played
  res[, played := wins + losses]
  res <- res[played >= min_matches]
  
  # Percentage
  res[, percentage := round(wins / played * 100, 2)]
  
  # Order
  setorder(res, -percentage)
  res[, percentage := paste0(percentage, " %")]
  
  return(res)
}


PercentageAsNumber1 <- function(min_matches = 10) {
  
  # Clean invalid matches
  db <- db[!score %in% c("W/O", "DEF", "(ABN)", "ABN")]
  
  # Ensure rank is numeric
  db[, winner_rank := as.integer(winner_rank)]
  db[, loser_rank := as.integer(loser_rank)]
  
  # Wins when player is ranked No. 1
  wins <- db[winner_rank == 1, .N, by = winner_name]
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  
  # Losses when player is ranked No. 1
  losses <- db[loser_rank == 1, .N, by = loser_name]
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Total matches as No.1 and win percentage
  res[, played := wins + losses]
  res <- res[played >= min_matches]
  res[, percentage := round(wins / played * 100, 2)]
  
  # Format percentage and order
  res[, percentage := paste0(percentage, "%")]
  setorder(res, -percentage)
  
  return(res)
}


PercentageSeasonbyPlayer <- function(player = 'Roger Federer') {
  library(data.table)
  library(stringr)
  
  # Remove invalid matches (walkovers, defaults, abandoned, etc.)
  db <- db[!score %in% c("W/O", "DEF", "(ABN)", "ABN")]
  
  # Extract year from tournament ID (first 4 characters)
  db[, year := str_sub(tourney_id, 1, 4)]
  
  # Calculate number of wins per year for the player
  wins <- db[winner_name == player, .N, by = year]
  setnames(wins, "N", "wins")
  
  # Calculate number of losses per year for the player
  losses <- db[loser_name == player, .N, by = year]
  setnames(losses, "N", "losses")
  
  # Calculate number of titles per year (final round wins)
  titles <- db[round == 'F' & winner_name == player, .N, by = year]
  setnames(titles, "N", "titles")
  
  # Merge wins, losses, and titles by year
  res <- merge(wins, losses, by = "year", all = TRUE)
  res <- merge(res, titles, by = "year", all = TRUE)
  
  # Replace any NA values with 0
  res[is.na(res)] <- 0
  
  # Calculate total matches played per year and winning percentage for that year
  res[, played := wins + losses]
  res[, percyear := round(wins / played * 100, 2)]
  
  # Calculate cumulative totals across all years
  res[, `:=`(
    totalwins = cumsum(wins),
    totLosses = cumsum(losses),
    totTitle = cumsum(titles),
    percTot = round(cumsum(wins) / (cumsum(wins) + cumsum(losses)) * 100, 2)
  )]
  
  # Add player name as a column
  res[, name := player]
  
  # Order results by year
  setorder(res, year)
  
  # Format winning percentages as strings with % sign
  res[, `:=`(
    percyear = paste0(percyear, "%"),
    percTot = paste0(percTot, "%")
  )]
  
  # Select and order columns for output
  res <- res[, .(name, year, wins, losses, percyear, titles, totalwins, totLosses, percTot, totTitle)]
  
  # Print the final results table
  print(res)
}


PercentageVsTeenagers <- function(min_matches = 20) {
  # Filter matches where the loser is a teenager (< 20 years old)
  matches_vs_teen_losers <- db[loser_age < 20]
  
  # Remove matches with walkovers, defaults, or abnormal endings
  matches_vs_teen_losers <- matches_vs_teen_losers[
    !score %in% c("W/O", "DEF") & 
      !str_detect(score, "WEA|ABN")
  ]
  
  # Count wins against teenage losers by winner
  wins <- matches_vs_teen_losers[, .N, by = winner_name]
  
  # Filter matches where the winner is a teenager (< 20 years old)
  matches_vs_teen_winners <- db[winner_age < 20]
  
  # Remove matches with walkovers, defaults, or abnormal endings
  matches_vs_teen_winners <- matches_vs_teen_winners[
    !score %in% c("W/O", "DEF") & 
      !str_detect(score, "WEA|ABN")
  ]
  
  # Count losses to teenage winners by loser
  losses <- matches_vs_teen_winners[, .N, by = loser_name]
  
  # Rename columns for merging
  setnames(wins, c("winner_name", "N"), c("name", "wins"))
  setnames(losses, c("loser_name", "N"), c("name", "losses"))
  
  # Merge wins and losses on player name, fill missing with 0
  res <- merge(wins, losses, by = "name", all = TRUE)
  res[is.na(res)] <- 0
  
  # Calculate total matches played and winning percentage against teenagers
  res[, played := wins + losses]
  
  # Filter players with at least min_matches matches vs teenagers
  res <- res[played >= min_matches]
  
  res[, percentage := wins / played * 100]
  
  # Order by winning percentage descending
  setorder(res, -percentage)
  
  # Format percentage to 2 decimal places and add '%' sign
  res[, percentage := paste0(sprintf("%.2f", percentage), "%")]
  
  print(res)
}




#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
WinningPercentageOnSurfaceOnTotal <- function(min_total_wins = 1, min_surface_wins = 1, surface = "Hard") {
  
  # Get wins on specified surface per player
  surface_wins <- winsSurface(surface)
  
  # Get overall wins per player
  overall_wins <- WinsOverall()
  
  # Filter players with minimum surface wins
  surface_wins <- surface_wins[N > min_surface_wins]
  
  # Filter players with minimum total wins
  overall_wins <- overall_wins[N > min_total_wins]
  
  # Merge surface wins with overall wins by player name
  merged <- merge(surface_wins, overall_wins, by = "winner_name", all = TRUE)
  
  # Replace NAs with zero to avoid calculation errors
  merged[is.na(merged)] <- 0
  
  # Calculate winning percentage on the surface relative to total wins
  merged[, percentage := (N.x / N.y) * 100]
  
  # Round percentage to two decimals
  merged[, percentage := round(percentage, 2)]
  
  # Order by descending winning percentage
  setorder(merged, -percentage)
  
  # Format percentage with '%' sign
  merged[, percentage := paste0(percentage, "%")]
  
  # Return the final result
  return(merged)
}



Percentage5settersPlayed <- function() {
  # Get number of 5-set matches played by each player
  five_set_stats <- Most5SetterPlayed()
  
  # Get total matches played by each player
  total_matches <- PlayedOverall()
  
  # Merge both tables on player name
  merged <- merge(five_set_stats, total_matches, by = "Player")
  
  # Calculate percentage of 5-set matches over total matches
  merged[, percentage := (N / matches) * 100]
  
  # Round to two decimal places
  merged[, percentage := round(percentage, 2)]
  
  # Sort by highest percentage
  setorder(merged, -percentage)
  
  # Append "%" symbol for display
  merged[, percentage := paste0(percentage, " %")]
  
  return(merged)
}




PercentageAtAge <- function(age = 23.78, min_matches = 30) {
  # Filter players (winners or losers) under the given age and their outcomes
  
  results <- db[, {
    players <- c(winner_name, loser_name)
    outcomes <- c("win", "loss")
    ages <- c(winner_age, loser_age)
    
    # Keep only entries where player is under the specified age
    valid_idx <- ages < age
    
    list(
      player = players[valid_idx],
      outcome = outcomes[valid_idx]
    )
  }, by = 1:nrow(db)]
  
  # Aggregate wins and losses per player
  summary <- results[, .(
    wins = sum(outcome == "win"),
    losses = sum(outcome == "loss")
  ), by = player]
  
  # Calculate total matches played
  summary[, matches_played := wins + losses]
  
  # Filter players with enough matches
  summary <- summary[matches_played >= min_matches]
  
  # Calculate win percentage
  summary[, percentage := round(wins / matches_played * 100, 2)]
  
  # Order by win percentage descending
  setorder(summary, -percentage)
  
  # Format percentage as a string
  summary[, percentage := paste0(percentage, " %")]
  
  return(summary)
}

PercentageAllSlams <- function(output_file = "Test.html") {
  
  # Remove invalid or incomplete match scores
  db <- db[
    !db$score == "W/O" & 
      !db$score == "DEF" & 
      !str_detect(db$score, "WEA") & 
      !str_detect(db$score, "ABN")
  ]
  
  # Extract year from tournament ID
  db$year <- str_sub(db$tourney_id, 1, 4)
  
  # Convert to data.table for efficient filtering
  setDT(db)
  
  # Filter only Grand Slam tournaments
  grand_slams <- db[tourney_level == 'G']
  
  # Clean up tournament names
  grand_slams$tourney_name <- gsub("-\\d+$", "", grand_slams$tourney_name)
  grand_slams$tourney_name <- trimws(grand_slams$tourney_name)
  grand_slams$tourney_name[grand_slams$tourney_name == "Australian Chps."] <- "Australian Open"
  
  # Create winner and loser dataframes
  winners <- data.frame(
    player = grand_slams$winner_name,
    tourney_name = grand_slams$tourney_name,
    year = grand_slams$year,
    win = 1
  )
  losers <- data.frame(
    player = grand_slams$loser_name,
    tourney_name = grand_slams$tourney_name,
    year = grand_slams$year,
    win = 0
  )
  
  # Combine both dataframes
  all_players <- rbind(winners, losers)
  all_players <- all_players[!is.na(all_players$player), ]
  
  # Calculate total win percentage per player
  total_stats <- aggregate(win ~ player, data = all_players, FUN = function(x) {
    round(100 * sum(x) / length(x), 2)
  })
  colnames(total_stats)[2] <- "total_win_percent"
  
  # Calculate win percentage per player per tournament
  tourney_stats <- aggregate(win ~ player + tourney_name, data = all_players, FUN = function(x) {
    round(100 * sum(x) / length(x), 2)
  })
  
  # Reshape tournament stats to wide format
  tourney_wide <- reshape(
    tourney_stats,
    idvar = "player",
    timevar = "tourney_name",
    direction = "wide"
  )
  
  # Clean column names
  colnames(tourney_wide) <- gsub("win\\.", "", colnames(tourney_wide))
  
  # Merge total and per-tournament stats
  final_output <- merge(total_stats, tourney_wide, by = "player")
  final_output <- final_output[order(-final_output$total_win_percent), ]
  
  # Keep only the top 100 players
  final_output <- head(final_output, 100)
  
  # Format numeric percentage columns with %
  percent_cols <- names(final_output)[sapply(final_output, is.numeric)]
  final_output[, percent_cols] <- lapply(final_output[, percent_cols], function(x) {
    paste0(x, "%")
  })

  return(final_output)
}




