MostRecurrentH2HinASeason <- function() {
  library(stringr)
  library(data.table)
  library(plyr)
  library(splitstackshape)
  
  # Remove matches with walkovers, defaults, or abandonments
  db <- db[!db$score %in% c("W/O", "DEF", "(ABN)"), ]
  
  # Extract relevant columns
  h2h <- db[, c("winner_name", "loser_name", "tourney_id")]
  
  # Extract year from tourney_id (assumes first 4 characters are year)
  h2h$year <- str_sub(h2h$tourney_id, 1, 4)
  
  # Create match identifiers for both directions: winner-loser and loser-winner
  h2h$match1 <- paste(h2h$winner_name, h2h$loser_name, h2h$year, sep = " - ")
  h2h$match2 <- paste(h2h$loser_name, h2h$winner_name, h2h$year, sep = " - ")
  
  # Combine both directions for each match
  all_matches <- unlist(mapply(c, h2h$match1, h2h$match2, SIMPLIFY = FALSE))
  
  # Count occurrences of each match
  match_counts <- as.data.frame(table(all_matches), stringsAsFactors = FALSE)
  names(match_counts) <- c("match", "N")
  
  # Filter to matches with more than 5 occurrences
  match_counts <- subset(match_counts, N > 5)
  
  # Split match string into player1, player2, year
  match_split <- cSplit(match_counts, "match", " - ")
  result <- match_split[, c("match_1", "match_2", "match_3", "N")]
  names(result) <- c("player_1", "player_2", "year", "N")
  
  # Remove duplicate pairs (ignoring order of players)
  result <- result[!duplicated(t(apply(result[, 1:2], 1, sort))), ]
  
  print(result)
  return(result)
}


MostRecurrentH2HinARound <- function(round_filter = 'F', min_matches = 5) {
  library(data.table)
  library(plyr)
  library(splitstackshape)
  
  # Filter the dataset for the specified round (e.g., 'F')
  db <- db[round == round_filter]
  
  # Remove matches with walkovers, defaults, or abandonments
  db <- db[!score %in% c("W/O", "DEF", "(ABN)")]
  
  # Create ordered pairs of players to treat player1 vs player2
  # the same as player2 vs player1 (to avoid duplicates)
  h2h <- db[, .(player1 = pmin(winner_name, loser_name),
                player2 = pmax(winner_name, loser_name))]
  
  # Create a combined key for the pair (without round since it's fixed)
  h2h[, match := paste(player1, player2, sep = " - ")]
  
  # Count the number of occurrences for each player pair
  counts <- h2h[, .N, by = match]
  
  # Keep only pairs with occurrences above the minimum threshold
  counts <- counts[N > min_matches]
  
  # Split the combined match key back into separate player columns
  counts <- cSplit(counts, "match", " - ")
  setnames(counts, c("match_1", "match_2", "N"), c("player1", "player2", "count"))
  
  # Add a 'round' column with the fixed round_filter value
  counts[, round := round_filter]
  
  # Reorder columns as: player1, player2, round, count
  counts <- counts[, .(player1, player2, round, count)]
  
  # Order rows by descending count (most frequent matches first)
  setorder(counts, -count)
  
  # Print the result and return it
  print(counts)
  return(counts)
}






MostRecurrentH2HinSurface <- function(surface_filter = 'Hard', min_matches = 5) {
  library(data.table)
  library(plyr)
  library(splitstackshape)
  
  # Filter matches for the selected surface (e.g., "Clay")
  db <- db[surface == surface_filter]
  
  # Remove walkovers, defaults, abandoned matches
  db <- db[!score %in% c("W/O", "DEF", "(ABN)")]
  
  # Create ordered player pairs to make A-B same as B-A
  h2h <- db[, .(player1 = pmin(winner_name, loser_name),
                player2 = pmax(winner_name, loser_name))]
  
  # Create a unique match key
  h2h[, match := paste(player1, player2, sep = " - ")]
  
  # Count the number of occurrences per matchup
  counts <- h2h[, .N, by = match]
  
  # Keep only matchups that occurred more than min_matches times
  counts <- counts[N > min_matches]
  
  # Split back player1 and player2 from match string
  counts <- cSplit(counts, "match", " - ")
  setnames(counts, c("match_1", "match_2", "N"), c("player1", "player2", "count"))
  
  # Add surface column with fixed value
  counts[, surface := surface_filter]
  
  # Reorder columns
  counts <- counts[, .(player1, player2, surface, count)]
  
  # Sort by count descending
  setorder(counts, -count)
  
  # Print and return result
  print(counts)
  return(counts)
}



MostRecurrentH2HinCategory <- function(category_filter = 'G', min_matches = 5) {
  library(data.table)
  library(plyr)
  library(splitstackshape)
  
  # Filter matches for the selected tournament category (e.g., 'G' for Grand Slam)
  db <- db[tourney_level == category_filter]
  
  # Remove invalid matches (walkover, default, abandoned)
  db <- db[!score %in% c("W/O", "DEF", "(ABN)")]
  
  # Create ordered head-to-head pairs (A vs B = B vs A)
  h2h <- db[, .(player1 = pmin(winner_name, loser_name),
                player2 = pmax(winner_name, loser_name))]
  
  # Create unique match string
  h2h[, match := paste(player1, player2, sep = " - ")]
  
  # Count occurrences of each matchup
  counts <- h2h[, .N, by = match]
  
  # Filter only matchups with more than min_matches
  counts <- counts[N > min_matches]
  
  # Split match string back to player1 and player2
  counts <- cSplit(counts, "match", " - ")
  setnames(counts, c("match_1", "match_2", "N"), c("player1", "player2", "count"))
  
  # Add fixed category column
  counts[, category := category_filter]
  
  # Reorder columns: player1, player2, category, count
  counts <- counts[, .(player1, player2, category, count)]
  
  # Sort by descending count
  setorder(counts, -count)
  
  # Print and return result
  print(counts)
  return(counts)
}


MostRecurrentH2HinSameTournament <- function() {
  library(data.table)
  
  # Convert to data.table (if not already)
  setDT(db)
  
  # Filter only Grand Slam matches and exclude walkovers, defaults, abandonments
  db <- db[tourney_level == 'G' & !score %in% c("W/O", "DEF", "(ABN)")]
  
  # Clean tournament ID by removing the year prefix (e.g. "2023-WIMBLEDON" -> "WIMBLEDON")
  db[, tourney_id_clean := sub("^[0-9]+-", "", tourney_id)]
  
  # Create player pairs in alphabetical order to avoid duplicate reverse pairs
  db[, player1 := pmin(winner_name, loser_name)]
  db[, player2 := pmax(winner_name, loser_name)]
  
  # Count how many times each pair met in the same tournament
  h2h_counts <- db[, .N, by = .(player1, player2, tourney_id_clean)]
  
  # Keep only pairs that met more than once in the same tournament
  h2h_counts <- h2h_counts[N > 1]
  
  # Extract unique tournament names with cleaned tournament IDs
  tourney_names <- unique(db[, .(tourney_id_clean, tourney_name)])
  
  # Ensure there are no duplicate tournament IDs before join
  tourney_names <- tourney_names[!duplicated(tourney_id_clean)]
  
  # Join the counts with tournament names by cleaned tournament ID
  result <- merge(h2h_counts, tourney_names, by = "tourney_id_clean", all.x = TRUE)
  
  # Order the results by decreasing number of matches
  setorder(result, -N)
  
  # Select and arrange final columns to display
  result <- result[, .(player1, player2, tourney_name, N)]
  
  # Print the final table
  print(result)
  
  # Return the final data.table
  return(result)
}

virginH2H2 <- function() {
  library(data.table)
  library(dplyr)  # For arrange
  
  setDT(db)
  
  # Remove matches with walkovers, defaults, abandonments
  db <- db[!score %in% c("W/O", "DEF", "(ABN)")]
  
  # Create pairs of players
  h2h <- db[, .(winner_name, loser_name)]
  
  # Create 'match' strings "Winner - Loser"
  h2h[, match := paste(winner_name, loser_name, sep = " - ")]
  # Create 'reverse' strings "Loser - Winner"
  h2h[, reverse := paste(loser_name, winner_name, sep = " - ")]
  
  # Keep only those matches where the reverse pair does NOT exist in the dataset
  # i.e., player pairs that have not played the reverse matchup before
  virgin_h2h <- h2h[!(reverse %in% match)]
  
  # Count occurrences of each unique match (should be 1 if virgin)
  out <- virgin_h2h[, .N, by = match]
  
  # Order descending by count (N)
  out <- arrange(out, desc(N))
  
  print(out)
  return(out)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
H2HTimespan <- function() {
  library(data.table)
  library(lubridate)
  
  setDT(db)
  
  # Create consistent H2H pair IDs with alphabetical ordering of player names
  db[, h2h := paste(pmin(winner_name, loser_name), pmax(winner_name, loser_name), sep = " - ")]
  
  # Extract unique matches with tournament info and winner age
  wins <- unique(db[, .(h2h, tourney_name, tourney_date, winner_age)])
  
  # Rename columns for clarity
  setnames(wins, c("h2h", "winner_age"), c("name", "age"))
  
  # Convert tournament date to Date format
  wins[, tourney_date := ymd(tourney_date)]
  
  # Order by pair and tournament date ascending
  setorder(wins, name, tourney_date)
  
  # For each pair, get the first and last tournament entries (date and name)
  first_last <- wins[, .SD[c(1, .N)], by = name]
  
  # Separate first and last records for merging
  first_date <- first_last[, .SD[1], by = name]
  last_date <- first_last[, .SD[2], by = name]
  
  # Combine first and last tournament info side by side
  timespan <- cbind(
    first_date[, .(name, first_tournament = tourney_name, first_date = tourney_date)],
    last_tournament = last_date$tourney_name,
    last_date = last_date$tourney_date
  )
  
  # Calculate timespan in days between first and last meeting
  timespan[, Days := as.numeric(difftime(last_date, first_date, units = "days"))]
  
  # Order pairs by descending timespan
  setorder(timespan, -Days)
  
  print(timespan)
  return(timespan)
}


VirginH2HByAge <- function() {
  
  max_age <- 23.436
  dt <- data.table(db)
  
  # Convert age to numeric if necessary
  dt[, winner_age := as.numeric(winner_age)]
  
  results <- lapply(unique(dt$winner_name), function(player_name) {
    young_wins <- dt[winner_name == player_name & winner_age < max_age]
    beaten_opponents <- unique(young_wins$loser_name)
    lost_matches <- dt[loser_name == player_name]
    unbeaten_opponents <- setdiff(beaten_opponents, unique(lost_matches$winner_name))
    
    if(length(unbeaten_opponents) > 0) {
      unbeaten_h2h <- young_wins[loser_name %in% unbeaten_opponents]
      h2h_count <- unbeaten_h2h[, .(match_count = .N), by = .(loser_name)]
      h2h_count[, winner_name := player_name]
      return(h2h_count[, .(winner_name, loser_name, match_count)])
    } else {
      return(NULL)
    }
  })
  
  # Combine all results, removing NULL entries
  final_results <- rbindlist(results[!sapply(results, is.null)])
  
  # Sorting by match count in descending order
  setorder(final_results, -match_count)
  
  return(final_results)
}

H2HByPlayer <- function(){
  
  player_name <- 'Roger Federer'
  
  # Filter matches where the player was either the winner or the loser
  player_matches <- db[db$winner_name == player_name | db$loser_name == player_name, ]
  
  # Identify all unique opponents (excluding the player themself)
  opponents <- unique(setdiff(c(player_matches$winner_name, player_matches$loser_name), player_name))
  
  # Calculate head-to-head statistics against each opponent
  h2h_results <- do.call(rbind, lapply(opponents, function(opponent) {
    # Filter matches between the player and the current opponent
    matches_against_opponent <- player_matches[
      (player_matches$winner_name == player_name & player_matches$loser_name == opponent) |
        (player_matches$winner_name == opponent & player_matches$loser_name == player_name), ]
    
    # Count number of wins and losses for the player
    wins <- sum(matches_against_opponent$winner_name == player_name)
    losses <- sum(matches_against_opponent$loser_name == player_name)
    total_matches <- wins + losses
    
    # Calculate win percentage (avoid division by zero)
    win_percentage <- if (total_matches > 0) (wins / total_matches) * 100 else 0
    
    # Return a data frame with stats for this opponent
    data.frame(
      opponent = opponent,
      wins = wins,
      losses = losses,
      win_percentage = paste0(round(win_percentage, 2), "%")
    )
  }))
  
  # Sort the results by win percentage in descending order
  h2h_results <- h2h_results[order(as.numeric(gsub("%", "", h2h_results$win_percentage)), decreasing = TRUE), ]
  
  return(h2h_results)
}


BeatSamePlayer <- function() {
  
  player_name <- 'Roger Federer'
  
  # Filter matches where the player lost
  matches_lost <- db[loser_name == player_name]
  
  # Count how many times each opponent beat the player
  res <- matches_lost[, .N, by = winner_name]
  
  # Order results by descending number of wins against the player
  setorder(res, -N)
  
  # Print the result table
  print(res)
}
