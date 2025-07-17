library("xlsx")
library("dplyr")
library("stringr")
library(tableHTML) 
library(splitstackshape)
library(tibble)

source("Reader.R")
source("Played.R")
source("Wins.R")
source("Entries.R")
source("Timespan.R")
source("Season.R")
source("CounterSeason.R")
source("Age.R")
source("Counter.R")
source("AverageAge.R")
source("Percentage/Percentage.R")
source("AgeFormat.R")


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
MostWinsNoSlammer <- function() {
  
  res <- db[round == 'F' & tourney_level == 'G', c("winner_name")]
  slam_winners <- unique(res)
  
  wins <- WinsOverall()
  
  names(slam_winners)[1] <- "winner_name"
  names(wins)[1] <- "winner_name"
  
  # Filter out players who have won a Slam
  no_slam_winners <- subset(wins, !(winner_name %in% slam_winners$winner_name))
  
  # Optional: sort by win count
  if ("wins" %in% colnames(no_slam_winners)) {
    no_slam_winners <- no_slam_winners[order(-no_slam_winners$wins), ]
  }
  
  return(no_slam_winners)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
WinsAgainstNumber1 <- function() {
  
  # Extract year from tourney_id
  db$year <- stringr::str_sub(db$tourney_id, 0 ,4)
  
  # Remove matches that were not played (walkovers, defaults, abandoned)
  db <- db[!db$score %in% c("W/O", "DEF", "(ABN)", "ABN")]
  
  # Keep only matches where the loser was ranked #1
  db <- db[loser_rank == 1]
  
  # Count wins against world #1 by winner_name
  losses <- db[, .N, by = winner_name]
  
  # Order by most wins
  setorder(losses, -N)
  
  # Top 20 players
  losses <- losses[1:20, ]
  
  print(losses)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
Top10BeatenToWinASlam <- function() {
  
  setDT(db)
  
  # Convert to integer
  db[, loser_rank := as.integer(loser_rank)]
  db[, loser_seed := as.integer(loser_seed)]
  
  # Filter Grand Slam finals
  finals <- db[round == 'F' & tourney_level == 'G']
  
  # Get unique winners of those finals
  winners <- unique(finals[, .(tourney_id, winner_name)])
  
  # Filter matches where these winners played in those tournaments
  wins <- db[tourney_id %in% winners$tourney_id & winner_name %in% winners$winner_name]
  
  # Filter matches with losers ranked top 10
  wins_top10 <- wins[loser_rank < 11 & !is.na(loser_rank)]
  
  # Count top10 beaten per winner and tournament
  stat <- wins_top10[, .N, by = .(tourney_id, winner_name)]
  
  # Set keys for merging
  setkey(winners, tourney_id, winner_name)
  setkey(stat, tourney_id, winner_name)
  
  # Merge full list of winners with counts (N), fill missing with 0
  stat_full <- stat[winners, on = .(tourney_id, winner_name)]
  stat_full[is.na(N), N := 0]
  
  # Add official tournament names
  official_names <- unique(finals[, .(tourney_id, tourney_name)])
  stat_full <- merge(stat_full, official_names, by = "tourney_id", all.x = TRUE)
  
  # Extract year
  stat_full[, year := substr(tourney_id, 1, 4)]
  
  # Reorder columns and sort descending
  stat_full <- stat_full[, .(tourney_name, year, winner_name, N)]
  setorder(stat_full, -N)
  
  return(stat_full)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
LessWinsCategorybyWinner <- function(category) {
  # Get the winners of final matches for the given category
  winners_final <- db[round == 'F' & tourney_level == category, winner_name]
  
  # Filter all matches in the category where the winner was in the finals
  wins <- db[tourney_level == category & winner_name %in% winners_final]
  
  # Remove matches with walkovers, defaults, or abandoned scores
  wins <- wins[!score %in% c("W/O", "DEF", "(ABN)")]
  
  # Count the number of wins per winner
  res <- wins[, .N, by = winner_name]
  
  # Order results by ascending number of wins
  setorder(res, N)
  
  # Return the result table
  return(res)
}




#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
QualifiersinRound <- function() {
  
  # Filter matches where the winner was a qualifier (seed or entry 'Q') and round is Quarter-Final ('QF')
  stat <- db[(winner_seed == 'Q' | winner_entry == 'Q') & round == 'QF']
  
  # Extract year from tourney_id (assumes first 4 characters represent the year)
  stat[, year := stringr::str_sub(tourney_id, 1, 4)]
  
  # Count occurrences grouped by tournament name and year
  stat <- stat[, .N, by = .(tourney_name, year)]
  
  # Order by descending count (N)
  stat <- setorder(stat, -N, na.last = FALSE)
  
  # Return the final table
  return(stat)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
AllTournamentsRollByPlayer <- function() {
  
  # Define the player of interest
  player <- 'Rafael Nadal'
  
  # Remove team events from the database using an external function
  db <- removeTeamEvents(db)
  
  # Filter the database to keep only matches where the player was winner or loser
  db <- db[winner_name == player | loser_name == player]
  
  # Modify 'tourney_id' by removing the initial part up to the first hyphen
  # This likely normalizes the tournament ID
  db$tourney_id <- sub("^[^-]*", "", db$tourney_id)
  
  # Keep only distinct rows by 'tourney_id', keeping all columns from the first occurrence
  db <- distinct(db, tourney_id, .keep_all = TRUE)
  
  # Print the filtered dataset for inspection
  print(db)
  
  # Extract unique tournament IDs as a vector
  tour <- dplyr::pull(db, tourney_id)
  
  # Extract corresponding tournament names as a vector
  tourney_name <- dplyr::pull(db, tourney_name)
  
  # Calculate statistics for the first tournament and filter for the player
  stat <- PercentageTour(tour[1])
  stat <- stat[name == player]
  
  # Loop through remaining tournaments, calculate stats, filter for the player,
  # and append the results directly to 'stat' without using an intermediate variable
  for (i in 2:length(tour)) {
    stat <- rbind(stat, PercentageTour(tour[i])[name == player], fill = TRUE)
  }
  
  # Add tournament names as a new column right after the "name" column
  stat <- add_column(stat, tourney_name, .after = "name")
  
  # Order the dataframe by 'percentage' in descending order, placing NA values last
  stat <- setorder(stat, -percentage, na.last = FALSE)
  
  # Print the final statistics table
  print(stat)
  
}



#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
Beat123intheSametournament <- function() {
  
  # Filter the database to keep only matches where the loser had a ranking better than 3 (i.e., rank < 4)
  stat <- db[loser_rank < 4]
  
  # Load dplyr package (for data manipulation functions like right_join)
  require(dplyr)
  
  # Count the number of matches grouped by tournament ID and winner's name
  # This creates a summary table with columns: tourney_id, winner_name, and count (N)
  stat <- stat[, .N, by = list(tourney_id, winner_name)]
  
  # Filter to keep only cases where the winner beat exactly 3 such top-ranked opponents in the tournament
  stat <- stat[N == 3]
  
  # Extract unique pairs of tournament ID and tournament name from the database
  officialName <- unique(db[, c('tourney_id', 'tourney_name')])
  
  # Join the tournament names with the stats dataframe using a right join on 'tourney_id'
  # This ensures all tournaments in 'stat' have their names attached
  stat <- right_join(officialName, stat, by = "tourney_id")
  
  # Extract the year from 'tourney_id' by taking the first 4 characters (assuming tourney_id starts with year)
  stat$year <- stringr::str_sub(stat$tourney_id, 0 , 4)
  
  # Select only relevant columns: tournament name, year, and winner's name
  stat <- stat[, c("tourney_name", "year", "winner_name")]
}



#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
OldestPlayerstoWin1stTitle <- function() {
  # Remove or add team event matches from the database using an external function
  db <- removeTeamEvents(db)
  
  # Filter the database to include only final round matches ('F' for final)
  stat <- db[round == 'F']
  
  # Get an ID per winner_name to identify their order of wins (assumed function 'getanID' does this)
  stat <- getanID(stat, "winner_name")
  
  # Keep only the first win for each winner (where .id == 1)
  stat <- stat[.id == 1]
  
  # Extract the year from the tournament ID (assuming first 4 characters represent the year)
  stat$year <- stringr::str_sub(stat$tourney_id, 0 , 4)
  
  # Order the data by decreasing winner age, putting NA values last
  setorder(stat, -winner_age, na.last = FALSE)
  
  # Clean up the 'winner_age' column:
  # Take only the first 5 characters (probably to limit decimals or format)
  stat$winner_age <- substr(stat$winner_age, 0, 5)
  
  # Replace commas with dots to ensure proper numeric conversion,
  # suppressing warnings that may arise from coercion
  stat$winner_age <-
    suppressWarnings(as.numeric(
      str_replace_all(
        stat$winner_age,
        pattern = ',',
        replacement = '.'
      )
    ))
  
  # Apply additional formatting to 'winner_age' (assumed external function)
  stat <- FormatWinnerAge(stat)
  
  # Select and return only relevant columns: tournament name, year, winner name, and winner age
  stat <- stat[, c("tourney_name", "year", "winner_name", "winner_age")]
}



#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
SameNation4Semifinalists <- function() {
  
  # Remove team events matches from the database using an external function
  db <- removeTeamEvents(db)
  
  # Filter database for quarterfinal matches only
  stat <- db[round == 'QF']
  
  # Aggregate count of winners by tournament and their country code (winner_ioc)
  # This counts how many quarterfinal winners belong to each country in each tournament
  stat <- with(stat, aggregate(
    list(count = winner_ioc),
    list(tourney_id = tourney_id, winner_ioc = winner_ioc),
    length
  ))
  
  # Extract quarterfinal matches again to get official tournament info
  res <- db[round == 'QF']
  
  # Get unique tournament info: tournament id, name, date, and winner name
  officialName <- unique(res[, c('tourney_id', 'tourney_name', 'tourney_date', 'winner_name')])
  
  # Join tournament info with aggregated count data on 'tourney_id'
  stat <- left_join(officialName, stat, by = "tourney_id")
  
  # Replace NA counts with 0 to avoid issues in filtering
  stat[is.na(stat)] <- 0
  
  # Convert counts to integer type
  stat$count <- as.integer(stat$count)
  
  # Filter to keep only tournaments where count equals 4,
  # i.e., all four quarterfinal winners are from the same nation
  stat <- filter(stat, count == '4')
  
  # Remove duplicated tournament rows (in case of multiple countries per tournament)
  stat <- stat[!duplicated(stat$tourney_id), ]
  
  # Extract year from tournament ID (assuming first 4 characters represent the year)
  stat$year <- stringr::str_sub(stat$tourney_id, 0 , 4)
  
  # Select and return only the relevant columns: tournament name, year, and country code (winner_ioc)
  stat <- stat[, c("tourney_name", "year", "winner_ioc")]
  
}



#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
library(data.table)
library(stringr)

Top10inATournament <- function() {
  
  db[, winner_rank := as.integer(winner_rank)]
  db[, loser_rank := as.integer(loser_rank)]
  
  stat <- db[tourney_level == 'A']
  
  winners <- stat[winner_rank < 11, .(rank = winner_rank, tourney_id)]
  winners <- unique(winners)
  
  losers <- stat[loser_rank < 11, .(rank = loser_rank, tourney_id)]
  losers <- unique(losers)
  
  # Combine winners and losers
  stat <- rbindlist(list(winners, losers), fill = TRUE)
  stat <- unique(stat, by = c('rank', 'tourney_id'))
  
  # Count number of top 10 players per tournament
  stat <- stat[, .N, by = tourney_id]
  
  # Get tournament official names and winners
  res <- db[round == 'F' & tourney_level == 'A']
  officialName <- unique(res[, .(tourney_id, tourney_name, winner_name)])
  
  # Join official names with counts
  stat <- officialName[stat, on = "tourney_id"]
  stat[is.na(N), N := 0]
  
  # Order by decreasing number of top 10 players
  setorder(stat, -N)
  
  # Extract year from tourney_id (first 4 chars)
  stat[, year := str_sub(tourney_id, 1, 4)]
  
  # Select relevant columns
  stat <- stat[, .(tourney_name, year, N)]
  
  return(stat)
}


AllATPWinsBySurface <- function() {
  
  # Overall wins
  stat <- WinsOverall()
  
  # Wins by surface
  surfaces <- c("Hard", "Clay", "Grass", "Carpet")
  stats_surface <- lapply(surfaces, winsSurface)
  names(stats_surface) <- surfaces
  
  # Combine all data
  coll <- stat
  for (surface in surfaces) {
    coll <- left_join(coll, stats_surface[[surface]], by = "winner_name")
  }
  
  colnames(coll) <- c("player", "Overall", "Hard", "Clay", "Grass", "carpet")
  
  return(as.data.frame(coll))
}

AllRivalsForAPlayer <- function() {
  
  # Filter out matches with special scores
  db_filtered <- db[!score %in% c("W/O", "DEF", "ABN")]
  
  # Select matches involving Rafael Nadal
  stat <- db_filtered[winner_name == 'Rafael Nadal' | loser_name == 'Rafael Nadal']
  
  # Extract winners and losers with their countries
  winners <- unique(stat[, .(Player = winner_name, Flag = winner_ioc)])
  losers <- unique(stat[, .(Player = loser_name, Flag = loser_ioc)])
  
  # Combine and get unique rivals
  rivals <- unique(rbindlist(list(winners, losers)))
  
  # Filter only players from Great Britain (GBR)
  rivals <- rivals[Flag == 'GBR']
  
  # Order alphabetically by Player name
  setorder(rivals, Player)
  
  return(rivals)
}



FormerSlamChampionsRoundVS <- function() {
  # Define the round we're interested in (Finals)
  round_to_search <- 'F'
  
  # Filter only Grand Slam matches
  db <- db[tourney_level == 'G']
  
  # Get unique tournament IDs in order
  tournament_ids <- unique(db$tourney_id)
  
  # Initialize an empty list of former champions
  former_champions <- character()
  
  # Initialize result container
  results <- data.table()
  
  # Loop through tournaments except the last one
  for (i in seq_len(length(tournament_ids) - 1)) {
    current_id <- tournament_ids[i]
    next_id <- tournament_ids[i + 1]
    
    # Get winner(s) from final of current tournament
    final_match <- db[tourney_id == current_id & round == 'F', winner_name]
    former_champions <- unique(c(former_champions, final_match))
    
    # Get matches from next tournament
    next_tournament_matches <- db[tourney_id == next_id]
    
    # Filter matches where both players are former champions
    champ_vs_champ <- next_tournament_matches[
      winner_name %in% former_champions & loser_name %in% former_champions
    ]
    
    # Collect the results
    results <- rbind(results, champ_vs_champ, fill = TRUE)
  }
  
  # Keep only matches from the desired round (Finals)
  results <- results[round == round_to_search]
  
  # Extract year from tourney_id
  results[, year := substr(tourney_id, 1, 4)]
  
  # Keep only relevant columns
  results <- results[, .(
    tourney_name, year, surface, round,
    winner_name, loser_name, score
  )]
  
  return(results)
}




SlamsOnCourt <- function() {
  db <- db[tourney_level == 'G']
  tournaments <- unique(db$tourney_id)
  
  slam_counts <- list()
  former_champions <- character()
  results <- data.table()
  
  for (i in seq_len(length(tournaments) - 1)) {
    current_id <- tournaments[i]
    next_id <- tournaments[i + 1]
    
    final <- db[tourney_id == current_id & round == 'F']
    winner <- unique(final$winner_name)
    
    if (length(winner) == 1 && !is.na(winner)) {
      if (is.null(slam_counts[[winner]])) {
        slam_counts[[winner]] <- 1
      } else {
        slam_counts[[winner]] <- slam_counts[[winner]] + 1
      }
      former_champions <- unique(c(former_champions, winner))
    }
    
    matches <- db[tourney_id == next_id]
    
    champ_matches <- matches[
      winner_name %in% former_champions &
        loser_name %in% former_champions
    ]
    
    if (nrow(champ_matches) > 0) {
      # Explicit numeric conversion of title counts
      champ_matches[, winner_slam_count := as.numeric(sapply(winner_name, function(x) slam_counts[[x]]))]
      champ_matches[, loser_slam_count := as.numeric(sapply(loser_name, function(x) slam_counts[[x]]))]
      champ_matches[, total_slams := winner_slam_count + loser_slam_count]
      
      results <- rbind(results, champ_matches, fill = TRUE)
    }
  }
  
  results[, year := substr(tourney_id, 1, 4)]
  
  results <- results[, .(
    tourney_name,
    year,
    surface,
    round,
    winner_name,
    winner_slam_count,
    loser_name,
    loser_slam_count,
    total_slams,
    score
  )]
  
  return(results)
}





RoundatDebut <- function() {
  library(data.table)
  library(dplyr)
  library(stringr)
  
  turn <- 'SF'
  tournament <- 'Australian Open'
  
  db <- db[tourney_name == tournament]
  
  id <- rownames(db)
  stat <- cbind(id = id, db)
  stat$id <- as.integer(stat$id)
  
  # Prepare player match records
  wins <- stat[, c("id", "tourney_id", "tourney_name", "round", "winner_name", "score")]
  losses <- stat[, c("id", "tourney_id", "tourney_name", "round", "loser_name", "score")]
  
  wins$WL <- 'W'
  losses$WL <- 'L'
  
  names(wins)[5] <- names(losses)[5] <- "Player"
  
  stat <- rbind(wins, losses)
  setorder(stat, id, na.last = FALSE)
  
  # Track debut per player
  stat <- getanID(stat, "Player")  # Adds .id
  debut <- stat[.id == 1, .(tourney_id, tourney_name, Player)]
  
  # Find all semifinal players
  round_matches <- db[round == turn]
  
  semifinal_winners <- round_matches[, .(tourney_id, tourney_name, Player = winner_name)]
  semifinal_losers  <- round_matches[, .(tourney_id, tourney_name, Player = loser_name)]
  round <- rbind(semifinal_winners, semifinal_losers)
  
  # Remove duplicates and track debut again
  round <- unique(round)
  round <- getanID(round, "Player")
  round <- round[.id == 1, ]
  
  # Join debut and semifinal players
  debut_in_SF <- inner_join(debut, round, by = c("tourney_id", "tourney_name", "Player"))
  debut_in_SF$year <- str_sub(debut_in_SF$tourney_id, 1, 4)
  
  debut_in_SF <- debut_in_SF[, c("tourney_name", "year", "Player")]
  
  return(debut_in_SF)
}




TeenagerInARound <- function() {
  
  library(data.table)
  library(dplyr)
  library(stringr)
  
  tourney_to_search <- 'Wimbledon'
  
  round_to_search <- 'R16'
  
  # Filter matches at Wimbledon round R16
  db_filtered <- db[tourney_name == tourney_to_search & round == round_to_search]
  
  age_limit <- 20
  
  # Unique teenage winners
  wins <- db_filtered[winner_age < age_limit, .(tourney_id, player = winner_name)] %>% distinct()
  
  # Unique teenage losers
  losses <- db_filtered[loser_age < age_limit, .(tourney_id, player = loser_name)] %>% distinct()
  
  # Combine winners and losers
  res <- bind_rows(wins, losses) %>% distinct()
  
  # Count teenagers per tournament
  same <- res[, .N, by = tourney_id]
  
  # Get tournament names
  officialName <- unique(db[, .(tourney_id, tourney_name)])
  
  # Join names and counts
  same <- merge(officialName, same, by = "tourney_id", all.x = TRUE)
  
  # Replace NA counts with 0
  same$N[is.na(same$N)] <- 0
  
  # Extract year from tourney_id
  same$year <- str_sub(same$tourney_id, 1, 4)
  
  # Filter tournaments with at least one teenage player in R16
  same <- same[N > 0]
  
  # Select columns and order descending by count
  same <- same[, .(tourney_name, year, N)][order(-N)]
  
  print(same)
  
  return(same)
}


library(data.table)

Beat12ToWinSlam <- function(db) {
  
  # Ensure loser_rank is numeric
  db[, loser_rank := as.integer(loser_rank)]
  
  # Get Slam final winners with year
  finals <- db[round == 'F' & tourney_level == 'G', .(tourney_id, winner_name, year)]
  
  # All Grand Slam matches with loser ranked in top 2
  topWins <- db[tourney_level == 'G' & loser_rank < 3,
                .(tourney_id, tourney_name, winner_name, loser_rank, year)]
  
  # Join to get only matches where the Slam winner beat a top-2 player
  topBeats <- topWins[finals, on = .(tourney_id, winner_name, year)]
  
  # Count how many top-2 players each winner beat in a Slam
  result <- topBeats[, .(count = .N), by = .(tourney_name, winner_name, year)]
  
  # Sort by descending count
  setorder(result, -count, na.last = FALSE)
  
  return(result)
}


FormerSlamChampionsinASlamDraw <- function() {
  
  roundToFind <- "R128"
  
  # Get all Slam tournaments with R128 draw
  r128_matches <- db[tourney_level == "G" & round == roundToFind]
  all_tourney_ids <- unique(r128_matches$tourney_id)
  
  # Get all former Slam champions (winners in any Slam final)
  former_champions <- unique(db[round == "F" & tourney_level == "G", winner_name])
  
  # Prepare result list
  results <- list()
  
  for (tid in all_tourney_ids) {
    # Get all players in R128 (both winners and losers)
    players <- db[tourney_id == tid & round == roundToFind & tourney_level == "G",
                  .(Player = c(winner_name, loser_name))]
    
    players <- unique(players)
    
    # Filter former champions in the current draw
    former_in_draw <- players[Player %in% former_champions]
    
    # Get tournament name and year
    tinfo <- unique(db[tourney_id == tid, .(tourney_name, tourney_id)])
    tinfo[, year := substr(tourney_id, 1, 4)]
    
    # Combine results
    if (nrow(former_in_draw) > 0) {
      former_in_draw[, `:=`(tourney_name = tinfo$tourney_name, year = tinfo$year)]
      results[[length(results) + 1]] <- former_in_draw
    }
  }
  
  # Combine all into a single data.table
  all_results <- rbindlist(results)
  
  # Count how many former champions were in each tournament draw
  summary <- all_results[, .N, by = .(tourney_name, year)]
  #setorder(summary, -N)
  
  return(summary)
}

SlammerLosingOpeningSet <- function() {
  
  # Filter finals
  res <- db[round == 'F', .(tourney_id, winner_name)]
  
  # Filter Grand Slam level matches
  stat <- db[tourney_level == 'G', .(tourney_id, winner_name, loser_name, round, score)]
  
  # Merge to keep only matches involving eventual winners of the finals
  stat <- merge(stat, res, by = c("tourney_id", "winner_name"))
  
  # Keep first match played by the eventual winner (typically R128 or R64)
  stat <- stat %>%
    group_by(tourney_id) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  # Get tourney names
  officialName <- unique(db[round == 'F', .(tourney_name, tourney_id)])
  stat <- right_join(officialName, stat, by = "tourney_id")
  
  # Extract year
  stat$year <- substr(stat$tourney_id, 1, 4)
  
  # Extract first set scores
  games <- strsplit(stat$score, split = " ")
  first_set_scores <- sapply(games, function(x) sub("\\(.*", "", x[1]))
  
  # Split into games won/lost
  games_won <- as.integer(sub("-.*", "", first_set_scores))
  games_lost <- as.integer(sub(".*-", "", first_set_scores))
  
  # Add to dataframe
  stat$games_won <- games_won
  stat$games_lost <- games_lost
  
  # Determine if the first set was lost
  stat$set1stLost <- games_won < games_lost
  
  # Filter only where first set was lost
  sets <- stat[stat$set1stLost == TRUE, .(
    tourney_name, year, round, winner_name, loser_name, games_won, games_lost, score
  )]
  
  print(sets)
}



SeasonsTitlesIn3DifferentSurfaces <- function() {
  library(data.table)
  library(stringr)
  
  # Convert to data.table if not already
  db <- as.data.table(db)
  
  # Filter only final matches played on Grass, Clay, or Hard courts
  stat <- db[round == 'F' & surface %in% c('Grass', 'Clay', 'Hard')]
  
  # Extract the year from the tournament ID (first 4 characters)
  stat[, year := str_sub(tourney_id, 1, 4)]
  
  # Count the number of wins for each player per year on each surface
  stat_summary <- stat[, .(n = .N), by = .(year, winner_name, surface)]
  
  # Calculate how many unique surfaces each player won on per year
  surfaces_per_player <- stat_summary[, .(surfaces_won = uniqueN(surface)), by = .(year, winner_name)]
  
  # Keep only players who won on all three surfaces in the same year
  result <- surfaces_per_player[surfaces_won == 3]
  
  # Order the results by winner name descending
  setorder(result, -winner_name, na.last = FALSE)
  
  return(result)
}


AverageHeight <- function(stage) {
  library(data.table)
  library(stringr)
  
  # Filter rows based on stage:
  # - 'W' means finals ('F')
  # - '0' means no filtering on round
  # - else filter by the given round
  filtered <- if(stage == 'W') {
    db[round == 'F']
  } else if(stage == '0') {
    db
  } else {
    db[round == stage]
  }
  
  # Extract unique winner heights per tournament
  winners <- unique(filtered[, .(tourney_id, height = as.integer(winner_ht))])
  
  # Extract unique loser heights if stage is not 'W'
  if(stage != 'W') {
    losers <- unique(filtered[, .(tourney_id, height = as.integer(loser_ht))])
    # Combine winners and losers heights
    heights <- unique(rbindlist(list(winners, losers)))
  } else {
    heights <- winners
  }
  
  # Calculate average height per tournament
  avg_height <- heights[, .(mean_height = mean(height, na.rm = TRUE)), by = tourney_id]
  
  # Add official tournament names from finals data
  finals <- unique(db[round == 'F', .(tourney_id, tourney_name)])
  avg_height <- merge(finals, avg_height, by = "tourney_id", all.x = TRUE)
  
  # Extract year from tournament ID (first 4 characters)
  avg_height[, year := str_sub(tourney_id, 1, 4)]
  
  # Format mean_height to string with comma as decimal separator
  avg_height[, mean_height := sprintf("%.2f", mean_height)]
  avg_height[, mean_height := gsub("\\.", ",", mean_height)]
  
  # Order by mean height descending
  setorder(avg_height, -mean_height)
  
  # Select relevant columns
  result <- avg_height[, .(tourney_name, year, mean_height)]
  
  print(result)
}


DifferentFinalistsInM1000InASeason <- function() {
  library(data.table)
  library(stringr)
  
  # Filter semifinals ('SF') at Masters 1000 level ('M')
  stat <- db[round == 'SF' & tourney_level == 'M']
  
  # Extract year from tournament ID (first 4 characters)
  stat[, year := str_sub(tourney_id, 1, 4)]
  
  # Count unique winners per year in SF round
  winners_per_year <- stat[, .N, by = .(year, winner_name)]
  
  # Count how many different winners appeared in SF per year
  result <- winners_per_year[, .(different_finalists = .N), by = year]
  
  return(result)
}

MostWinsWithNoTitleInASeasons <- function() {
  
  # Ensure db is a data.table
  db <- as.data.table(db)
  
  # Extract year from tournament ID
  db[, year := str_sub(tourney_id, 1, 4)]
  
  # Get all players who won at least one final (i.e., won a title), per year
  title_winners <- unique(db[round == 'F', .(year, winner_name)])
  
  # Remove walkovers, defaults, or incomplete matches
  clean_matches <- db[
    !score %in% c("W/O", "DEF") &
      !str_detect(score, "WEA") &
      !str_detect(score, "ABN")
  ]
  
  # Filter only players who did NOT win a title that year
  # Anti-join equivalent: keep (year, winner_name) not in title_winners
  clean_matches <- clean_matches[!paste(year, winner_name) %in% paste(title_winners$year, title_winners$winner_name)]
  
  # Count match wins per player per year
  result <- clean_matches[, .N, by = .(year, winner_name)]
  
  # Order by year, then by descending win count
  setorder(result, -N)
  
  return(result)
}


BestPercentageInASeasonYearByYear <- function() {
  library(data.table)
  library(lubridate)
  
  years <- 1968:year(now())  # Years from Open Era to current
  results <- list()          # Use list for faster accumulation
  
  for (i in years) {
    cat("Processing year:", i, "\n")
    
    # Get win percentage stats for year i
    stat <- PercentageYearByYear(i, db)
    
    if (nrow(stat) > 0) {
      # Take top player and add year column
      top_player <- head(stat, 1)
      top_player$year <- i
      
      results[[length(results) + 1]] <- top_player
    }
  }
  
  # Combine all rows into one data.table
  seasons <- rbindlist(results, fill = TRUE)
  
  # Optional: sort by year (ascending)
  setorder(seasons, year)
  
  return(seasons)
}



OldestNo1SeedinSlams <- function(){
  
  #extract year from tourney_date
  db$year <- stringr::str_sub(db$tourney_id, 0 ,4)
  
  winners <- db[winner_seed == '1' & tourney_level == 'G']
  
  losers <- db[loser_seed == '1' & tourney_level == 'G']
  
  winners <- unique(winners[, c('tourney_name', 'year', 'winner_name', 'winner_age')])
  losers <- unique(losers[, c('tourney_name', 'year', 'loser_name', 'loser_age')])
  
  names(winners)[3] <- names(losers)[3] <- "player"
  names(winners)[4] <- names(losers)[4] <- "age"
  
  stat <-  rbind(winners, losers, fill = TRUE)
  
  stat <- unique(stat[, c('tourney_name', 'year', 'player', 'age')])
  
  ## order by decreasing
  setorder(stat, -age, na.last=FALSE)
  
  stat <- FormatAge(stat)
}

OverAgedInDraw <- function(age = 38) {
  library(data.table)
  library(stringr)
  
  # Filter matches from Wimbledon
  stat <- db[tourney_name == "Wimbledon"]
  
  # Extract all years from tourney_id (assumes year is first 4 chars)
  all_years <- unique(str_sub(stat$tourney_id, 1, 4))
  
  # Get over-aged winners
  winners <- stat[winner_age > age, .(age = winner_age, player = winner_name, tourney_id)]
  
  # Get over-aged losers
  losers <- stat[loser_age > age, .(age = loser_age, player = loser_name, tourney_id)]
  
  # Combine and deduplicate players per tournament
  combined <- unique(rbind(winners, losers))
  
  # Count number of over-aged players per tournament
  stat_count <- combined[, .N, by = .(tourney_id)]
  
  # Extract year from tourney_id
  stat_count[, year := str_sub(tourney_id, 1, 4)]
  
  # Create data.table with all years (even if no over-aged players)
  all_years_dt <- data.table(year = all_years)
  
  # Merge counts with all years to include years with zero counts
  stat_complete <- merge(all_years_dt, stat_count[, .(year, N)], by = "year", all.x = TRUE)
  
  # Replace NA with zero (years with zero over-aged players)
  stat_complete[is.na(N), N := 0]
  
  # Add tournament name column
  stat_complete[, tourney_name := "Wimbledon"]
  
  # Sort by year ascending
  setorder(stat_complete, year)
  
  # Return final summary
  return(stat_complete[, .(tourney_name, year, N)])
}



SimpsonParadox <- function(){
  
  db[is.na(db)] <- 0
  
  stat <- WinsOverall()
  
  stat <- stat[N > 2]
  
  players <- unique(dplyr::pull(stat, winner_name))
  
  rows <- NULL
  
  for(i in 1:length(players)){
    
    player <- players[i]  
    
    print(player)
    
    stat <- db[loser_name == player]
    
    stat <-  stat[, w_points := as.integer(stat$w_1stWon) + as.integer(stat$w_2ndWon) +  (as.integer(stat$l_svpt) - as.integer(stat$l_1stWon) - as.integer(stat$l_2ndWon))]
    
    stat <-  stat[, l_points := as.integer(stat$l_1stWon) + as.integer(stat$l_2ndWon) +  (as.integer(stat$w_svpt) - as.integer(stat$w_1stWon) - as.integer(stat$w_2ndWon))]
    
    stat <-  stat[l_points > w_points]
    
    #print(stat)
    
    if(nrow(stat) > 1){
      stat <- getanID(stat, "loser_name")
      
      stat2 <- stat[, c(".id", "loser_name")]
      
      #library(tidyverse)
      stat2 <- stat2 %>%
        slice_tail(n = 1)
      
      rows <- rbind(rows, stat2)
      
    }
    
  }
  stat <- rows  
  
  setorder(stat, -.id, na.last=FALSE)
}
