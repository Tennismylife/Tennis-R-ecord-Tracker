WinnerInATourByNation <- function(nation_code = 'ITA', tourney_suffix = '-580') {
  
  # Filter matches where the winner's nationality matches the specified code
  filtered_db <- db[winner_ioc == nation_code]
  
  # Further filter tournaments whose IDs contain the specified suffix
  filtered_db <- filtered_db[grepl(tourney_suffix, tourney_id)]
  
  # Extract unique winner names from the filtered matches
  unique_winners <- unique(filtered_db[, .(winner_name)])
  
  # Return the unique winners
  return(unique_winners)
}



TitlesByNation <- function() {
  # Filter matches that are finals
  finals <- db[round == 'F']
  
  # Exclude matches with abandonment or walkover scores
  finals <- finals[score != 'ABN' & score != '(ABN)' & !str_detect(score, "WEA")]
  
  # Remove team events from the dataset (assuming this function is defined)
  finals <- removeTeamEvents(finals)
  
  # Count number of titles won by each nation (winner_ioc)
  res <- finals[, .N, by = winner_ioc]
  
  # Order results by descending number of titles
  setorder(res, -N)
  
  # Return the result table
  return(res)
}



PercentageAgainstANationbyPlayer <- function(player_name = 'Novak Djokovic', min_games = 15) {
  library(data.table)
  library(plyr)  # for count(), or alternatively you can use data.table syntax
  
  # Count wins per player against opponents' nations (loser_ioc)
  stat_wins <- count(db, c("winner_name", "loser_ioc"))
  
  # Count losses per player against opponents' nations (winner_ioc)
  stat_losses <- count(db, c("loser_name", "winner_ioc"))
  
  # Rename columns for easier merging
  names(stat_wins)[1:2] <- c("name", "flag")
  names(stat_losses)[1:2] <- c("name", "flag")
  
  # Merge wins and losses by player and opponent nation
  stat <- merge(stat_wins, stat_losses, by = c("name", "flag"), all = TRUE)
  
  # Replace NAs with zeros (no wins or losses)
  stat[is.na(stat)] <- 0
  
  # Rename count columns to wins and losses
  names(stat)[3:4] <- c("wins", "losses")
  
  # Convert to data.table for further processing
  stat <- as.data.table(stat)
  
  # Filter players with at least min_games played against that nation
  stat <- stat[(wins + losses) > min_games]
  
  # Calculate win percentage against that nation
  stat[, percentage := (wins / (wins + losses)) * 100]
  
  # Order by decreasing win percentage
  setorder(stat, -percentage, na.last = FALSE)
  
  # Filter results to only the specified player
  stat <- stat[name == player_name]
  
  # Take top 100 records if more than 100 exist
  stat <- stat[1:min(100, .N)]
  
  # Return the final table
  return(stat)
}



RoundCollectedYearByYear <- function(nation_code = 'ITA') {
  library(stringr)
  library(dplyr)
  
  # Remove team events from the dataset
  filtered_db <- removeTeamEvents(db)
  
  # Exclude matches with walkovers, defaults, abandonments, or retirements
  filtered_db <- filtered_db[!score %in% c("W/O", "DEF") & 
                               !str_detect(score, "WEA") & 
                               !str_detect(score, "ABN")]
  
  # Create a data frame of years from 1968 to 2025
  years <- data.frame(year = 1968:2025)
  
  # Filter matches where winner's nationality matches the input nation
  filtered_db <- filtered_db[winner_ioc == nation_code]
  
  # Filter matches that reached the final round
  filtered_db <- filtered_db[round == 'F']
  
  # Extract year from the first 4 characters of tourney_id (fix indexing)
  filtered_db[, year := as.integer(str_sub(tourney_id, 1, 4))]
  
  # Group by year and count matches per year (number of finals won by the nation)
  stat <- filtered_db %>%
    group_by(year) %>%
    tally(name = "count")
  
  # Join with full year range to include years with zero matches
  stat <- left_join(years, stat, by = "year")
  
  # Replace NA counts with zero
  stat$count[is.na(stat$count)] <- 0
  
  # Return the final table with years and counts
  return(stat)
}


WinsByANationInCategory <- function(year = '2024', category = 'M') {
  library(stringr)
  library(dplyr)
  
  # Filter matches by tournament level (e.g., 'M' for Masters)
  stat <- db[tourney_level == category]
  
  # Exclude walkovers, defaults, abandonments, or retirements
  stat <- stat[!score %in% c("W/O", "DEF") & 
                 !str_detect(score, "WEA") & 
                 !str_detect(score, "ABN")]
  
  # Extract year from the first 4 characters of tourney_id
  stat <- stat %>% 
    mutate(year = str_sub(tourney_id, 1, 4))
  
  # Filter by the specified year
  stat <- stat %>% filter(year == year)
  
  # Group by winner's nation and count wins
  result <- stat %>% 
    group_by(winner_ioc) %>% 
    tally(name = "wins") %>%
    arrange(desc(wins))
  
  # Return the summarized data
  return(result)
}



NfromANationinRound <- function(){
  
  db <- db[tourney_level == 'G']
  
  # Funzione per contare vincitori per nazionalità, turno, torneo e anno
  count_nationality_per_tourney <- function(data, round_value, nationality_code) {
    data$year <- as.integer(gsub("([0-9]{4}).*", "\\1", data$tourney_id))
    filtered_data <- data[data$round == round_value & data$winner_ioc == nationality_code, ]
    counts <- as.data.frame(table(filtered_data$tourney_name, filtered_data$year))
    colnames(counts) <- c("tourney_name", "year", "count")
    counts$year <- as.integer(as.character(counts$year))
    counts$tourney_name <- as.character(counts$tourney_name)
    counts <- counts[order(counts$count, counts$year, counts$tourney_name, decreasing = TRUE), ]
    rownames(counts) <- NULL
    return(counts)
  }
  
  # Esempio: vincitori italiani al primo turno (R64)
  result <- count_nationality_per_tourney(db, "R64", "USA")
  
}