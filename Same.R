library(stringr)

SameTournamentRound <- function(stage) {
  
  library(data.table)
  
  # Remove team events and keep only Grand Slam tournaments
  db <- removeTeamEvents(db)
  db <- db[tourney_level == 'G']
  
  # Filter matches by specific round (if not 'W' or '0')
  if (stage != 'W' & stage != '0') {
    db <- db[round == stage]
  }
  
  # If stage is 'W', keep only final round (to get the winners)
  if (stage == 'W') {
    db <- db[round == 'F']
  }
  
  # Create table of winners
  wins <- db[, .(name = winner_name, tourney_id, tourney_name)]
  wins[, tourney_id := sub("^[^-]*-", "", tourney_id)]
  
  # If not only winners, also include losers
  if (stage != 'W') {
    losses <- db[, .(name = loser_name, tourney_id, tourney_name)]
    losses[, tourney_id := sub("^[^-]*-", "", tourney_id)]
    res <- rbindlist(list(wins, losses), use.names = TRUE, fill = TRUE)
  } else {
    res <- wins
  }
  
  # Count player appearances by tournament
  appearances <- res[, .N, by = .(Player = name, tourney_id, tourney_name)]
  
  # Remove duplicates (if any), sort by count descending
  appearances <- unique(appearances, by = c("Player", "tourney_id"))
  appearances <- appearances[order(-N)]
  
  # Keep only tourney_name, Player, and count
  appearances <- appearances[, .(Player, tourney_name, N)]
  
  # Limit to top 30
  appearances <- head(appearances, 30)
  
  return(appearances)
}



SameTournamentEntries <- function() {
  
  library(data.table)
  
  # Remove team events
  db <- removeTeamEvents(db)
  
  # Remove walkovers, defaults, and abandonments
  db <- db[!score %in% c("W/O", "DEF", "(ABN)")]
  
  # Get winner and loser entries
  wins <- unique(db[, .(name = winner_name, tournament = tourney_name, date = tourney_date)])
  losses <- unique(db[, .(name = loser_name, tournament = tourney_name, date = tourney_date)])
  
  # Combine all entries (each row = one player in one tournament edition)
  entries <- rbindlist(list(wins, losses), use.names = TRUE)
  
  # Remove duplicate player-tournament-date entries
  entries <- unique(entries)
  
  # Count how many times each player has appeared in the same tournament
  appearances <- entries[, .N, by = .(Player = name, Tournament = tournament)]
  
  # Order by most frequent entries
  appearances <- appearances[order(-N)]
  
  # Limit to top 70
  appearances <- head(appearances, 70)
  
  return(appearances)
}



SameSurfaceEntries <- function() {
  
  library(data.table)
  
  # Remove team events
  db <- removeTeamEvents(db)
  
  # Exclude walkovers, defaults, and abandoned matches
  db <- db[!score %in% c("W/O", "DEF", "(ABN)")]
  
  # Create winner and loser entry tables
  wins <- unique(db[, .(name = winner_name, tournament = tourney_name, date = tourney_date, surface)])
  losses <- unique(db[, .(name = loser_name, tournament = tourney_name, date = tourney_date, surface)])
  
  # Combine all entries (each row = player entry into a tournament edition)
  entries <- rbindlist(list(wins, losses), use.names = TRUE)
  
  # Remove duplicates (same player in same tournament edition)
  entries <- unique(entries)
  
  # Count appearances by surface
  appearances <- entries[, .N, by = .(Player = name, Surface = surface)]
  
  # Sort by highest number of entries
  appearances <- appearances[order(-N)]
  
  # Keep top 20
  appearances <- head(appearances, 20)
  
  return(appearances)
}


SameSurfaceRound <- function(stage) {
  
  library(data.table)
  library(stringr)
  
  # Remove team events and unwanted score types
  db <- removeTeamEvents(db)
  db <- db[!score %in% c("ABN", "(ABN)") & !str_detect(score, "\\(WEA\\)")]
  
  # Filter matches by round
  if (stage != 'W' & stage != '0') {
    db <- db[round == stage]
  }
  if (stage == 'W') {
    db <- db[round == 'F']
  }
  
  # Get winner entries
  wins <- db[, .(name = winner_name, tourney_id, surface)]
  
  # Get loser entries only if not 'W' stage
  if (stage != 'W') {
    losses <- db[, .(name = loser_name, tourney_id, surface)]
  }
  
  # Clean tourney_id to remove prefix
  wins[, tourney_id := sub("^[^-]*-", "", tourney_id)]
  if (stage != 'W') {
    losses[, tourney_id := sub("^[^-]*-", "", tourney_id)]
    res <- rbindlist(list(wins, losses), use.names = TRUE)
  } else {
    res <- wins
  }
  
  # Count appearances per surface
  surface_counts <- res[, .N, by = .(Player = name, Surface = surface)]
  
  # Order and limit to top 20
  surface_counts <- surface_counts[order(-N)]
  surface_counts <- head(surface_counts, 20)
  
  return(surface_counts)
}



SameTournamentPlayed <- function() {
  
  library(data.table)
  
  db <- removeTeamEvents(db)  # Assume this function is defined elsewhere
  
  db <- db[tourney_level == 'G']
  
  db <- db[!(score %in% c("W/O", "ABN", "(ABN)")) & !grepl("\\(WEA\\)", score)]
  
  # Tournaments won
  wins <- db[, .(name = winner_name, id = tourney_id, tournament = tourney_name)]
  
  # Tournaments lost
  losses <- unique(db[, .(name = loser_name, id = tourney_id, tournament = tourney_name)])
  
  # Merge wins and losses
  res <- rbind(wins, losses)
  
  # Count number of times each player played each tournament
  same <- res[, .N, by = .(name, tournament)][order(-N)]
  
  # Rename columns
  setnames(same, c("name", "tournament", "N"), c("Player", "Tournament", "N"))
  
  # Top 20
  same <- same[1:20]
  
  print(same)
}



SameSurfacePlayed <- function() {
  
  library(data.table)
  
  # Filter out unwanted scores
  db <- db[!(score %in% c("W/O", "ABN", "(ABN)")) & !grepl("\\(WEA\\)", score)]
  
  # Tournaments won
  wins <- db[, .(name = winner_name, id = tourney_id, tournament = tourney_name, surface)]
  
  # Tournaments lost
  losses <- unique(db[, .(name = loser_name, id = tourney_id, tournament = tourney_name, surface)])
  
  # Combine wins and losses
  res <- rbindlist(list(wins, losses), use.names = TRUE, fill = TRUE)
  
  # Count number of matches per player per surface
  same <- res[, .N, by = .(name, surface)][order(-N)]
  
  # Rename columns for clarity
  setnames(same, c("name", "surface", "N"), c("Player", "Surface", "N"))
  
  # Select top 20 players by matches played on a surface
  same <- same[1:20]
  
  print(same)
}



SameSeasonPlayed <- function() {
  
  library(data.table)
  library(stringr)
  
  # Filter unwanted scores
  db_filtered <- db[!(score %in% c("W/O", "ABN", "(ABN)")) & !grepl("\\(WEA\\)", score)]
  
  # Tournaments won
  wins <- db_filtered[, .(name = winner_name, id = tourney_id)]
  
  # Tournaments lost
  losses <- db_filtered[, .(name = loser_name, id = tourney_id)]
  
  # Combine wins and losses
  res <- rbind(wins, losses)
  
  # Extract year from tourney_id (assuming format starts with year)
  res[, year := str_sub(id, 1, 4)]
  
  # Count number of matches played by player per year
  same <- res[, .N, by = .(name, year)][order(-N)]
  
  # Rename columns
  setnames(same, c("name", "year", "N"), c("Player", "Season", "N"))
  
  # Print top 20 by default
  print(same[1:20])
}



SameTournamentWins <- function() {
  
  # Filter out matches with walkovers, abandonments, or withdrawals
  db_filtered <- db[!db$score %in% c("W/O", "ABN", "(ABN)")]
  db_filtered <- db_filtered[!str_detect(db_filtered$score, "(WEA)")]
  
  # Select winner name and tournament info
  wins <- db_filtered[, c('winner_name', 'tourney_id', 'tourney_name')]
  
  # Simplify the tournament ID by removing everything before the first dash (including the dash)
  # You might want to check this regex depending on the format of your IDs
  wins$tourney_id <- sub("^[^-]*-", "", wins$tourney_id)
  
  # Rename columns for convenience
  names(wins) <- c("Player", "tourney_id", "Tournament")
  
  # Count number of wins per player per tournament
  win_counts <- wins[, .N, by = .(Player, tourney_id, Tournament)]
  
  # Order by decreasing number of wins
  win_counts <- win_counts[order(-N)]
  
  # Keep only relevant columns for output
  result <- win_counts[, .(Player, Tournament, Wins = N)]
  
  print(result)
}



SameSurfaceWins <- function() {
  
  # Remove team events from the dataset (assuming removeTeamEvents is defined)
  db <- removeTeamEvents(db)
  
  # Filter out matches with walkovers, abandonments, or withdrawals
  db <- db[!db$score %in% c("W/O", "ABN", "(ABN)")]
  db <- db[!str_detect(db$score, "(WEA)")]
  
  # Select winner name, tournament ID, and surface type
  wins <- db[, c('winner_name', 'tourney_id', 'surface')]
  
  # Rename columns for easier reference
  names(wins) <- c("name", "id", "surface")
  
  # Copy wins data for processing
  res <- wins
  
  # Replace any NA values with 0 (to avoid issues in counting)
  res[is.na(res)] <- 0
  
  # Count the number of wins per player on each surface
  same <- res[, .N, by = .(name, surface)]
  
  # Order the results by descending number of wins
  same <- same[order(-N)] 
  
  # Rename columns for output clarity
  names(same) <- c("Player", "Surface", "N")
  
  # Print the result showing players and their number of wins by surface
  print(same)
}


