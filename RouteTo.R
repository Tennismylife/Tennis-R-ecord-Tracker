rounds <- c("R128", "R64", "R32", "R16", "QF", "SF", "F", "W")


BreaksRouteTo <- function(use_parallel = TRUE) {
  library(stringr)
  library(foreach)
  library(doParallel)
  
  # Player to filter matches by; set to NULL to analyze all players
  player_filter <- 'Novak Djokovic'
  # The target round we want to reach (e.g. SF = semifinal)
  round_target <- 'SF'
  # Tournament category filter, e.g. 'G' for Grand Slams
  category <- 'G'
  
  # Find position of target round in the ordered vector 'rounds'
  pos <- match(round_target, rounds)
  
  # Define rounds of interest as all rounds before the target round
  rounds_of_interest <- rounds[1:(pos-1)]
  
  # The last round in the rounds of interest (e.g. 'R16')
  final_round <- tail(rounds_of_interest, 1)
  
  # If player_filter is set to 0, treat it as no filter (NULL)
  if (!is.null(player_filter) && player_filter == 0) {
    player_filter <- NULL
  }
  
  # Filter database for matches in specified category and rounds of interest
  db_filtered <- db[db$tourney_level == category & db$round %in% rounds_of_interest, ]
  
  # If player filter is specified, keep only matches involving that player
  if (!is.null(player_filter)) {
    db_filtered <- db_filtered[
      db_filtered$winner_name == player_filter | db_filtered$loser_name == player_filter, ]
  }
  
  # Aggregate rounds played in each tournament to check if tournament is complete
  rounds_by_tourney <- aggregate(round ~ tourney_id, data = db_filtered, FUN = unique)
  
  # Helper function to check if a tournament has all rounds_of_interest
  is_complete_tourney <- function(rounds) all(rounds_of_interest %in% rounds)
  
  # Select tournaments that have all rounds_of_interest completed
  complete_tourney_ids <- rounds_by_tourney$tourney_id[sapply(rounds_by_tourney$round, is_complete_tourney)]
  db_filtered <- db_filtered[db_filtered$tourney_id %in% complete_tourney_ids, ]
  
  # Extract tournament year from tourney_id (assumes year is first 4 characters)
  db_filtered$year <- as.numeric(substr(db_filtered$tourney_id, 1, 4))
  # Get unique tournaments along with their name and year
  unique_tourneys <- unique(db_filtered[, c("tourney_id", "tourney_name", "year")])
  
  # Function to process data for each tournament separately
  process_tournament <- function(i) {
    tid <- unique_tourneys$tourney_id[i]
    name <- unique_tourneys$tourney_name[i]
    year <- unique_tourneys$year[i]
    
    # Subset matches for this tournament
    tourney_data <- db_filtered[db_filtered$tourney_id == tid, ]
    # Get unique players who played in this tournament
    players <- unique(c(tourney_data$winner_name, tourney_data$loser_name))
    players <- na.omit(players)
    
    # If player filter is set, only analyze that player if present
    if (!is.null(player_filter)) {
      if (!(player_filter %in% players)) return(NULL)
      players <- player_filter
    }
    
    # For each player, get the rounds they played in this tournament
    rounds_by_player <- lapply(players, function(p) {
      unique(tourney_data$round[tourney_data$winner_name == p | tourney_data$loser_name == p])
    })
    names(rounds_by_player) <- players
    
    # Keep only players who played in all rounds_of_interest (i.e. reached at least final_round)
    valid_players <- players[sapply(rounds_by_player, function(r) all(rounds_of_interest %in% r))]
    if (length(valid_players) == 0) return(NULL)
    
    # Exclude players who lost in the final_round (i.e., did not advance past it)
    valid_players <- Filter(function(player) {
      last_match <- tourney_data[
        tourney_data$round == final_round & tourney_data$loser_name == player, ]
      return(nrow(last_match) == 0)  # Keep player only if they did NOT lose last round
    }, valid_players)
    
    if (length(valid_players) == 0) return(NULL)
    
    # Initialize vector to accumulate break points conceded for each valid player
    breaks_conceded <- setNames(numeric(length(valid_players)), valid_players)
    
    # Loop over valid players to calculate total break points conceded in this tournament
    for (player in valid_players) {
      # Get all matches involving the player
      player_rows <- tourney_data[tourney_data$winner_name == player | tourney_data$loser_name == player, ]
      for (j in 1:nrow(player_rows)) {
        row <- player_rows[j, ]
        # Determine break points faced and saved depending on match outcome
        if (row$winner_name == player) {
          faced <- as.numeric(row$w_bpFaced)
          saved <- as.numeric(row$w_bpSaved)
        } else {
          faced <- as.numeric(row$l_bpFaced)
          saved <- as.numeric(row$l_bpSaved)
        }
        # Calculate break points conceded (faced minus saved), 0 if NA
        conceded <- if (!is.na(faced) && !is.na(saved)) faced - saved else 0
        breaks_conceded[player] <- breaks_conceded[player] + conceded
      }
    }
    
    # Return a data frame with player info and breaks conceded
    data.frame(
      player = names(breaks_conceded),
      tourney_name = name,
      year = year,
      breaks_conceded = as.numeric(breaks_conceded),
      stringsAsFactors = FALSE
    )
  }
  
  # Run processing either in parallel or sequentially
  if (use_parallel) {
    num_cores <- parallel::detectCores(logical = FALSE) - 1
    cluster <- makeCluster(num_cores)
    registerDoParallel(cluster)
    
    # Parallel foreach loop over tournaments
    results_list <- foreach(i = 1:nrow(unique_tourneys), .combine = rbind) %dopar% {
      process_tournament(i)
    }
    
    stopCluster(cluster)
  } else {
    # Sequential processing
    results_list <- do.call(rbind, lapply(1:nrow(unique_tourneys), process_tournament))
  }
  
  # Sort results by breaks conceded ascending (lowest first)
  results <- results_list[order(results_list$breaks_conceded), ]
  
  # Print top 10 players with lowest breaks conceded
  top_results <- head(results[, c("player", "year", "tourney_name", "breaks_conceded")], 10)
  print(top_results)
  
  # Return full results data frame
  return(results)
}


GamesLostRouteTo <- function(player_filter = NULL, use_parallel = TRUE) {
  library(stringr)
  library(foreach)
  library(doParallel)
  
  # Player filter is set to '0' (string), which disables filtering below
  player_filter <- '0'
  # Target round (e.g. 'W' for Winner)
  round_target <- 'W'
  # Tournament category, e.g. 'G' for Grand Slams
  category <- 'G'
  
  # Find position of the target round in rounds vector
  pos <- match(round_target, rounds)
  
  # Define rounds of interest as all rounds before the target round
  rounds_of_interest <- rounds[1:(pos-1)]
  
  # Last round in the rounds of interest
  final_round <- tail(rounds_of_interest, 1)
  
  # If player_filter is '0' (character) or 0 (numeric), treat as NULL (no filter)
  if (!is.null(player_filter) && player_filter == 0) {
    player_filter <- NULL
  }
  
  # Filter database for given category and rounds of interest
  db_filtered <- db[db$tourney_level == category & db$round %in% rounds_of_interest, ]
  
  # Clean up score column: replace walkovers (W/O) with dummy scores, remove retirements (RET)
  db_filtered$score <- gsub('W/O', '0-0 0-0', db_filtered$score)
  db_filtered$score <- gsub('RET', '', db_filtered$score)
  
  # Filter to matches involving the player, if player_filter specified
  if (!is.null(player_filter)) {
    db_filtered <- db_filtered[
      db_filtered$winner_name == player_filter | db_filtered$loser_name == player_filter, ]
  }
  
  # Function to extract number of games lost by player from the match score
  extract_lost_games <- function(score, is_winner) {
    # Split score into sets by space
    sets <- unlist(strsplit(score, " "))
    games_lost <- 0
    for (set_score in sets) {
      # Remove tiebreak details (e.g., "(7)")
      set_score <- gsub("\\(.*\\)", "", set_score)
      # Skip if set_score does not contain "-"
      if (!grepl("-", set_score)) next
      # Split set score into games won and lost
      scores <- as.numeric(unlist(strsplit(set_score, "-")))
      if (length(scores) == 2 && !any(is.na(scores))) {
        # If player won match, games lost = opponent's games (second number)
        # If player lost match, games lost = player's own games (first number)
        games_lost <- games_lost + if (is_winner) scores[2] else scores[1]
      }
    }
    games_lost
  }
  
  # Aggregate rounds by tournament to check tournament completeness
  rounds_by_tourney <- aggregate(round ~ tourney_id, data = db_filtered, FUN = unique)
  # Function to check if tournament has all rounds_of_interest
  is_complete_tourney <- function(rounds) all(rounds_of_interest %in% rounds)
  # Get IDs of tournaments that are complete
  complete_tourney_ids <- rounds_by_tourney$tourney_id[sapply(rounds_by_tourney$round, is_complete_tourney)]
  db_filtered <- db_filtered[db_filtered$tourney_id %in% complete_tourney_ids, ]
  
  # Extract year from tournament ID (assumed to be first 4 characters)
  db_filtered$year <- as.numeric(substr(db_filtered$tourney_id, 1, 4))
  # Unique tournaments with their name and year
  unique_tourneys <- unique(db_filtered[, c("tourney_id", "tourney_name", "year")])
  
  # Function to process each tournament
  process_tournament <- function(i) {
    tid <- unique_tourneys$tourney_id[i]
    name <- unique_tourneys$tourney_name[i]
    year <- unique_tourneys$year[i]
    
    # Subset data for this tournament
    tourney_data <- db_filtered[db_filtered$tourney_id == tid, ]
    players <- unique(c(tourney_data$winner_name, tourney_data$loser_name))
    
    # If filtering by player, check player presence, else return NULL
    if (!is.null(player_filter)) {
      if (!(player_filter %in% players)) return(NULL)
      players <- player_filter
    }
    
    # Get rounds played by each player in tournament
    rounds_by_player <- lapply(players, function(p) {
      unique(tourney_data$round[tourney_data$winner_name == p | tourney_data$loser_name == p])
    })
    names(rounds_by_player) <- players
    
    # Keep only players who played all rounds of interest (reached at least final_round)
    valid_players <- players[sapply(rounds_by_player, function(r) all(rounds_of_interest %in% r))]
    if (length(valid_players) == 0) return(NULL)
    
    # Exclude players who lost in the final_round (did not advance further)
    valid_players <- Filter(function(player) {
      last_match <- tourney_data[
        tourney_data$round == final_round & tourney_data$loser_name == player, ]
      return(nrow(last_match) == 0)
    }, valid_players)
    
    if (length(valid_players) == 0) return(NULL)
    
    # Initialize lost games counter for valid players
    lost_games <- setNames(numeric(length(valid_players)), valid_players)
    
    # Loop through valid players and sum games lost in their matches
    for (player in valid_players) {
      player_rows <- tourney_data[tourney_data$winner_name == player | tourney_data$loser_name == player, ]
      for (j in 1:nrow(player_rows)) {
        is_winner <- player_rows$winner_name[j] == player
        score <- player_rows$score[j]
        # Only consider valid score strings that start with a digit
        if (!is.na(score) && grepl("^[0-9]", score)) {
          lost_games[player] <- lost_games[player] + extract_lost_games(score, is_winner)
        }
      }
    }
    
    # Return data frame with player, tournament info and total games lost
    data.frame(
      player = names(lost_games),
      tourney_name = name,
      year = year,
      games_lost = as.numeric(lost_games),
      stringsAsFactors = FALSE
    )
  }
  
  # Use parallel or sequential processing over tournaments
  if (use_parallel) {
    num_cores <- parallel::detectCores(logical = FALSE) - 1
    cluster <- makeCluster(num_cores)
    registerDoParallel(cluster)
    
    results_list <- foreach(i = 1:nrow(unique_tourneys), .combine = rbind, .packages = c("stringr")) %dopar% {
      process_tournament(i)
    }
    
    stopCluster(cluster)
  } else {
    results_list <- do.call(rbind, lapply(1:nrow(unique_tourneys), process_tournament))
  }
  
  # Sort results by games lost ascending
  results <- results_list[order(results_list$games_lost), ]
  
  # Show top 10 players with least games lost
  top_results <- head(results[, c("player", "year", "tourney_name", "games_lost")], 10)
  print(top_results)
  
  # Return full results data frame
  return(results)
}



MinutesSpentRouteTo <- function(use_parallel = TRUE) {
  library(stringr)
  library(foreach)
  library(doParallel)
  
  # Fixed parameters for filtering
  player_filter <- '0'  # Only consider this player (can be changed)
  round_target <- 'W'                # Target round (e.g., semifinals)
  category <- 'G'                    # Tournament category (e.g., Grand Slam)
  
  # Find the index of the target round in the rounds vector
  pos <- match(round_target, rounds)
  
  # Select all rounds before the target round (rounds_of_interest)
  rounds_of_interest <- rounds[1:(pos-1)]
  
  # Identify the last round of interest (e.g., QF if SF is target)
  final_round <- tail(rounds_of_interest, 1)
  
  # If player_filter is zero or equivalent, reset to NULL (no filter)
  if (!is.null(player_filter) && player_filter == 0) {
    player_filter <- NULL
  }
  
  # Filter database for selected tournament category and rounds
  db_filtered <- db[db$tourney_level == category & db$round %in% rounds_of_interest, ]
  
  # Clean scores — though not actually used here, kept for future safety
  db_filtered$score <- gsub('W/O', '0-0 0-0', db_filtered$score)
  db_filtered$score <- gsub('RET', '', db_filtered$score)
  
  # If player filter is set, keep only matches involving that player
  if (!is.null(player_filter)) {
    db_filtered <- db_filtered[
      db_filtered$winner_name == player_filter | db_filtered$loser_name == player_filter, ]
  }
  
  # Remove incomplete tournaments that don't have all rounds_of_interest
  rounds_by_tourney <- aggregate(round ~ tourney_id, data = db_filtered, FUN = unique)
  is_complete_tourney <- function(rounds) all(rounds_of_interest %in% rounds)
  complete_tourney_ids <- rounds_by_tourney$tourney_id[sapply(rounds_by_tourney$round, is_complete_tourney)]
  db_filtered <- db_filtered[db_filtered$tourney_id %in% complete_tourney_ids, ]
  
  # Extract tournament year from tourney_id (assumes year is first 4 chars)
  db_filtered$year <- as.numeric(substr(db_filtered$tourney_id, 1, 4))
  unique_tourneys <- unique(db_filtered[, c("tourney_id", "tourney_name", "year")])
  
  # Function to process each tournament and calculate minutes spent by player(s)
  process_tournament <- function(i) {
    tid <- unique_tourneys$tourney_id[i]
    name <- unique_tourneys$tourney_name[i]
    year <- unique_tourneys$year[i]
    tourney_data <- db_filtered[db_filtered$tourney_id == tid, ]
    players <- unique(c(tourney_data$winner_name, tourney_data$loser_name))
    
    # If player_filter is active, keep only if player participated, else skip tournament
    if (!is.null(player_filter)) {
      if (!(player_filter %in% players)) return(NULL)
      players <- player_filter
    }
    
    # Find rounds played by each player in this tournament
    rounds_by_player <- lapply(players, function(p) {
      unique(tourney_data$round[tourney_data$winner_name == p | tourney_data$loser_name == p])
    })
    names(rounds_by_player) <- players
    
    # Keep players who played all rounds of interest (meaning they advanced sufficiently)
    valid_players <- players[sapply(rounds_by_player, function(r) all(rounds_of_interest %in% r))]
    if (length(valid_players) == 0) return(NULL)
    
    # Exclude players who lost in the final round of interest (e.g., lost in quarterfinals if SF target)
    valid_players <- Filter(function(player) {
      last_match <- tourney_data[
        tourney_data$round == final_round & tourney_data$loser_name == player, ]
      return(nrow(last_match) == 0)
    }, valid_players)
    
    if (length(valid_players) == 0) return(NULL)
    
    # Initialize a named vector to accumulate minutes played
    minutes_spent <- setNames(numeric(length(valid_players)), valid_players)
    
    # Sum minutes spent in matches for each valid player
    for (player in valid_players) {
      player_rows <- tourney_data[tourney_data$winner_name == player | tourney_data$loser_name == player, ]
      player_minutes <- player_rows$minutes
      player_minutes[is.na(player_minutes)] <- 0  # Replace NA with 0 minutes
      minutes_spent[player] <- sum(player_minutes)
    }
    
    # Return data frame with player name, tournament info, and total minutes spent
    data.frame(
      player = names(minutes_spent),
      tourney_name = name,
      year = year,
      minutes = as.numeric(minutes_spent),
      stringsAsFactors = FALSE
    )
  }
  
  # Run tournament processing in parallel or sequentially
  if (use_parallel) {
    num_cores <- parallel::detectCores(logical = FALSE) - 1
    cluster <- makeCluster(num_cores)
    registerDoParallel(cluster)
    results_list <- foreach(i = 1:nrow(unique_tourneys), .combine = rbind, .packages = c("stringr")) %dopar% {
      process_tournament(i)
    }
    stopCluster(cluster)
  } else {
    results_list <- do.call(rbind, lapply(1:nrow(unique_tourneys), process_tournament))
  }
  
  # Sort by ascending minutes spent and show top 10 entries
  results <- results_list[order(results_list$minutes), ]
  top_results <- head(results[, c("player", "year", "tourney_name", "minutes")], 10)
  print(top_results)
  
  # Return full results
  return(results)
}

