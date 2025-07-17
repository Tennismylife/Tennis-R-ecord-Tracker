RoundsAtAge <- function() {
  
  # Remove any team events from the dataset to focus only on individual matches
  db <- removeTeamEvents(db)
  
  # Filter the dataset to keep only Grand Slam tournaments (tourney_level 'G')
  db <- db[tourney_level == 'G']
  
  # The following filters are currently commented out but can be enabled:
  # Filter matches played on Hard surface
  # db <- db[surface == 'Hard']
  
  # Filter matches from the Australian Open tournament only
  # db <- db[tourney_name == 'Australian Open']
  
  # Filter matches at a specific round (e.g., Round of 16)
  stage <-  'SF'
  db <- db[round == stage]
  
  # Filter matches where the winner's age is below approximately 22.17 years
  db <- db[winner_age < 22.16563997]
  
  # Count the number of wins per player in the filtered dataset
  # .N gives the count of rows per group, grouped by winner_name
  res <- db[, .N, by = list(db$winner_name)]
  
  # Order the results in descending order by number of wins
  setorder(res, -N)
  
  # Rename the columns to be more descriptive:
  # 'player' for the winner's name and '#Ws' for number of wins
  names(res)[1:2] <- c("player", "#Ws")
  
  # Print the resulting summary table
  print(res)
}


AgeOfNthRounds <- function(turn = 'F', N = 5) {
  
  # Validazione degli input
  if (!turn %in% db$round) {
    stop("Il turno specificato non è presente nei dati.")
  }
  if (N <= 0 || !is.numeric(N)) {
    stop("N deve essere un numero positivo.")
  }
  
  # Rimuovi match invalidi
  db <- db[!score %in% c("W/O", "DEF") & 
             !str_detect(score, "WEA") & 
             !str_detect(score, "ABN")]
  
  # Filtra solo i match di tipo Grand Slam e del turno desiderato
  db <- db[tourney_level == 'G' & round == turn]
  
  # Estrai le statistiche del vincitore
  stat <- getanID(db, "winner_name")  # Presuppone che aggiunga .id
  
  # Estrai l’anno dal tourney_id
  stat[, year := str_sub(tourney_id, 1, 4)]
  
  # Assicurati che l’età sia numerica
  stat[, winner_age := as.numeric(winner_age)]
  
  # Ordina per età crescente (dal più giovane)
  setorder(stat, winner_age, na.last = FALSE)
  
  # Aggiungi .id se non già presente
  if (!".id" %in% names(stat)) stat[, .id := .I]
  
  # Prendi l’N-esimo più giovane
  stat_n <- stat[.id == N]
  
  # Formatta l’età
  stat_n <- FormatWinnerAge(stat_n)
  
  # Rinomina colonne per chiarezza
  stat_n[, player := winner_name]
  stat_n[, age := winner_age]
  
  # Seleziona e riordina le colonne
  return(stat_n[, .(tourney_name, year, player, age, round, .id)])
}




RoundAtAgeinSlams <- function() {

  # Remove team events (make sure you have this function defined)
  if (exists("removeTeamEvents")) {
    db <- removeTeamEvents(db)
  } else {
    warning("Function removeTeamEvents not found. Proceeding without removing team events.")
  }
  
  # Filter for Grand Slam tournaments ('G'), semifinals ('SF'), and winner age under 22.181
  db <- db[
    tourney_level == 'G' 
    & round == 'F' 
    & winner_age < 23.9
  ]
  
  # List of Grand Slam tournament names
  slams <- c("Australian Open", "Roland Garros", "Wimbledon", "US Open")
  
  # Initialize result table with unique player names from winners
  res <- data.table(Player = unique(db$winner_name))
  
  # Loop over each Slam to count number of times each player reached the filtered round
  for (slam in slams) {
    db_slam <- db[tourney_name == slam]
    slam_counts <- db_slam[, .N, by = winner_name]   # Count occurrences by player
    setnames(slam_counts, "N", slam)                  # Rename count column to Slam name
    setnames(slam_counts, "winner_name", "Player")   # Rename player column to 'Player'
    res <- merge(res, slam_counts, by = "Player", all.x = TRUE)  # Merge counts into result table
  }
  
  # Replace NA values with 0 for players who did not reach the round in a Slam
  for (col in slams) {
    res[is.na(get(col)), (col) := 0]
  }
  
  # Calculate total appearances across all Slams and order by descending total
  res[, Total := rowSums(.SD), .SDcols = slams]
  setorder(res, -Total)
  
  # Rearrange columns to Player, Total, then each Slam
  res <- res[, c("Player", "Total", slams), with = FALSE]
  
  print(res)
}


#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
AgeOfRoundWhoWillBeSlammer <- function() {
  # Remove team events from the dataset
  db <- removeTeamEvents(db)
  
  # Filter to only final rounds ('F')
  db <- db[round == 'F']
  
  # Extract winner info with an ID (assumed custom function)
  stat <- getanID(db, "winner_name")
  
  # Extract year from the 'tourney_id' (first 4 characters)
  # Note: stringr indices start at 1, not 0
  stat$year <- stringr::str_sub(stat$tourney_id, 1, 4)
  
  # Keep only rows where .id == 1 (unique winners or first occurrence)
  stat <- stat[.id == 1]
  
  # Order by winner_age descending (oldest first)
  # setorder sorts ascending by default, so use -winner_age for descending
  data.table::setorder(stat, -winner_age, na.last = TRUE)
  
  # Format the winner_age column (custom formatting function)
  stat <- FormatwinnerAge(stat)
  
  # Select relevant columns to keep
  stat <- stat[, .(tourney_name, year, winner_name, winner_age)]
  
  # Print intermediate stat table for debugging
  print(stat)
  
  # Filter original db to finals and Grand Slam tournaments only
  res <- db[round == 'F' & tourney_level == 'G']
  
  # Extract year from tourney_id similarly
  res$year <- stringr::str_sub(res$tourney_id, 1, 4)
  
  # Keep only winner_name column for matching
  res <- res[, .(winner_name)]
  
  # Print intermediate res table for debugging
  print(res)
  
  # Match stat and res data frames by common rows (assumed custom function)
  stat <- match_df(stat, res)
  
  # Return the resulting matched data frame
  return(stat)
}

YoungestNthwinInSlams <- function(N = 100) {
  library(data.table)
  library(stringr)
  
  # Filter only Grand Slam matches (tourney_level == 'G')
  stat <- db[tourney_level == 'G']
  
  # Extract year from tourney ID (first 4 characters)
  stat[, year := str_sub(tourney_id, 1, 4)]
  
  # Assign a win counter per player (e.g. 1st win, 2nd win, ..., Nth win)
  stat <- getanID(stat, "winner_name")  # Assumes this function adds .id
  
  # Keep only rows where the player is achieving their Nth win
  stat <- stat[.id == N]
  
  # Order by youngest age first
  setorder(stat, winner_age, na.last = FALSE)
  
  # Format winner_age (e.g., from decimal to human-readable format)
  stat <- FormatWinnerAge(stat)
  
  # Select and return relevant columns
  result <- stat[, .(tourney_name, year, round, winner_name, winner_age, loser_name, score)]
  
  return(result)
}


YoungestPlayerTOCollectNWinsInSlams <- function(max_wins = 300) {
  library(data.table)
  library(stringr)
  
  # Copy the Grand Slam matches and ensure no NAs (replace with 0 for age-based sorting)
  stat <- copy(db)[tourney_level == 'G' & winner_age > 0]
  stat[is.na(stat)] <- 0
  
  # Add win counter (.id) per player using external function
  stat <- getanID(stat, "winner_name")  # Assumes .id column is added (1st win, 2nd win, etc.)
  
  # Preallocate result list for performance
  result_list <- vector("list", max_wins)
  
  for (i in 1:max_wins) {
    # Filter players reaching exactly the i-th Grand Slam win
    match_i <- stat[.id == i]
    
    if (nrow(match_i) == 0) next  # Skip if no players reached that number
    
    # Take youngest player to achieve i-th win
    youngest <- match_i[order(winner_age)][1]
    
    # Extract year from tourney_id
    youngest[, year := str_sub(tourney_id, 1, 4)]
    
    # Keep relevant columns
    result_list[[i]] <- youngest[, .(tourney_name, year, round, winner_name, winner_age, loser_name, score)]
  }
  
  # Combine result
  result <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
  
  result <- FormatWinnerAge(result)
  
  return(result)
}