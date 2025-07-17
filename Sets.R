Most5SetterPlayed <- function() {
  library(data.table)
  library(stringr)
  
  db <- as.data.table(db)
  
  # Remove invalid matches
  db <- db[!score %in% c("W/O", "DEF", "(ABN)", "ABN")]
  
  # Get winner and loser data
  wins <- db[, .(name = winner_name, tourney_id, score)]
  losses <- db[, .(name = loser_name, tourney_id, score)]
  
  # Keep only matches with 5 sets (i.e., 4 hyphens in the score)
  wins <- wins[lengths(regmatches(score, gregexpr("-", score))) == 5]
  losses <- losses[lengths(regmatches(score, gregexpr("-", score))) == 5]
  
  # Combine wins and losses
  matches <- rbind(wins, losses)
  
  # Extract year from tourney_id
  matches[, year := str_sub(tourney_id, 1, 4)]
  
  # Count matches by player
  count <- matches[, .N, by = name]
  setorder(count, -N)
  
  setnames(count, c("name", "N"), c("Player", "Five_Set_Matches"))
  
  return(count)
}


MostComebackfrom2SetsDown <- function() {
  # Filter matches that lasted 5 sets (4 dashes in score string, meaning 5 sets played)
  wins <- db[str_count(score, "-") == 5]
  
  # Initialize comeback column
  wins[, comeback := "NO"]
  
  # Parse each score string to determine if winner came back from 2 sets down
  wins[, comeback := {
    # Split the score into sets
    sets <- str_split(score, " ")[[1]]
    
    # Extract numeric scores for winner and loser for each set
    # Remove tiebreak info like (7)
    parse_set <- function(s) {
      # Remove tiebreaks in parentheses
      s_clean <- str_remove(s, "\\(.*\\)")
      # Split winner-loser scores
      as.numeric(str_split(s_clean, "-")[[1]])
    }
    
    # Parse all sets
    scores_list <- lapply(sets, parse_set)
    
    # Winner scores and loser scores in each set
    winner_scores <- sapply(scores_list, `[[`, 1)
    loser_scores  <- sapply(scores_list, `[[`, 2)
    
    # Check if winner lost first two sets and won last three sets
    if (loser_scores[1] > winner_scores[1] && loser_scores[2] > winner_scores[2] &&
        winner_scores[3] > loser_scores[3] && winner_scores[4] > loser_scores[4] && winner_scores[5] > loser_scores[5]) {
      "Comeback"
    } else {
      "NO"
    }
  }, by = seq_len(nrow(wins))]
  
  # Filter only comeback matches
  comeback_matches <- wins[comeback == "Comeback"]
  
  # Count comebacks by winner name
  comeback_counts <- comeback_matches[, .N, by = winner_name]
  
  # Order descending by number of comebacks
  comeback_counts <- setorder(comeback_counts, -N)
  
  print(comeback_counts)
  
  return(comeback_counts)
}


library(stringr)
library(data.table)

MostLosses2SetsUp <- function() {
  
  # Filter matches with exactly 3 sets played (5 dashes)
  wins <- db[str_count(db$score, "-") == 5]
  
  # Prepare a vector to store comeback flags
  wins[, comeback := "NO"]
  
  for (i in 1:nrow(wins)) {
    sets <- strsplit(wins$score[i], " ")[[1]]
    
    # Initialize vectors to store set scores (winner and loser points)
    winner_points <- integer(length(sets))
    loser_points <- integer(length(sets))
    
    for (k in seq_along(sets)) {
      scores <- strsplit(sets[k], "-")[[1]]
      
      # Remove tiebreaks (e.g., "7(7)" -> "7")
      scores[2] <- sub("\\(.*", "", scores[2])
      
      # Convert to numeric, handle NA safely
      w_score <- as.numeric(scores[1])
      l_score <- as.numeric(scores[2])
      
      if (is.na(w_score)) w_score <- 0
      if (is.na(l_score)) l_score <- 0
      
      winner_points[k] <- w_score
      loser_points[k] <- l_score
    }
    
    # Check if player lost first 2 sets (loser_points > winner_points in first 2 sets)
    if (loser_points[1] > winner_points[1] && loser_points[2] > winner_points[2]) {
      wins$comeback[i] <- "Set2Down"
    }
  }
  
  # Filter those with comeback
  comeback_wins <- wins[comeback == "Set2Down"]
  
  # Count number of comeback wins by loser_name (who was down 2 sets but won)
  comeback_counts <- comeback_wins[, .N, by = loser_name]
  
  # Order decreasingly by count
  stat <- setorder(comeback_counts, -N)
  
  print(stat)
  
  return(stat)
}


Comebackfrom2SetsDownTowinSlam <- function() {
  
  # Get Slam finals and their winners
  finals <- db[round == "F" & tourney_level == "G", .(tourney_id, winner_name)]
  
  # Get all Grand Slam matches
  slams <- db[tourney_level == "G", .(tourney_id, round, winner_name, loser_name, score)]
  
  # Keep only matches played by Slam final winners (within the same tournament)
  matches <- slams[finals, on = .(tourney_id, winner_name)]
  
  # Keep only matches with 5 sets played (i.e., 5 hyphens in score)
  matches <- matches[str_count(score, "-") == 5]
  
  # Initialize comeback flag
  matches[, comeback := "NO"]
  
  # Loop through each match to check for 0–2 set deficit
  for (i in seq_len(nrow(matches))) {
    # Split score into individual sets
    sets <- strsplit(matches$score[i], " ")[[1]]
    
    # Extract game scores for each set
    scores <- lapply(sets, function(set) {
      parts <- strsplit(set, "-")[[1]]
      if (length(parts) < 2) return(c(0, 0))
      p1 <- as.numeric(parts[1])
      p2 <- as.numeric(sub("\\(.*", "", parts[2]))  # remove tiebreaks
      c(p1, p2)
    })
    
    # Check if player lost first two sets
    if (length(scores) >= 2 &&
        scores[[1]][2] > scores[[1]][1] &&
        scores[[2]][2] > scores[[2]][1]) {
      matches$comeback[i] <- "Comeback"
    }
  }
  
  # Filter only matches with comebacks
  comebacks <- matches[comeback == "Comeback"]
  
  # Add tournament name
  tourney_names <- unique(db[, .(tourney_id, tourney_name)])
  comebacks <- merge(comebacks, tourney_names, by = "tourney_id", all.x = TRUE)
  
  # Extract year from tournament ID
  comebacks[, year := substr(tourney_id, 1, 4)]
  
  # Add .id column using external function if it exists
  if (exists("getanID")) {
    comebacks <- getanID(comebacks, "winner_name")
  } else {
    comebacks[, .id := winner_name]
  }
  
  # Final output with selected columns
  return(comebacks[, .(tourney_name, year, round, winner_name, loser_name, score, .id)])
}