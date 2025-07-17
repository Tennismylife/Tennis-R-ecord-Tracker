library(data.table)
library(foreach)
library(doParallel)
library(stringr)

LopsidedLoss <- function() {
  
  # Set up parallel backend, use one less core than available
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  # Copy dataset to work on and convert to data.table
  stat <- as.data.table(db)
  
  # Filter matches involving Rafael Nadal (either winner or loser)
  stat <- stat[winner_name == 'Rafael Nadal' | loser_name == 'Rafael Nadal']
  
  # Exclude matches with walkovers, defaults, abandonments, or retirements
  stat <- stat[!score %in% c("W/O", "DEF", "(ABN)") & !str_detect(score, "RET")]
  
  # Calculate points won by winner and loser based on statistics
  stat[, winner_points := w_1stWon + w_2ndWon + (l_svpt - l_1stWon - l_2ndWon)]
  stat[, loser_points := l_1stWon + l_2ndWon + (w_svpt - w_1stWon - w_2ndWon)]
  
  # Compute difference in points and percentage of points won by winner
  stat[, diffpoints := winner_points - loser_points]
  stat[, percentage := (winner_points / (winner_points + loser_points)) * 100]
  
  # Extract year from tournament ID (assumes first 4 chars are year)
  stat[, year := str_sub(tourney_id, 1, 4)]
  
  # Replace walkover score strings with a placeholder "0-0 0-0"
  stat[, score := gsub('W/O', '0-0 0-0', score)]
  
  # Use parallel foreach to parse scores and calculate total games won/lost
  results <- foreach(i = 1:nrow(stat), .combine = rbind, .packages = c("stringr")) %dopar% {
    
    # Split the score string into sets
    scores <- strsplit(stat$score[i], " ")
    
    totallost <- 0
    totalwon <- 0
    
    # For each set, extract games won and lost
    for (set in scores[[1]]) {
      games <- strsplit(set, "-")[[1]]
      
      # Remove tiebreak notation (e.g., "7(7)" -> "7")
      won <- as.numeric(sub("\\(.*", "", games[1]))
      lost <- as.numeric(sub("\\(.*", "", games[2]))
      
      # Handle NA values (if conversion fails)
      won <- ifelse(is.na(won), 0, won)
      lost <- ifelse(is.na(lost), 0, lost)
      
      totalwon <- totalwon + won
      totallost <- totallost + lost
    }
    
    # Return a data frame with tournament ID and games won/lost
    data.frame(tourney_id = stat$tourney_id[i], lostgames = totallost, wongames = totalwon)
  }
  
  # Convert results to data.table and remove duplicate tournament IDs
  results <- as.data.table(results)
  results <- results[!duplicated(tourney_id)]
  stat <- stat[!duplicated(tourney_id)]
  
  # Merge the original data with calculated games stats
  stat <- merge(stat, results, by = "tourney_id", allow.cartesian = TRUE)
  
  # Calculate difference and total number of games
  stat[, diffgames := wongames - lostgames]
  stat[, totalgames := wongames + lostgames]
  
  # Sort by descending difference in games and then by difference in points
  stat <- stat[order(-diffgames, -diffpoints)]
  
  # Select relevant columns for output
  final_stat <- stat[, .(
    tourney_name, year, round, winner_name, loser_name, score,
    wongames, lostgames, diffgames, totalgames,
    winner_points, loser_points, diffpoints
  )]
  
  # Stop parallel cluster
  stopCluster(cl)
  
  # Return final table with match stats
  return(final_stat)
}
