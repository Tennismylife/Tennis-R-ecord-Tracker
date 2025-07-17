source("Reader.R")

library(dplyr)
library(parallel)
library(tableHTML)

# Function to calculate the percentage of first serves won per player
# Only includes players with at least `min_matches` matches played
calculate_percentage_per_player <- function(df, min_matches = 100) {
  # Get a unique list of all players from both winners and losers columns
  player_list <- unique(c(df$winner_name, df$loser_name))
  
  # Function to calculate stats for a single player
  calculate_for_player <- function(player) {
    # Filter matches where the player was the winner
    matches_winner <- df %>% filter(winner_name == player)
    # Filter matches where the player was the loser
    matches_loser <- df %>% filter(loser_name == player)
    
    # Calculate total matches played by the player
    total_matches <- nrow(matches_winner) + nrow(matches_loser)
    
    # Skip players with fewer matches than the minimum threshold
    if (total_matches < min_matches) return(NULL)
    
    # Sum the number of first serves won as winner and loser
    total_1stWon <- sum(matches_winner$w_1stWon, na.rm = TRUE) + sum(matches_loser$l_1stWon, na.rm = TRUE)
    # Sum the number of first serves in as winner and loser
    total_1stIn <- sum(matches_winner$w_1stIn, na.rm = TRUE) + sum(matches_loser$l_1stIn, na.rm = TRUE)
    
    # Calculate overall first serve win percentage
    overall_percentage <- ifelse(total_1stIn > 0, (total_1stWon / total_1stIn) * 100, NA)
    
    # Return a data frame with player stats
    return(data.frame(player = player, total_matches = total_matches, overall_percentage = overall_percentage))
  }
  
  # Detect the number of cores available for parallel processing
  no_cores <- detectCores() - 1
  # Initialize cluster for parallel computation
  cl <- makeCluster(no_cores)
  
  # Export necessary variables and functions to the cluster workers
  clusterExport(cl, varlist = c("df", "min_matches", "calculate_for_player"), envir = environment())
  # Load dplyr package on each cluster worker
  clusterEvalQ(cl, library(dplyr))
  
  # Perform the calculation in parallel for each player
  results <- parLapply(cl, player_list, calculate_for_player)
  
  # Stop the cluster after computation is done
  stopCluster(cl)
  
  # Combine results into a single data frame and arrange by descending percentage
  results_df <- bind_rows(results) %>% arrange(desc(overall_percentage))
  return(results_df)
}

# Run the function on the dataset returned by ParallelReaderATP()
result <- calculate_percentage_per_player(ParallelReaderATP())

# Print the results
print(result)

# Save the results as an HTML table
write_tableHTML(tableHTML(result), file = "Test.html")
