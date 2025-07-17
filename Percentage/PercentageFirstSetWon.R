library(dplyr)
library(tableHTML)

source("Reader.R")

df <- ParallelReaderATP()

# Filter for Grand Slam tournaments only
df <- subset(df, tourney_level == 'G')

percent_wins_all_players_top <- function(min_matches = 50, top_n = 50) {
  all_players <- unique(c(df$winner_name, df$loser_name))
  
  did_win_first_set <- function(score, player_is_winner) {
    if (is.na(score) || score == "") return(NA)
    first_set <- unlist(strsplit(score, " "))[1]
    first_set <- gsub("\\(.*\\)", "", first_set)
    if (!grepl("-", first_set)) return(NA)
    games <- unlist(strsplit(first_set, "-"))
    if (length(games) != 2) return(NA)
    g1 <- as.numeric(games[1])
    g2 <- as.numeric(games[2])
    if (is.na(g1) || is.na(g2)) return(NA)
    if (player_is_winner) {
      return(g1 > g2)
    } else {
      return(g2 > g1)
    }
  }
  
  results <- data.frame(
    player = character(),
    n_matches_after_1st_set_win = integer(),
    n_wins_after_1st_set_win = integer(),
    percent_wins = numeric(),
    total_matches = integer(),
    stringsAsFactors = FALSE
  )
  
  for (player in all_players) {
    total_played <- sum(df$winner_name == player | df$loser_name == player)
    
    if (total_played >= min_matches) {
      df$player_in_match <- df$winner_name == player | df$loser_name == player
      df$player_is_winner <- df$winner_name == player
      player_matches <- subset(df, player_in_match)
      
      player_matches$won_first_set <- mapply(
        did_win_first_set,
        player_matches$score,
        player_matches$player_is_winner
      )
      
      matches_won_first_set <- subset(player_matches, won_first_set == TRUE)
      
      n_total <- nrow(matches_won_first_set)
      n_wins <- sum(matches_won_first_set$winner_name == player, na.rm = TRUE)
      
      percent <- if (n_total > 0) (n_wins / n_total) * 100 else NA
      
      results <- rbind(results, data.frame(
        player = player,
        n_matches_after_1st_set_win = n_total,
        n_wins_after_1st_set_win = n_wins,
        percent_wins = round(percent, 2),
        total_matches = total_played,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  results <- results[order(-results$percent_wins), ]
  results_top <- head(results, top_n)
  
  return(results_top)
}

# Run with parameters: minimum 50 matches played, top 50 players
result <- percent_wins_all_players_top(min_matches = 50, top_n = 50)

# Print and save to HTML
print(result)
write_tableHTML(tableHTML(result), file = "Test.html")
