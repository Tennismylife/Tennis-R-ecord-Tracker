# --- Dataset d'esempio ---
matches_df <- data.frame(
  tourney_id = c("2020-AO", "2020-AO", "2020-AO", "2020-AO", "2020-AO", "2020-AO"),
  year = c(2020, 2020, 2020, 2020, 2020, 2020),
  winner_name = c("Roger Federer", "Rafael Nadal", "Novak Djokovic", "Andy Murray", "Roger Federer", "Andy Murray"),
  loser_name = c("Player A", "Player B", "Player C", "Player D", "Andy Murray", "Player E"),
  round = c("R32", "R32", "R32", "R32", "Quarterfinals", "Quarterfinals"),
  stringsAsFactors = FALSE
)
matches_df <- ParallelReaderATP()

matches_df$year <- stringr::str_sub(matches_df$tourney_id, 0 ,4)
matches_df <- matches_df[tourney_level == 'G']

count_round_and_tourneys_when_big3_present <- function(data, player_name, round_name) {
  big3 <- c("Roger Federer", "Rafael Nadal", "Novak Djokovic")
  
  player_list <- rbind(
    data.frame(tourney_id = data$tourney_id, year = data$year, player_name = data$winner_name, stringsAsFactors = FALSE),
    data.frame(tourney_id = data$tourney_id, year = data$year, player_name = data$loser_name, stringsAsFactors = FALSE)
  )
  
  player_list <- unique(player_list)
  unique_tourneys <- unique(player_list[, c("tourney_id", "year")])
  
  big3_tourneys <- data.frame(tourney_id = character(), year = character(), stringsAsFactors = FALSE)
  
  for (i in seq_len(nrow(unique_tourneys))) {
    tid <- unique_tourneys$tourney_id[i]
    y <- unique_tourneys$year[i]
    players <- player_list$player_name[player_list$tourney_id == tid & player_list$year == y]
    if (all(big3 %in% players)) {
      big3_tourneys <- rbind(big3_tourneys, data.frame(tourney_id = tid, year = y, stringsAsFactors = FALSE))
    }
  }
  
  if (nrow(big3_tourneys) == 0) return(list(count = 0, tourneys = data.frame()))
  
  count <- 0
  tourneys_played <- data.frame(tourney_name = character(), year = character(), stringsAsFactors = FALSE)
  
  for (i in seq_len(nrow(data))) {
    match <- data[i, ]
    if (match$round == round_name &&
        (match$winner_name == player_name || match$loser_name == player_name)) {
      if (any(big3_tourneys$tourney_id == match$tourney_id & big3_tourneys$year == match$year)) {
        count <- count + 1
        tourneys_played <- rbind(tourneys_played,
                                 data.frame(tourney_name = match$tourney_name,
                                            year = match$year,
                                            stringsAsFactors = FALSE))
      }
    }
  }
  
  tourneys_played <- unique(tourneys_played)
  
  return(list(count = count, tourneys = tourneys_played))
}


# --- Esempio di utilizzo ---
result <- count_round_and_tourneys_when_big3_present(matches_df, "Roger Federer", "SF")

cat("Andy Murray ha giocato", result$count, "volte i Quarti di Finale con i Big 3 presenti nel tabellone.\n")
cat("I tornei sono:", paste(result$tourneys, collapse = ", "), "\n")

result <- as.data.frame(result)


write_tableHTML(tableHTML(result), file = paste("Test.html"))

