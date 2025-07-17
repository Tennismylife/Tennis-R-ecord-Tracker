library(NLP)


CountRoundSeason <- function() {
  # Remove team events
  db <- removeTeamEvents(db)
  
  # ===============================
  #          FILTER SECTION
  # ===============================
  
  # Filter by tournament level (e.g. Grand Slam)
  db <- db[tourney_level == "G"]
  
  # Filter by round (e.g. Semifinals)
  db <- db[round == "SF"]
  
  # Optional: filter by surface (e.g. Clay)
  # db <- db[surface == "Clay"]
  
  # Optional: filter by winner nationality (e.g. Italy)
  # db <- db[winner_ioc == "ITA"]
  
  # Optional: filter by winner age (e.g. under 21.547)
  # db <- db[winner_age < 21.547]
  
  # ===============================
  #         PROCESSING
  # ===============================
  
  # Extract year from tourney_id
  db[, year := substr(tourney_id, 1, 4)]
  
  # Select relevant columns
  wins <- db[, .(player = winner_name, year)]
  
  # Count number of semifinal wins per player per year
  season_counts <- wins[, .N, by = .(player, year)]
  
  # Group years into a list per player
  result <- season_counts[, .(seasons = list(year)), by = player]
  
  # Count number of distinct seasons
  result[, n := lengths(seasons)]
  
  # Format seasons as comma-separated string without quotes
  result[, seasons := sapply(seasons, function(x) paste(x, collapse = ", "))]
  
  # Order by number of seasons (descending)
  setorder(result, -n)
  
  # Print the result
  print(result)
}





CountEntrySeason <- function() {
  # Remove team events
  db <- removeTeamEvents(db)
  
  # Filter out walkovers and abandoned matches
  db <- db[!score %in% c("W/O", "DEF", "(ABN)")]
  
  # Combine winner and loser data with tourney_id
  entries <- rbind(
    db[, .(player = winner_name, tourney_id)],
    db[, .(player = loser_name, tourney_id)]
  )
  
  # Extract year (first 4 characters of tourney_id)
  entries[, year := substr(tourney_id, 1, 4)]
  
  # Count player appearances per year
  season_counts <- entries[, .N, by = .(player, year)]
  
  # Aggregate seasons per player
  out <- season_counts[, .(
    seasons = paste(sort(unique(as.integer(year))), collapse = ", "),
    n = uniqueN(year)
  ), by = player]
  
  # Order by number of seasons descending
  out <- out[order(-n)]
  
  return(out)
}
