LowestRankingRound <- function(stage) {
  
  # Remove team competitions (e.g., Davis Cup)
  db_filtered <- removeTeamEvents(db)
  
  # Filter by round, unless '0' (which means all rounds)
  if (stage != '0') {
    db_filtered <- db_filtered[
      if (stage == 'W') round == 'F' else round == stage
    ]
  }
  
  # Get unique winners from finals
  winners <- unique(
    db_filtered[round == 'F', .(tourney_name, tourney_id, flag = winner_ioc,
                                name = winner_name, rank = winner_rank)]
  )
  
  # If stage is not just winners, also collect unique losers
  if (stage != 'W') {
    losers <- unique(
      db_filtered[, .(tourney_name, tourney_id, flag = loser_ioc,
                      name = loser_name, rank = loser_rank)]
    )
    # Combine winners and losers
    res <- rbind(winners, losers, fill = TRUE)
  } else {
    res <- winners
  }
  
  # Extract year from tourney_id
  res[, year := substr(tourney_id, 1, 4)]
  res[, tourney_id := NULL]  # drop original ID
  
  # Order by descending rank (lowest ranked = highest number)
  res <- res[order(-rank)]
  
  # Show top 100 entries
  head(res, 100)
}
