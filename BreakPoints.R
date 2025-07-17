TotalBreakPoints <- function() {
  
  # Step 1: Filter matches that are best-of-5
  stat <- db[best_of == '5']
  
  # Step 2: Filter only finals
  stat <- stat[round == 'F']
  
  # Step 3: Extract year from tourney_id (first 4 characters)
  stat[, year := substr(tourney_id, 1, 4)]
  
  # Step 4: Calculate total break points faced (winner + loser)
  stat[, totalbreakpoints := as.integer(w_bpFaced) + as.integer(l_bpFaced)]
  
  # Step 5: Keep only matches with more than 40 total break points
  stat <- stat[totalbreakpoints > 40]
  
  # Step 6: Order by total break points in descending order
  stat <- stat[order(-totalbreakpoints)]
  
  # Step 7: Select and return relevant columns
  return(stat[, .(
    tourney_name,
    year,
    round,
    winner_name,
    loser_name,
    score,
    totalbreakpoints
  )])
}



TotalBreaks <- function() {
  
  # Extract year from tourney_id (first 4 characters)
  db[, year := str_sub(tourney_id, 1, 4)]
  
  # Calculate w_breaks and l_breaks
  # Coerce to integer just in case, but ideally the columns are already numeric
  db[, w_breaks := as.integer(w_bpFaced) - as.integer(w_bpSaved)]
  db[, l_breaks := as.integer(l_bpFaced) - as.integer(l_bpSaved)]
  
  # Calculate total_breaks
  db[, total_breaks := w_breaks + l_breaks]
  
  # Order by decreasing total_breaks, NA last
  setorder(db, -total_breaks, na.last = TRUE)
  
  # Select and return subset of columns
  return(db[, .(tourney_name, year, round, winner_name, loser_name, score, total_breaks)])
}