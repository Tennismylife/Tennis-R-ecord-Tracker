#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
RoundOnSurface <- function(surface = "Hard") {
  
  # Assuming 'db' is a global or defined variable
  db <<- removeTeamEvents(db)
  
  entries <- EntriesSurface(surface)
  round_counts <- CountSurfaceRound(surface, 'W')
  
  percentage <- merge(entries, round_counts, by = "name", all = TRUE)
  
  percentage[is.na(percentage)] <- 0
  
  names(percentage)[2] <- 'entries'
  names(percentage)[3] <- 'rounds'
  
  # Convert to data.table for easy manipulation
  percentage <- data.table::as.data.table(percentage)
  
  # Calculate percentage and format as string with 2 decimals
  percentage[, percentage := ifelse(entries > 0, (rounds / entries) * 100, 0)]
  percentage[, percentage := sprintf("%.2f%%", percentage)]
  
  # Order descending by percentage (numeric part)
  percentage[, perc_num := as.numeric(sub("%", "", percentage))]
  data.table::setorder(percentage, -perc_num)
  percentage[, perc_num := NULL]
  
  # Select relevant columns
  result <- percentage[, .(name, rounds, entries, percentage)]
  
  return(result)
}



RoundOnCategory <- function(category = "G", turn = "F") {
  
  # Assuming 'db' is a global variable or environment
  db <<- removeTeamEvents(db)
  
  entries <- EntriesCategory(category)
  round_counts <- CountCategoryRound(category, turn)
  
  percentage <- merge(entries, round_counts, by = "name", all = TRUE)
  
  # Replace NAs with 0
  percentage[is.na(percentage)] <- 0
  
  # Rename columns for clarity
  names(percentage)[2] <- "entries"
  names(percentage)[3] <- "rounds"
  
  # Convert to data.table
  percentage <- data.table::as.data.table(percentage)
  
  # Calculate percentage safely (avoid division by zero)
  percentage[, percentage := ifelse(entries > 0, (rounds / entries) * 100, 0)]
  
  # Format percentage to 2 decimals with % sign
  percentage[, percentage := sprintf("%.2f%%", percentage)]
  
  # Order by numeric value of percentage descending
  percentage[, perc_num := as.numeric(sub("%", "", percentage))]
  data.table::setorder(percentage, -perc_num)
  percentage[, perc_num := NULL]
  
  # Select final columns
  res <- percentage[, .(name, rounds, entries, percentage)]
  
  print(res)  # If you want to keep the print for debugging
  
  return(res)
}



RoundOnOverall <- function(round_type = "W") {
  
  db <<- removeTeamEvents(db)
  
  entries <- EntriesOverall()
  round_counts <- CountOverallRound(round_type)
  
  percentage <- merge(entries, round_counts, by = "name", all = TRUE)
  
  # Replace NA with 0
  percentage[is.na(percentage)] <- 0
  
  names(percentage)[2] <- "entries"
  names(percentage)[3] <- "rounds"
  
  percentage <- data.table::as.data.table(percentage)
  
  # Calculate percentage safely
  percentage[, percentage := ifelse(entries > 0, (rounds / entries) * 100, 0)]
  
  # Format percentage with 2 decimals and %
  percentage[, percentage := sprintf("%.2f%%", percentage)]
  
  # Order by numeric value of percentage descending
  percentage[, perc_num := as.numeric(sub("%", "", percentage))]
  data.table::setorder(percentage, -perc_num)
  percentage[, perc_num := NULL]
  
  # Select relevant columns
  res <- percentage[, .(name, rounds, entries, percentage)]
  
  print(res)
  
  return(res)
}
