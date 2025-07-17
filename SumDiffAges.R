
SumAges <- function() {
  stat <- db[
    tourney_level == 'G' & round == 'F' & winner_age > 0 & loser_age > 0
  ]
  
  stat[, age := winner_age + loser_age]
  stat <- stat[age < 40]
  
  stat <- FormatWinnerAge(stat)
  stat <- FormatLoserAge(stat)
  
  stat[, year := stringr::str_sub(tourney_id, 1, 4)]
  
  setorder(stat, age, na.last = FALSE)
  
  # Optional: convert age to character with comma as decimal separator
  stat[, age := gsub("\\.", ",", as.character(age))]
  
  stat <- stat[, .(
    tourney_name,
    year,
    round,
    winner_name,
    winner_age,
    loser_name,
    loser_age,
    score,
    age
  )]
  
  return(stat)
}

DiffAges <- function() {
  # Filter semifinal matches at Grand Slam tournaments
  stat <- db[round == "SF" & tourney_level == "G"]
  
  # Calculate absolute age difference between winner and loser
  stat[, age := abs(winner_age - loser_age)]
  
  # Keep only matches where age difference is greater than 10 years
  stat <- stat[age > 10]
  
  # Extract the year from the tournament ID (assumes first 4 characters represent year)
  stat[, year := stringr::str_sub(tourney_id, 1, 4)]
  
  # Order by descending age difference, placing NAs last
  data.table::setorder(stat, -age, na.last = TRUE)
  
  # Apply formatting functions (assumed to return modified data.table)
  stat <- FormatWinnerAge(stat)
  stat <- FormatLoserAge(stat)
  stat <- FormatAge(stat)
  
  # Select and return relevant columns
  stat[, .(tourney_name, year, round, winner_name, winner_age, loser_name, loser_age, score, age)]
}

