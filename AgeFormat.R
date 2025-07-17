# Format winner age: from decimal to "Xy Yd"
FormatWinnerAge <- function(stat) {
  stat$winner_age <- as.numeric(stat$winner_age)
  years <- floor(stat$winner_age)
  days <- as.integer((stat$winner_age - years) * 365.25)
  stat$winner_age <- paste0(years, "y ", days, "d")
  return(stat)
}

# Format loser age: from decimal to "Xy Yd"
FormatLoserAge <- function(stat) {
  stat$loser_age <- as.numeric(stat$loser_age)
  years <- floor(stat$loser_age)
  days <- as.integer((stat$loser_age - years) * 365.25)
  stat$loser_age <- paste0(years, "y ", days, "d")
  return(stat)
}

# Format general age column: from decimal to "Xy Yd"
FormatAge <- function(stat) {
  stat$age <- as.numeric(stat$age)
  years <- floor(stat$age)
  days <- as.integer((stat$age - years) * 365.25)
  stat$age <- paste0(years, "y ", days, "d")
  return(stat)
}
