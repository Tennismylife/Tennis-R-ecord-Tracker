
FormatwinnerAge <- function(stat) {
  
stat$dec <- stat$winner_age - floor(stat$winner_age)

stat$dec <- stat$dec * 365.25

stat$winner_age <- as.integer(stat$winner_age)

stat$dec <- as.integer(stat$dec)

stat$winner_age <- paste(stat$winner_age, "y", " ", stat$dec, "d", sep = "")

print(stat)

}

FormatLoserAge <- function(stat){

stat$dec <- stat$loser_age - floor(stat$loser_age)

stat$dec <- stat$dec * 365.25

stat$loser_age <- as.integer(stat$loser_age)

stat$dec <- as.integer(stat$dec)

stat$loser_age <- paste(stat$loser_age, "y", " ", stat$dec, "d", sep = "")

stat$dec <- stat$diffage - floor(stat$diffage)

stat$dec <- stat$dec * 365.25

stat$diffage <- as.integer(stat$diffage)

stat$dec <- as.integer(stat$dec)

stat$diffage <- paste(stat$diffage, "y", " ", stat$dec, "d", sep = "")

print(stat)
  
}

FormatAge <- function(stat) {
  
  stat$dec <- stat$age - floor(stat$age)
  
  stat$dec <- stat$dec * 365.25
  
  stat$age <- as.integer(stat$age)
  
  stat$dec <- as.integer(stat$dec)
  
  stat$age <- paste(stat$age, "y", " ", stat$dec, "d", sep = "")
  
  print(stat)

}