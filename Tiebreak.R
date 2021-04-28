#\\\\\\\\\\\\\\\\\APPROVED\\\\\\\\\\\\\\\\\
LongestTiebreak <- function(){
  
  stat <- db <- db[(grepl("\\(13\\)", db$score) | grepl("\\(14\\)", db$score) | grepl("\\(15\\)", db$score) | grepl("\\(16\\)", db$score) | grepl("\\(17\\)", db$score) | grepl("\\(18\\)", db$score) | grepl("\\(19\\)", db$score) | grepl("\\(20\\)", db$score) | grepl("\\(21\\)", db$score) | grepl("\\(22\\)", db$score))  & tourney_level == 'G']
  
  
}