library(tibble)
library(tableHTML)

source("Reader.R")
       
db <- ParallelReaderATP()

round <- c("0", "R64","R32", "R16", "QF", "SF" , "F", "W")

stat2 <- NULL 

for(i in 1:length(round)){

stat <- CountCategoryRound('G', round[i])

stat <- stat[name == 'Roger Federer' | name == 'Rafael Nadal' | name == 'Novak Djokovic']

stat <- setorder(stat, -name, na.last=FALSE)

stat <- add_column(stat, round[i], .after = "name")

stat2 <- rbind(stat2, stat, fill = TRUE)

}

print(stat2)

write_tableHTML(tableHTML(stat2), file = paste("Test.html"))

