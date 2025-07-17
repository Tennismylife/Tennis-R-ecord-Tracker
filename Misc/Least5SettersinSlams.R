source("Reader.R")  # Load custom reader function

# Read and filter the database for Grand Slam tournaments (tourney_level == 'G')
db <- ParallelReaderATP()
db <- subset(db, tourney_level == 'G')

# Extract year from tourney_id (first 4 characters)
db$year <- stringr::str_sub(db$tourney_id, 1, 4)

# Add a logical column: TRUE if the match finished in a fifth set (5 dashes in the score)
db$finished_fifth_set <- stringr::str_count(db$score, "-") == 5

# Filter for matches only in rounds R16, QF, SF, and F
db <- db[db$round %in% c("R16", "QF", "SF", "F"), ]

# Aggregate the number of finished fifth set matches per tournament
result <- aggregate(finished_fifth_set ~ tourney_id, data = db, FUN = function(x) sum(x == TRUE, na.rm = TRUE))

library(data.table)
setDT(db)
setDT(result)

# Join the aggregated counts back to the original data by tourney_id
result <- db[result, on = "tourney_id", nomatch = NA]

# Select unique tournament name, year, and count of fifth set matches
result <- unique(result[, .(tourney_name, year, i.finished_fifth_set)])

# Order by the count of fifth set matches ascending
setorder(result, i.finished_fifth_set, na.last = FALSE)

# Save as HTML
library(tableHTML)
write_tableHTML(tableHTML(result), file = "Test.html")
