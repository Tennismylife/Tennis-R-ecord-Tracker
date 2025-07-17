library(dplyr)
library(tableHTML)

# Load custom functions from external scripts
source("Reader.R")      # Presumably contains the ParallelReaderATP() function
source("AgeFormat.R")   # Presumably contains the FormatWinnerAge() function

# Read the database and convert the year from the tournament ID
db <- ParallelReaderATP()
db <- db %>%
  mutate(year = as.numeric(substr(tourney_id, 1, 4)))  # Extract year from first 4 chars of tourney_id

# Filter and group tournaments based on specific conditions:
# - Winner is younger than 20 years old
# - Loser rank is 10 or better (<= 10)
# Then group by tournament and only keep tournaments where there are at least 3 distinct winners and losers
result <- db %>%
  filter(winner_age < 20, loser_rank <= 10) %>%
  group_by(tourney_id) %>%
  filter(n_distinct(winner_name) >= 3, n_distinct(loser_name) >= 3) %>%
  ungroup() %>%
  # Select relevant columns for output
  select(tourney_name, year, round, winner_name, winner_age, loser_name, loser_rank, score)

# Format the winner age column using a custom formatting function
result <- FormatWinnerAge(result)

# Save the resulting data frame as an HTML table
write_tableHTML(tableHTML(result), file = "Test.html")
