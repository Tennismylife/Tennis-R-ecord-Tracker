# Source the custom function for reading data
source("Reader.R")

# Load required library for string operations
library(stringr)

# Read and preprocess data
db <- ParallelReaderATP() %>%
  filter(!score %in% c("W/O", "DEF"), 
         !str_detect(score, "WEA|ABN")) %>%
  mutate(year = str_sub(tourney_id, 1, 4))

# Filter for Grand Slam matches
slam_matches <- db %>%
  filter(tourney_level == "G")

# Identify players who won a Grand Slam final (Slammers)
slammers <- unique(slam_matches$winner_name[slam_matches$round == "F"])

# Function to get first match against a Top 10 player for each Slammer
get_first_top10_match <- function(slammer) {
  slammer_matches <- slam_matches %>%
    filter((winner_name == slammer | loser_name == slammer) & 
             !is.na(winner_rank) & !is.na(loser_rank)) %>%
    filter((winner_name == slammer & loser_rank <= 10) | 
             (loser_name == slammer & winner_rank <= 10)) %>%
    arrange(tourney_date) %>%
    slice(1)
  
  if (nrow(slammer_matches) == 0) return(NULL)
  
  with(slammer_matches, data.frame(
    tourney_date = tourney_date,
    tourney_name = tourney_name,
    round = round,
    year = year,
    winner_name = winner_name,
    loser_name = loser_name,
    slammer = ifelse(winner_name == slammer, winner_name, loser_name),
    winner_rank = winner_rank,
    loser_rank = loser_rank,
    score = score
  ))
}

# Apply function to all Slammers
results <- do.call(rbind, lapply(slammers, get_first_top10_match))

# Sort results by date
results <- results[order(results$tourney_date), ]

# Print results
print(results)

# Write to HTML file (assuming tableHTML and write_tableHTML are custom functions)
write_tableHTML(tableHTML(results), file = "Test.html")