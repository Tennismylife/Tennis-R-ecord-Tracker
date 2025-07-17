# =========================
# find early losses of seeds 4 or lower in rounds r128 or r64, including year
# =========================

# install and load necessary packages if not already installed
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("readr")) install.packages("readr", dependencies = TRUE)

library(dplyr)
library(readr)

source("Reader.R")

# define function to find early losses of top 4 seeds in r128 or r64 rounds
find_early_losses_seed4_or_lower <- function(df) {
  
  # check that required columns exist
  required_columns <- c("year", "round", "tourney_name", "loser_name", "loser_seed", "winner_name")
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    stop(paste("missing columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # normalize 'round' to uppercase and ensure 'loser_seed' is numeric
  df <- df %>%
    mutate(
      round = toupper(round),
      loser_seed = as.numeric(loser_seed)
    )
  
  # filter for early losses of seeds 4 or lower in rounds r128 and r64
  early_losses <- df %>%
    filter(round %in% c("R128", "R64")) %>%
    filter(!is.na(loser_seed) & loser_seed <= 4) %>%
    select(tourney_name, year, round, eliminated_player = loser_name, 
           seed = loser_seed, opponent = winner_name)
  
  return(early_losses)
}

# read data using parallel reader for atp
df <- ParallelReaderATP()

# extract year from tourney_id (first 4 characters)
df$year <- stringr::str_sub(df$tourney_id, 0, 4)

# apply the function to find early losses of top seeds
results <- find_early_losses_seed4_or_lower(df)

# print the results
print(results)

# export results as html
write_tableHTML(tableHTML(results), file = paste("Test.html"))
