library(stringr)
library(dplyr)
library(tableHTML)

source("Reader.R")
# Load the match data
matches <- ParallelReaderATP()

# Filter only Grand Slam tournaments
matches <- matches[matches$tourney_level == 'G', ]

# Extract year from tournament ID (assumes first 4 chars represent the year)
matches$year <- stringr::str_sub(matches$tourney_id, 1, 4)
matches$year <- as.numeric(matches$year)

# Identify defending champions (winners of the previous year's final)
previous_champions <- matches %>%
  filter(round == "F") %>%  # Final matches only
  select(tourney_name, year, previous_champion = winner_name) %>%
  mutate(year = year + 1)  # Shift year forward to match current tournament year

# Merge defending champion info into current matches
matches_with_champion <- matches %>%
  left_join(previous_champions, by = c("tourney_name", "year"))

# Get the current year's tournament winners (final match winners)
finals <- matches_with_champion %>%
  filter(round == "F") %>%
  select(tourney_name, year, final_winner = winner_name)

# Add tournament winner info to match data
matches_with_final_winner <- matches_with_champion %>%
  left_join(finals, by = c("tourney_name", "year"))

# Filter matches where the eventual champion beat the defending champion
champ_beats_defending_champ <- matches_with_final_winner %>%
  filter(winner_name == final_winner,
         loser_name == previous_champion)

# Select relevant columns and remove duplicates, sorted by year
all_rounds <- champ_beats_defending_champ %>%
  distinct(tourney_name, year, final_winner, previous_champion) %>%
  arrange(year)

# Print the result
print(all_rounds)

# Export result to HTML
write_tableHTML(tableHTML(all_rounds), file = "Test.html")
