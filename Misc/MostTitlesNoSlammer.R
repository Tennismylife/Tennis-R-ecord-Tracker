source("Reader.R")
source("Remover.R")

# Function to count titles won by players who have never won a Slam final
count_titles_no_slam <- function(matches) {
  # Identify players who have won at least one Slam final
  slam_winners <- unique(matches$winner_name[matches$tourney_level == "G" & matches$round == 'F'])
  
  # Filter matches to exclude winners who have won a Slam,
  # considering only final matches (round == 'F')
  filtered_matches <- matches[!matches$winner_name %in% slam_winners & round == 'F']
  
  # Count titles won by each player (after filtering)
  title_count <- aggregate(
    filtered_matches$winner_name,
    by = list(Player = filtered_matches$winner_name),
    FUN = length
  )
  
  # Rename columns to Player and Titles
  colnames(title_count) <- c("Player", "Titles")
  
  # Sort the result in descending order by number of titles
  title_count <- title_count[order(-title_count$Titles), ]
  
  return(title_count)
}

# Read database from CSV using custom function
db <- ParallelReaderATP()

# Remove team events if any (e.g. doubles)
db <- removeTeamEvents(db)

# Calculate the titles won by players who never won a Slam
results <- count_titles_no_slam(db)

# Print the results
print(results)

# Save the results to an HTML file
write_tableHTML(tableHTML(results), file = "Test.html")
