# Load data
df <- ParallelReaderATP()

# Extract year from tourney_id (first 4 characters)
df$year <- substr(df$tourney_id, 1, 4)

# Filter only Grand Slam tournaments
df <- df[df$tourney_level == 'G', ]

# Filter only final matches
finals <- df[df$round == "F", ]

# Add tournament code abbreviations
finals$tourney_code <- NA
finals$tourney_code[grepl("Australian", finals$tourney_name, ignore.case = TRUE)] <- "AO"
finals$tourney_code[grepl("Roland Garros", finals$tourney_name, ignore.case = TRUE)] <- "RG"
finals$tourney_code[grepl("Wimbledon", finals$tourney_name, ignore.case = TRUE)] <- "W"
finals$tourney_code[grepl("US Open", finals$tourney_name, ignore.case = TRUE)] <- "UO"

# Create label combining tournament code and year, e.g. "AO 2023"
finals$label <- paste(finals$tourney_code, finals$year)

# Create data frame of winners with "Win" label
winners <- data.frame(
  player_name = finals$winner_name,
  label = paste(finals$label, "Win"),
  year = finals$year,
  tourney_code = finals$tourney_code,
  stringsAsFactors = FALSE
)

# Create data frame of losers with "Loss" label
losers <- data.frame(
  player_name = finals$loser_name,
  label = paste(finals$label, "Loss"),
  year = finals$year,
  tourney_code = finals$tourney_code,
  stringsAsFactors = FALSE
)

# Combine winners and losers into one data frame
finalists <- rbind(winners, losers)

# Map tournament codes to chronological order within the year
tourney_order_map <- c("AO" = 1, "RG" = 2, "W" = 3, "UO" = 4)
finalists$tourney_order <- tourney_order_map[finalists$tourney_code]

# Convert year and tourney_order to integers for sorting
finalists$year <- as.integer(finalists$year)
finalists$tourney_order <- as.integer(finalists$tourney_order)

# Sort by player, year, and tournament order
finalists <- finalists[order(finalists$player_name, finalists$year, finalists$tourney_order), ]

# Keep only players with at least 7 finals appearances
final_counts <- table(finalists$player_name)
players_6plus <- names(final_counts[final_counts >= 7])
finalists <- finalists[finalists$player_name %in% players_6plus, ]

# Add progressive final number per player (1,2,3,...)
finalists$final_number <- ave(finalists$player_name, finalists$player_name, FUN = seq_along)

# Create column labels like Slam1, Slam2, etc.
finalists$slam_label <- paste0("Slam", finalists$final_number)

# Reshape data to wide format: one row per player, columns for each Slam final result
wide_results <- reshape(finalists[, c("player_name", "slam_label", "label")],
                        idvar = "player_name",
                        timevar = "slam_label",
                        direction = "wide")

# Rename columns to remove "label." prefix
names(wide_results) <- gsub("label\\.", "", names(wide_results))

# Print the final table
print(wide_results)

# Export the table to HTML with a caption
library(tableHTML)
write_tableHTML(
  tableHTML(wide_results, rownames = FALSE,
            caption = "Grand Slam Finals (Win/Loss) of Players with at least 7 Finals"),
  file = "Test.html"
)
