library(stringr)
library(tableHTML)

# load data
matches <- ParallelReaderATP()

# filter and prepare data
matches <- matches[matches$tourney_level == 'G', ]  # keep only grand slam tournaments
matches$year <- str_sub(matches$tourney_id, 1, 4)   # extract year from tourney_id

# function to find matches with a comeback and retirement in an unfinished third set
find_comebacks_with_retirement <- function(matches) {
  
  # helper function to count how many sets player 1 or 2 has won, ignoring tie-break points
  count_sets_won <- function(set_scores, player) {
    sum(sapply(set_scores, function(set) {
      set <- gsub("\\(.*\\)", "", set)  # remove tie-break scores, e.g. 7-6(3) -> 7-6
      scores <- unlist(strsplit(set, "-"))
      if (length(scores) != 2) return(0)
      p1 <- as.numeric(scores[1])
      p2 <- as.numeric(scores[2])
      if (is.na(p1) || is.na(p2)) return(0)
      if (player == 1 && p1 > p2) return(1)
      if (player == 2 && p2 > p1) return(1)
      return(0)
    }))
  }
  
  # for each match, check if it has a retirement ('ret') in the score, and matches the comeback condition:
  # - winner lost the first two sets,
  # - third set started but not finished (less than 6 games or 6-6),
  # - retirement occurred in the third set
  matches$retired_comeback <- sapply(matches$score, function(score) {
    if (is.na(score)) return(FALSE)
    if (grepl("W/O|DEF|ABN", score, ignore.case = TRUE)) return(FALSE)  # exclude walkovers, defaults, abandonments
    
    score_clean <- trimws(score)
    set_parts <- unlist(strsplit(score_clean, "\\s+"))
    
    if (!any(grepl("RET", set_parts, ignore.case = TRUE))) return(FALSE)
    
    pre_ret_sets <- set_parts[!grepl("RET", set_parts, ignore.case = TRUE)]
    
    if (length(pre_ret_sets) < 3) return(FALSE)
    
    # check if the winner lost first two sets
    first_two_sets <- pre_ret_sets[1:2]
    p1_sets_won_first_two <- count_sets_won(first_two_sets, player = 1)
    if (p1_sets_won_first_two != 0) return(FALSE)
    
    # check the third set status: started but not completed
    third_set <- pre_ret_sets[3]
    third_set <- gsub("\\(.*\\)", "", third_set)
    scores <- unlist(strsplit(third_set, "-"))
    if (length(scores) != 2) return(FALSE)
    g1 <- as.numeric(scores[1])
    g2 <- as.numeric(scores[2])
    if (is.na(g1) || is.na(g2)) return(FALSE)
    
    max_games <- max(g1, g2)
    if (max_games > 6) return(FALSE)        # set finished
    if (g1 == 6 && g2 == 6) return(TRUE)    # tiebreak started but not finished
    if (max_games < 6) return(TRUE)         # set incomplete
    
    return(FALSE)
  })
  
  # return subset with relevant columns only, for matches that meet the criteria
  result <- subset(matches, retired_comeback == TRUE,
                   select = c("tourney_name", "year", "round", "winner_name", "loser_name", "score"))
  return(result)
}

# execute the function
result <- find_comebacks_with_retirement(matches)

# print results
print(result)

# export results to html
write_tableHTML(tableHTML(result), file = "Test.html")
