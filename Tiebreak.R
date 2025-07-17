library(stringr)
library(dplyr)

LongestTiebreak <- function(top_n = 20) {
  extract_tiebreaks <- function(score_str) {
    tiebreaks <- str_extract_all(score_str, "\\((\\d+)\\)")[[1]]
    as.numeric(str_replace_all(tiebreaks, "[()]", ""))
  }
  
  all_tiebreaks <- do.call(rbind, lapply(1:nrow(df), function(i) {
    tbs <- extract_tiebreaks(df$score[i])
    if(length(tbs) == 0) return(NULL)
    data.frame(
      tourney_name = df$tourney_name[i],
      year = df$year[i],
      round = df$round[i],
      winner_name = df$winner_name[i],
      loser_name = df$loser_name[i],
      score = df$score[i],
      tiebreak_points = tbs
    )
  }))
  
  all_tiebreaks %>%
    arrange(desc(tiebreak_points)) %>%
    head(top_n)
}
