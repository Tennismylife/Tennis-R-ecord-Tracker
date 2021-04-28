lopsidedLoss <- function() {
  
  stat <- db
  
  #extract id from tourney_id
  #stat$tourid <- sub("^[^-]*", "", stat$tourney_id)
  
  #stat <- stat[tourid == -580]
  
  stat <- stat[tourney_level == 'G' & tourney_name == 'Roland Garros']
  
  #stat <- stat[round == 'F']
  
  #stat <- stat[loser_name == 'Novak Djokovic']
  
  #stat <- stat[loser_rank == 1]
  
  stat <- stat[winner_name == 'Rafael Nadal' | loser_name == 'Rafael Nadal']
  
  #stat <- stat[(str_count(stat$score, "-") == '5' & best_of == 5)]
  
  stat <-stat[!stat$score == "W/O" & !stat$score == "DEF" & !stat$score == "(ABN)" & !str_detect(stat$score, "RET")]
  
  stat <-stat[, winner_points := w_1stWon + w_2ndWon + (l_svpt - l_1stWon - l_2ndWon)]
  
  stat <-stat[, loser_points := l_1stWon + l_2ndWon + (w_svpt - w_1stWon - w_2ndWon)]
  
  stat$diffpoints <- stat[, winner_points - loser_points]
  
  #stat <- stat[winner_points > 0]
  
  stat <-stat[, percentage := (winner_points / (winner_points + loser_points)) * 100]
  
  #stat$diffpoints[is.na(stat$diffpoints)] <- 0
  
  #extract year from tourney_date
  stat$year <- stringr::str_sub(stat$tourney_id, 0 , 4)
  
  #change walkover with 0 games
  stat$score <- gsub('W/O', '0-0 0-0', stat$score)
  
  #split to catch the sets
  stat$set <- strsplit(stat$score, " ")
  
  library(foreach)
  foreach(i = 1:length(stat$set)) %do%
    {
      print(stat$tourney_id[i])
      
      score <- strsplit(stat$set[[i]], "-")
      
      totallost <- 0
      totalwon <- 0
      
      #count lost games
      foreach(j = 1:length(score)) %do%
        {
          #sub for tiebreaks
          score[[j]][2] <-  sub("\\(.*", "", score[[j]][2])
          
          score[[j]][2][is.na(score[[j]][2])] <- 0
          
          totallost<-totallost+as.numeric(score[[j]][2])
          
          score[[j]][1] <-  sub("\\(.*", "", score[[j]][1])
          
          score[[j]][1][is.na(score[[j]][1])] <- 0
          
          totalwon<-totalwon+as.numeric(score[[j]][1])
        }
      
      #sub score with lost games
      stat$lostgames[i]<- unlist(totallost)
      stat$wongames[i]<- unlist(totalwon)
    }
  
  stat$lostgames <- as.numeric(as.character(unlist(stat$lostgames)))
  stat$wongames <- as.numeric(as.character(unlist(stat$wongames)))
  
  stat$diffgames <- stat[, wongames - lostgames]
  stat$totalgames <- stat[, wongames + lostgames]
  
  stat <- 
    stat[,c("tourney_name", 
            "year", 
            "round", 
            "winner_name", 
            "loser_name", 
            "score", 
            "wongames", 
            "lostgames",
            "diffgames",
            "totalgames",
            "winner_points",
            "loser_points",
            "diffpoints"
    )]
  
  ## order by decreasing
  stat <- setorder(stat, lostgames, na.last = FALSE)
  
  stat <- stat[with(stat, order(-diffgames, -diffpoints)), ]
  
  #stat <- stat[1:200, ]
}