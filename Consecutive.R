source("Reader.R")
source("Remover.R")
library(data.table)


## Select all matches of one given player (by name)
PlayerMatches <- function(name, db) {
  db[winner_name==name | loser_name==name, ]
}

##' @title Find all subsequences longer than "cutoff" in a sequence
##' @param name Player name
##' @param dt data.table with matches of all players
##' @param cutoff Minimal length of the streak
##' @param win logical. If TRUE, look for streaks of wins, if FALSE
##'     for losses
##' @param breaks logical. If TRUE includes to returned matches the
##'     last before the streak and the first after
##' @return A list containing all streaks as data.table
##' @author dommy
LongestSequences <- function(name, dt, cutoff=10, win=TRUE, breaks=TRUE, verbose=FALSE) {
  ## subset the matches of that player
  matches <- PlayerMatches(name, dt)
  
  ## if win is TRUE, we encode the wins as TRUE
  if (win) {
    vec <- matches$winner_name == name
  } else {
    vec <- matches$loser_name == name
  }
  
  ## check if there is at least a TRUE in the sequence
  if (!(TRUE %in% vec)) 
    return(NA)
  
  ## use rle() to find all subsequences within vec
  tmp <- rle(vec)
  
  ## need to subset "tmp" in order to match the target value
  ## (default: TRUE, but can be anything)
  inds_of_streak <- which(tmp$values==TRUE & tmp$lengths > cutoff)
  
  ## check if there is at least one streak that made the cutoff
  if (length(inds_of_streak) == 0) 
    return(NA)
  
  ## backtransform the rle encoding in order to find the beginning
  ## of each streak in the original "vec" sequence
  posinvec <- sapply(inds_of_streak, function(i) sum(tmp$lengths[seq(1,i-1)])+1 )
  streak_len <- tmp$lengths[inds_of_streak]
  
  ord <- order(streak_len, decreasing = TRUE)
  
  ## Return a list whose elements are data.tables containing all
  ## matches in the streak. If "breaks==TRUE" INCLUDE matches before
  ## and after!
  if (breaks)
    res <- lapply(seq_along(posinvec), function(x) matches[seq(posinvec[x]-1, posinvec[x] + streak_len[x]), ] )
  else
    res <- lapply(seq_along(posinvec), function(x) matches[seq(posinvec[x], posinvec[x] + streak_len[x] -1), ] )
  
  return(res[ord])
}

## Flatten a list where elements itself can be a list
flatten <- function(lst) {
  do.call(c, lst)
}


## Generic function which extracts all streaks of wins (win==TRUE) or
## losses (win==FALSE) of at least "cutoff" matches from a data.table
## (which must be subsetted beforehand, i.e. for surface or tourney!).
Streaks <- function(db, cutoff=10, win=TRUE, breaks=TRUE) {
  
  ## find all names in this sdb
  all_names <- sort(unique(c(db$winner_name, db$loser_name)))
  
  ## iterate on each name to find the single longest streak
  all_streak <- lapply(all_names, LongestSequences,
                       dt=db, cutoff=cutoff, win=win, breaks=breaks)

  
  names(all_streak) <- all_names
  unordered <- all_streak[!is.na(all_streak)]
  
  ## now we extract the useful info and order the list of matches
  ## start from the correct names repetitions
  nstreak <- sapply(unordered, length)
  ## flatten the list
  flat <- flatten(unordered)
  
  ## compute how many matches are in the streaks
  lengths <- sapply(flat, nrow, USE.NAMES = FALSE)
  
  ## account for the 2 matches before and after the streak if they
  ## are included in the list (argument "breaks" is TRUE)
  if (breaks)
    lengths <- lengths - 2
  
  ## find walkovers
  walk <- sapply(flat, function(x) length(grep("W/O", x$score, fixed = TRUE)))
  
  ## order the longest streaks
  ord <- order(lengths, decreasing = TRUE)
  info <- data.frame(name=rep(names(nstreak), times=nstreak)[ord], N=lengths[ord], WO=walk)
  rownames(info) <- NULL
  
  return(list(info=info, matches=flat[ord]))
}
