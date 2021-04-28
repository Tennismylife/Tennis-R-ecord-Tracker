# Tennis-R-ecord-Tracker
A project to find all tennis records in R

To run R project in your laptop it's necessary having R-Studio available on this link https://rstudio.com/products/rstudio/download/#download

After downloading and installing, you have to create a 'New Project' and importing all files included in the main repository

This work is based on a theory explained on Tennismylife website (http://www.tennismylife.org/recordology/). All collected records are stored in the HTML files available in directory named 'Data'. To update it you can run the R script Update.R. After this you can navigate through records starting from 'Index.html'

```
>> Rscript Update.R
```

According to "Recordology" all functions are splitted in many R files to reasearch better the results and to use them with the sub-categories explained in this article, and to use them eventually as APIs. For this reason there isn't a code refactoring.

1st step is having a consistent and live updated database. This is available on our account in the repository https://github.com/Tennismylife/TML-Database

# Reader

2nd step is reading all the database rows. Thank to parallel instructions it is possible. You can read all the csvs quickly and storing them in your R workspace

# Played

After data loading you can calculate the first records. The simplest one is to collect all played matches by single player. All scripts to search this kind of record are available in Rscript Played.R

# Wins

Such like Played.R, Wins.R collects the records based on winning matches (a subcategory of played)

# Entries

Collects all scripts to count the entries by player

# Timespan

Part I: Collects all scripts to find timespan between the same round on: overall, surface, category, tournament
Part II: As Part I but focused on entries

# Counter

Collects all scripts to count the rounds (including triumph) collected by players, divided according to sub-categories listed in "Recordology"

# Age

Implements the big categories known as "oldest" and "youngest". Setting a decreasing ordering or an increascing one you can order the records by players age

# AverageAgeTour

Particular scripts to collect the average ages year-by-year in a particular round of a specific tournament (or all main draw)

# Percentage

Scripts to calculate all percentages {[wins/ (wins + losses)]} in a specific sub-category

# Same

Implements the scripts to search all records concerning the 2nd part of the Recordology table. All records in the same: surface  category, season

# Season

All scripts to search the records collected in a season: entries, played, wins, rounds

# CounterSeason

A particular script with a function to calculate the number of rounds reached by a player year-by-year

# Least

Scripts to calculate: minutes, breaks, games to reach a round, including triumph (often used in Slams)

# Nationality

Scripts to focus the records on winners and losers nationality (on winner_ioc and loser_ioc data)

# Aces

Scripts to find records about scored aces by a player or tournament or season

## MostAces

Ranking of players who have scored the most aces in their career

## MostAcesinASeasonByPlayer

Total aces scored by a player in a season

## MostAcesinASlamByAPlayer

Total aces scored by a player in a given Slam

## RatioAcesinASlamByPlayer

Ratio between aces scored on the total number of service points by player

## MostAcesBySlammer

Ranking of players who have scored the most aces in a Slam by a winner

# Bagel

'6-0' or '0-6' scored by player with all matches

## ListMatchesBagel

Matches list of a tennis player in which he scored a bagel (6-0)

## MostBagel

Ranking of those who have scored the most bagels in their career

# H2H

Calculate stats in all match-ups between 2 players

## MostRecurrentH2HinASeason

Look for the most recurring match-ups in a single season

## VirginH2H2

Find all H2Hs where a player has never won a single match

## H2HTimespan

H2Hs rankings in which more time has passed between the 1st and the last match

## BeatSamePlayer

Player who has suffered the most defeats by a single player

# Length

Scripts based on 'minutes' parameter

# Lopsided

Scripts to find the most unbalanced wins and / or losses in a tennis player's career

# Tiebreak

All scripts for the decisive game

# Ranking

Scripts based on parameters 'winner_rank' or 'loser_rank'

# Consecutive

A powerful engine to find all record with a consecutive pattern

# Least

Calculate the statistics where you spent the least to reach an achievement

## LeastMinuteToRound

Less time spent reaching a round

## LeastBreakToRound

Less games lost to serve to reach a round

## LeastGameToReachRound

Less games lost to reach a round

## LeastSetToWintour

Less sets to win an ATP Tour title

## NoDroppedSetTitle

Tournaments won where the winner has never lost a set

# Example

Collection of miscellaneous functions to find records, not categorized

## MostWinsNoSlammer

Standings of players who have won the most matches in a Slam without ever winning a title

## PercentageInSlamsVsTop10

Win rate against Top 10s in Slams

## TimeToWinSlam

Standings of players who have spent the least time on the catch a  Slam

## PercentageAsNumber1

Win rate for the current ATP No. 1

## WinsAgainstNumber1

Standings of players who have beaten the number 1 several times

## Most5SetterPlayed

Standings of players who have played the most games in the 5th set

## NoBig3inQFs

Tournaments where there aren't Federer, Nadal & Djokovic in quarterfinals

## RoundAtAge

Rounds collected in a specific age (i.e. 24.56 years old)

## PercentageEntryWinsinCategorySurfaceOverall

Ratio of participations in tournaments on the same surface (or overall)

## PercentageSameSeasonbyPlayer

Year-to-year win rate for a given player

## MostEntriesNoTitle

Most appearances without ever winning a title

## Top10ToWinMasters

Number of Top 10 batted to catch an ATP Masters 1000 tournament

## LessWinsCategorybyWinner

Ranking of who has won the fewest matches among the winners of a title

## AgeOfNTitle

Find the age at which a certain achievement has been reached

## NWinsatAge

Number of wins collected at a certain age

## QualifiersinRound

Number of qualifiers in a given round

## RetirementsInASlam

Standings of the Slams in which there have been more retirements

## AgeWhoWillMakeARound

Age of a player who will then reach a certain turn

## AllTournamentsRoll

Collect all wins-losses for a specific player in all tournaments where he joined

## ConsecutiveWon1stSetWins

Ranking of those who have won the most matches in a row by winning the 1st set

## won1stWins

Ranking of those who have won the most matches by winning the 1st set

## Beat123intheSametournament

Tournaments where a player has defeated ATP No. 1, 2 and 3

## OldestPlayerstoWin1stTitle

Ranking of the oldest players to win a tournament

## YoungestToWinAMatchinSlams

Ranking of the youngest players to win a match in a Slam

## consecutiveSetsWon

All streaks for a given player where he won a set

## SameNation4Semifinalists

Tournaments where all 4 semifinalists are from the same country

## Top10inATournament

Ranking of tournaments where there are more ATP Top 10s

## RoundinCategoryWithNoFormerChampions

Round in a subset of tournaments where there aren't former champions (in this category)

## TournamentsPlayedToReachARound

Tournaments played to reach a specific round

## Totalbreaks

Calculate the breaks on service for all matches

## WinningPercentageClayOnTotal

Win rates on a surface for those who have collected a certain number of ATP wins

## TournamentWonByABig3WillAllInDraw

Tournament won by Federer or Nadal or Djokovic where all Big 3 were in the main draw

## TournamentsPlayedToReachNARound

Tournaments played by a player to reach N rounds

## AllATPWinsBySurface

Collect all wins by 4 surfaces for all players

## Totalbreakpoints

Calculate the break points for all matches


# Here the map to find a specific record in its subest

<a href="https://ibb.co/s6ZVRd3"><img src="https://i.ibb.co/k9tSBsc/Immagine.png" alt="Immagine" border="0"></a>
