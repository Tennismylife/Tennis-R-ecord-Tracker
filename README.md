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

* Entries

Collects all scripts to count the entries by player

# Timespan

Part I: Collects all scripts to find timespan between the same round on: overall, surface, category, tournament
Part II: As Part I but focused on entries

* Counter

Collects all scripts to count the rounds (including triumph) collected by players, divided according to sub-categories listed in "Recordology"

# Age

Implements the big categories known as "oldest" and "youngest". Setting a decreasing ordering or an increascing one you can order the records by players age

* AverageAgeTour

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

# Bagel

'6-0' or '0-6' scored by player with all matches

# H2H

Calculate stats in all match-ups between 2 players

# Length

Scripts based on 'minutes' parameter

# Lopsided

Scripts to find the most unbalanced wins and / or losses in a tennis player's career

# Tiebreak

All scripts for the decisive game

# Example

Collection of miscellaneous functions to find records, not categorized

<a href="https://ibb.co/s6ZVRd3"><img src="https://i.ibb.co/k9tSBsc/Immagine.png" alt="Immagine" border="0"></a>
