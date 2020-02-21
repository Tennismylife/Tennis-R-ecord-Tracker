# Tennis-R-ecord-Tracker
A project to find all tennis records in R

This work is based on a theory explained on Tennismylife website. All collected records are stored in the HTML files available in directory named 'Data'. To update it you can run the R script Update.R. After this you can navigate through records starting from 'Index.html'

: $ Rscript Update.R

According to "Recordology" all functions are splitted in many R files to reasearch better the results and to use it with the sub-categories explained in this article, and to use it eventually as APIs. For this reason there isn't a code refactoring.

The 1st step is having a consistent and updated live database. This is available on our account in the repository https://github.com/Tennismylife/TML-Database

* Reader.R 

The 2nd step is reading all the database rows. Thank to parallel instructions it is possible. You can read all the csvs quickly and storing them in your R workspace

* Played.R

After data loading you can calculate the first records. The simplest one is to collect all played matches by single player. All scripts to search this kind of record are available in Rscript Played.R

* Wins.R

Such like Played.R, Wins.R collects the records based on winning matches (a subcategory of played)

* Entries.R

Collects all scripts to count the entries by player

* Timespan.R

Part I: Collects all scripts to find timespan between the same round on: overall, surface, category, tournament
Part II: As Part I but focused on entries

* Counter.R

Collects all scripts to count the rounds (including triumph) collected by players, divided according to sub-categories listed in "Recordology"

* Age. R

Implements the big categories known as "oldest" and "youngest". Setting a decreasing ordering or a increascing one you can order the records by players age

* AverageAgeTour.R

Particular scripts to collect the average ages year-by-year in a particular round of a specific tournament (or all main draw)

* Percentage.R

Scripts to calculate all percentages {[wins/ (wins + losses)]} in a specific sub-category

* Same.R

Implements the scripts to search all records concerning the 2nd part of the Recordology table. All records in the same: surface  category, season

* Season.R

All scripts to search the records collected in a season: entries, played, wins, rounds

* CounterSeason.R

A particular script with a function to calculate the number of rounds reached by a player year-by-year

* Example.R

Collection of miscellaneous functions to find records not categorized

* Least.R

Scripts to calculate: minutes, breaks, games to reach a round including triumph (often used in Slams)

* Nationality.R

Scripts to focus the records on winners and losers nationality (on winner_ioc and loser_ioc data)


<a href="https://ibb.co/s6ZVRd3"><img src="https://i.ibb.co/k9tSBsc/Immagine.png" alt="Immagine" border="0"></a>



