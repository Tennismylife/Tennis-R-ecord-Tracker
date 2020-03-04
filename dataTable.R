library(DT)

# if (!require("DT")) install.packages('DT')
xfun::session_info('DT')

tennisTable <- ParallelReader()

tennisTable <- tennisTable[, c('tourney_date', 'tourney_name','round', 'winner_ioc', 'winner_name', 'winner_age', 'loser_name', 'score')]


## 'data.frame':    32 obs. of  3 variables:
##  $ names: chr  "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
##  $ date : Date, format: "2015-03-24" "2015-03-25" ...
##  $ time : POSIXct, format: "2015-03-23 13:23:20" "2015-03-23 14:46:40" ...
datatable(tennisTable, selection = c("winner_name", "loser_name", "score"), filter = 'bottom', options = list(pageLength = 5))
