#Read database from csv
db <- ParallelReader()


db <- db[grepl("2020-", db$tourney_id)]

print(db)

db1 <- unique(db[, c('tourney_id', 'winner_id')])

db2 <- unique(db[, c('tourney_id', 'loser_id')])

names(db1)[1] <- names(db2)[1] <- 'idtournament'
names(db1)[2] <- names(db2)[2] <- 'idplayer'

dbm <- rbind(db1, db2)

write.csv(dbm, file = "C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/TourAndPlayerIds.csv", na="", quote=F, row.names = FALSE)


#Read database from csv
db <- ParallelReader()
db <- db[grepl("2020-", db$tourney_id)]

print(db)

db1 <- unique(db[, c('winner_id', 'winner_name')])

db2 <- unique(db[, c('loser_id', 'loser_name')])

names(db1)[1] <- names(db2)[1] <- 'idplayer'
names(db1)[2] <- names(db2)[2] <- 'name'

dbm <- rbind(db1, db2)

dbm <- unique(dbm)

write.csv(dbm, file = "C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/PlayerIds.csv", na="", quote=F, row.names = FALSE)


#Read database from csv
db <- ParallelReader()

db <- db[grepl("2020-", db$tourney_id)]

print(db)

db$idmatch <- seq.int(nrow(db))

db1 <- unique(db[, c('idmatch','round', 'winner_id', 'winner_age', 'winner_rank', 'winner_seed', 'winner_entry', 'loser_id', 'loser_age', 'loser_rank', 'loser_seed', 'loser_entry', 'tourney_id', 'score', 'best_of')])


db2 <- unique(db[, c('idmatch', "minutes", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved", "l_bpFaced")])


colnames(db)[3] <- "idwinner"
colnames(db)[8] <- "idloser"
colnames(db)[13] <- "idtournament"

write.csv(db1, file = "C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/Matches.csv", na="", quote=F, row.names = FALSE)

colnames(db2)[2] <- "minutes"
colnames(db2)[3] <- "winnerace"
colnames(db2)[4] <- "winnerdf"
colnames(db2)[5] <- "winnersvpts"
colnames(db2)[6] <- "winner1stIn"
colnames(db2)[7] <- "winner1stWon"
colnames(db2)[8] <- "winner2ndWon"
colnames(db2)[9] <- "winnerSvGms"
colnames(db2)[10] <- "winnerBPSaved"
colnames(db2)[11] <- "winnerBPFaced"
colnames(db2)[12] <- "loserace"
colnames(db2)[13] <- "loserdf"
colnames(db2)[14] <- "losersvpts"
colnames(db2)[15] <- "loser1stIn"
colnames(db2)[16] <- "loser1stWon"
colnames(db2)[17] <- "loser2ndWon"
colnames(db2)[18] <- "loserSvGms"
colnames(db2)[19] <- "loserBPSaved"
colnames(db2)[20] <- "loserBPFaced"

write.csv(db2, file = "C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/MatchesStats.csv", na="", quote=F, row.names = FALSE)


wins <- read.csv(file = 'C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/PlayerIds.csv')
losses <-  read.csv(file = 'C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/atp_players.csv')

print(wins)


tot <- merge(wins, losses, by = "name")

notContained <- anti_join(wins, losses, by = "name")

#notContained <- notContained[,c('idplayer', 'name', 'born', 'hand', 'country')]


tot <- tot[,c('idplayer', 'name', 'born', 'hand', 'country')]

write.csv(tot, file = "C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/completePlayers.csv", na="", quote=F, row.names = FALSE)
write.csv(notContained, file = "C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/Notcontained.csv", na="", quote=F, row.names = FALSE)
