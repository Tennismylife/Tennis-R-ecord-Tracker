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

db <- unique(db[, c('round', 'winner_id', 'winner_age', 'winner_seed', 'winner_entry', 'loser_id', 'loser_age', 'loser_seed', 'loser_entry', 'tourney_id', 'score', 'best_of', 'w_svpt')])

db$idmatch <- seq.int(nrow(db))

colnames(db)[2] <- "idwinner"
colnames(db)[6] <- "idloser"
colnames(db)[10] <- "idtournament"
colnames(db)[13] <- "winnerpoint"

write.csv(db, file = "C:/Users/Andrea/Documents/GitHub/ATP-Tennis-Record/Java/ATPStats/Matches.csv", na="", quote=F, row.names = FALSE)