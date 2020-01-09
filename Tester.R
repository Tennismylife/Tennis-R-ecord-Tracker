library("xlsx")
library("dplyr")
library("stringr")


source("Reader.R")

if(FALSE){
##################################################### MERGE CSV ###############################################################
wins <- read.csv(file = 'C:/Users/Andrea/Documents/TennisRecordTracker/Wins Simpson.csv')
losses <-  read.csv(file = 'C:/Users/Andrea/Documents/TennisRecordTracker/Losses Simpson.csv')

tot <- merge(wins,losses)


write.csv(tot, file = "C:/Users/Andrea/Documents/TennisRecordTracker/tot.csv", na="", quote=F, row.names = FALSE)

##################################################### UNION CSV ###############################################################
# Get a List of all files in directory named with a key word, say all `.csv` files
filenames <- list.files("C:/Users/Andrea/Downloads/tennis_atp-master/tennis_atp-master", pattern="*.csv", full.names=TRUE,)

# read and row bind all data sets
data <- rbindlist(lapply(filenames,fread))

print(data)

write.csv(data, file = "C:/Users/Andrea/Documents/TennisRecordTracker/Sackmann.csv", na="", quote=F, row.names = FALSE)

##############################################################################################################################

file="C:/Users/Andrea/git/repository/ATPStats/newdb.txt"

#Read database from csv TOP
db <- ReadData(file)

#Drop World Team Cup entries
ind_Dusseldorf <- grep("^World Team Cup", db$tourney_name)
if (length(ind_Dusseldorf)>0)
  db <- db[-ind_Dusseldorf, ]

#Drop Davis Cup entries
ind_davis <- grep("^Davis", db$tourney_name)
if (length(ind_davis)>0)
  db <- db[-ind_davis, ]

dbm <- unique(db[,c("tourney_id", "surface", "tourney_name")])

dbm$tourney_name<- gsub(' WCT', '', dbm$tourney_name)


############################################### DOMMY #######################################################################à

file="C:/Users/Andrea/git/repository/ATPStats/newdb.txt"

#Read database from csv Dommy
db2 <- ReadData(file)

dbm2 <- unique(db2[,c("tourney_id", "surface", "tourney_name")])

dbm2$tourney_name<- gsub(' WCT', '', dbm2$tourney_name)

dbm2$tourney_id<- gsub('_', '-', dbm2$tourney_id)



diff <- setdiff(dbm, dbm2)

#write.xlsx(diff, file = "SurfacesDiff.xlsx", sheetName="SurfacesDiff", append=FALSE)
#write.xlsx(dbm, file = "Surfaces.xlsx", sheetName="Surfaces1", append=FALSE)
#write.xlsx(dbm2, file = "Surfaces.xlsx", sheetName="Surfaces2", append=TRUE)


#Players collection
dbmWinners <- db2[,c("winner_name", "winner_id")]

dbmLosers <- db2[,c("loser_name", "loser_id")]

names(dbmWinners)[1] <- names(dbmLosers)[1] <- "name"
names(dbmWinners)[2] <- names(dbmLosers)[2] <- "id"

dbmPlayers <- rbind(dbmWinners, dbmLosers)

N <- dbmPlayers[, .N, by = list(name, id)]

dbmPlayers <- unique(dbmPlayers)

print(N)
write.xlsx(N, file = "dbmPlayers3.xlsx", sheetName="Players", append=FALSE)

name <- 'Zhuoyang Qiu'
true_id <- 'Q949'
fake_id <- 'X014'

dbm2 <-db2[(winner_name == name & winner_id == fake_id) | (loser_name == name & loser_id == fake_id)]


db2$winner_id[db2$winner_id == fake_id & db2$winner_name == name] <- true_id
db2$loser_id[db2$loser_id == fake_id & db2$loser_name == name] <- true_id


write.csv(db2, file = "C:/Users/Andrea/git/repository/ATPStats/test.csv", na="", quote=F, row.names = FALSE)

string  <- paste0(dbm2, collapse=",")

print(typeof(string))
print(string)


write.csv(new_dataset, file = "C:/Users/Andrea/git/repository/ATPStats/dbtml4.csv", na="", quote=F, row.names = FALSE)



#################################################################################### GET IDs AND DUPLICATE #####################################################
ind_davis <- grep("^Davis", db2$tourney_name)
if (length(ind_davis)>0)
  db2 <- db2[-ind_davis, ]

###Winner
id_collection_winner <- db2 %>%
  select(winner_id, winner_name)

id_collection_winner <- with(id_collection_winner,  id_collection_winner[order(winner_name) , ])

id_collection_winner <- unique(id_collection_winner)


###loser
id_collection_loser <- db2 %>%
  select(loser_id, loser_name)

id_collection_loser <- with(id_collection_loser,  id_collection_loser[order(loser_name) , ])

id_collection_loser <- unique(id_collection_loser)

##rename

names(id_collection_winner)[1] <- names(id_collection_loser)[1] <- "id"
names(id_collection_winner)[2] <- names(id_collection_loser)[2] <- "name"

##merge
id_collection <- rbind(id_collection_winner, id_collection_loser)

id_collection <- unique(id_collection)


##Select duplicates
id_duplicate <- id_collection[duplicated(name)]

print(id_collection)
print(id_duplicate)


write.csv(id_collection, file = "C:/Users/Andrea/git/repository/ATPStats/id_collection.csv", na="", quote=F, row.names = FALSE)
write.csv(id_duplicate, file = "C:/Users/Andrea/git/repository/ATPStats/id_duplicate.csv", na="", quote=F, row.names = FALSE)


#################################################################à Chronological order ##########################################################################
file="C:/Users/Andrea/git/repository/ATPStats/newdb.txt"

print(file)
#Read database from csv Dommy
db2 <- ReadData(file)

#transform date format
db2$tourney_date <- lubridate::ymd(db2$tourney_date)

#order by date in subgroup by player
dbm<-db2[order(db2$tourney_date),]

dbm$tourney_date <- gsub('-', '', dbm$tourney_date)


dbm <- unique(dbm)

################################################### CORRELATION ##############################################################

file1 ="C:/Users/Andrea/Downloads/StatisticsLeaders(1).csv"
file2 = "C:/Users/Andrea/Downloads/StatisticsLeaders(2).csv"

db1 <- ReadData(file1)
db2 <- ReadData(file2)

dbtot <- merge(db1,db2,by=c('name','country_name'),all.x=T)

print(dbtot)

corr <- cor(dbtot$return, dbtot$breakpoints)

print(corr)

################################################################################################################################

file="C:/Users/Andrea/git/repository/ATPStats/newdb.txt"

#Read database from csv TOP

db2 <- ReadData(file)
#Drop World Team Cup entries
ind_Dusseldorf <- grep("^World Team Cup", db2$tourney_name)
if (length(ind_Dusseldorf)>0)
  db2 <- db2[-ind_Dusseldorf, ]

#Drop Davis Cup entries
ind_davis <- grep("^Davis", db2$tourney_name)
if (length(ind_davis)>0)
  db2 <- db2[-ind_davis, ]

###Winner
id_collection_winner <- db2 %>%
  select(winner_id, winner_name)

id_collection_winner <- with(id_collection_winner,  id_collection_winner[order(winner_name) , ])

id_collection_winner <- unique(id_collection_winner)


###loser
id_collection_loser <- db2 %>%
  select(loser_id, loser_name)

id_collection_loser <- with(id_collection_loser,  id_collection_loser[order(loser_name) , ])

id_collection_loser <- unique(id_collection_loser)

##rename

names(id_collection_winner)[1] <- names(id_collection_loser)[1] <- "id"
names(id_collection_winner)[2] <- names(id_collection_loser)[2] <- "name"

##merge
id_collection <- rbind(id_collection_winner, id_collection_loser)

id_collection <- unique(id_collection)


##Select duplicates
id_duplicate <- id_collection[duplicated(name)]

print(id_collection)
print(id_duplicate)


write.csv(id_collection, file = "C:/Users/Andrea/git/repository/ATPStats/id_collection.csv", na="", quote=F, row.names = FALSE)
write.csv(id_duplicate, file = "C:/Users/Andrea/git/repository/ATPStats/id_duplicate.csv", na="", quote=F, row.names = FALSE)

#########################################################################################################################################à
file="C:/Users/Andrea/git/repository/ATPStats/newdb3.csv"

#Read database from csv TOP
db2 <- ReadData(file)

###Winner
id_collection_winner <- db2 %>%
  select(winner_id, winner_name)

id_collection_winner <- with(id_collection_winner,  id_collection_winner[order(winner_name) , ])

id_collection_winner <- unique(id_collection_winner)


###loser
id_collection_loser <- db2 %>%
  select(loser_id, loser_name)

id_collection_loser <- with(id_collection_loser,  id_collection_loser[order(loser_name) , ])

id_collection_loser <- unique(id_collection_loser)

##rename

names(id_collection_winner)[1] <- names(id_collection_loser)[1] <- "id"
names(id_collection_winner)[2] <- names(id_collection_loser)[2] <- "name"

##merge
id_collection <- rbind(id_collection_winner, id_collection_loser)

id_collection <- unique(id_collection)

id_collection <- id_collection[order(name) , ]

##Select duplicates
id_duplicate <- id_collection[duplicated(name)]

print(id_collection)
print(id_duplicate)


id <- "G048"
name <- "Tom Gullikson"

record <- db2[(winner_id ==id & winner_name == name) | (loser_id ==id & loser_name == name)]

string  <- paste0(record, collapse=",")

print(string)

write.csv(id_collection, file = "C:/Users/Andrea/git/repository/ATPStats/id_collection.csv", na="", quote=F, row.names = FALSE)
write.csv(id_duplicate, file = "C:/Users/Andrea/git/repository/ATPStats/id_duplicate.csv", na="", quote=F, row.names = FALSE)

#db2$winner_id = toupper(db2$winner_id)
#db2$loser_id = toupper(db2$loser_id)
#write.csv(db2, file = "C:/Users/Andrea/git/repository/ATPStats/newdb2.csv", na="", quote=F, row.names = FALSE)

############################################################################ DUPLICATE ########################################################
file="C:/Users/Andrea/git/repository/ATPStats/newdb3.csv"

#Read database from csv TOP

db2 <- ReadData(file)

#Drop Davis Cup entries
ind_davis <- grep("^Davis", db2$tourney_name)
if (length(ind_davis)>0)
  db2 <- db2[-ind_davis, ]

dupl <- subset(db2, duplicated(subset(db2, select=c(tourney_id, loser_id))))
write.csv(dupl, file = "C:/Users/Andrea/git/repository/ATPStats/loser_dup.csv", na="", quote=F, row.names = FALSE)

########################################### LONGEST MATCHES IN 2019 ##############################################################


file="C:/Users/Andrea/git/repository/ATPStats/newdb3.csv"

#Read database from csv TOP

db2 <- ReadData(file)

#extract year from tourney_date
db2$tourney_id <- stringr::str_sub(db2$tourney_id, 0 ,4)

dbm <- db2[tourney_id =='2019']

longest <- dbm[order(as.numeric(dbm$minutes), decreasing = TRUE),]
longest <- longest[1:10,]

print(longest)

write.xlsx(longest, "LongestMatch2019.xlsx")


########################################### MOST ACES IN 2019 ##############################################################

file="C:/Users/Andrea/git/repository/ATPStats/newdb3.csv"

#Read database from csv TOP
db2 <- ReadData(file)

#extract year from tourney_date
db2$tourney_id <- stringr::str_sub(db2$tourney_id, 0 ,4)

dbm <- db2[tourney_id =='2019']

Winner_ace <- dbm[,c('winner_id', 'winner_name', 'w_ace')]
loser_ace <- dbm[,c('loser_id', 'loser_name', 'l_ace')]

names(Winner_ace)[1] <- names(loser_ace)[1] <- "id"
names(Winner_ace)[2] <- names(loser_ace)[2] <- "name"
names(Winner_ace)[3] <- names(loser_ace)[3] <- "ace"

res <- rbind(Winner_ace, loser_ace, by = c("id"), fill=TRUE)

count <- res[, sum(ace), by = id]

count <- count[order(count$V1, decreasing = TRUE),]

count <- count[1:10,]

print(count)

write.xlsx(count, "MostAces.xlsx")


########################################### MOST DFs IN 2019 ##############################################################

file="C:/Users/Andrea/git/repository/ATPStats/newdb3.csv"

#Read database from csv TOP

db2 <- ReadData(file)

#extract year from tourney_date
db2$tourney_id <- stringr::str_sub(db2$tourney_id, 0 ,4)

dbm <- db2[tourney_id =='2019']

Winner_df <- dbm[,c('winner_id', 'winner_name', 'w_df')]
loser_df <- dbm[,c('loser_id', 'loser_name', 'l_df')]

names(Winner_df)[1] <- names(loser_df)[1] <- "id"
names(Winner_df)[2] <- names(loser_df)[2] <- "name"
names(Winner_df)[3] <- names(loser_df)[3] <- "df"

res <- rbind(Winner_df, loser_df, by = c("id"), fill=TRUE)

count <- res[, sum(df), by = id]

count <- count[order(count$V1, decreasing = TRUE),]

count <- count[1:10,]

print(count)

write.xlsx(loser_ace, "MostDFs.xlsx")


########################################### MOST 5-SETTERS WON IN 2019 ##############################################################

file="C:/Users/Andrea/git/repository/ATPStats/newdb3.csv"

#Read database from csv TOP
db2 <- ReadData(file)

#extract year from tourney_date
db2$tourney_id <- stringr::str_sub(db2$tourney_id, 0 ,4)

dbm <- db2[tourney_id =='2019']

Winner_5set <- dbm[,c('winner_id', 'winner_name', 'score')]

#count playes sets
count <- str_count(Winner_5set$score, "-")

#select only 5-setters
Winner_5setwon <- Winner_5set[which(count == 5)]

#count by players
out <- Winner_5setwon[,.N, by=winner_name]

#order
out <- out[order(-N)] 

print(out)


write.xlsx(out, "Ted.xlsx")

#####################################################################################################################################################################
# Get a List of all files in directory named with a key word, say all `.csv` files
filenames <- list.files("C:/Users/Andrea/Downloads/tennis_atp-master/tennis_atp-master", pattern="*.csv", full.names=TRUE,)

# read and row bind all data sets
data <- rbindlist(lapply(filenames,fread))

print(data)

write.csv(data, file = "C:/Users/Andrea/Documents/TennisRecordTracker/Sackmann.csv", na="", quote=F, row.names = FALSE)
#####################################################################################################################################################################
}


file="C:/Users/Andrea/git/repository/ATPStats/newdb3.csv"

#Read database from csv TOP
db2 <- ReadData(file)

###Winner
id_collection_winner <- db2 %>%
  select(winner_id, winner_name)

id_collection_winner <- with(id_collection_winner,  id_collection_winner[order(winner_name) , ])

id_collection_winner <- unique(id_collection_winner)


###loser
id_collection_loser <- db2 %>%
  select(loser_id, loser_name)

id_collection_loser <- with(id_collection_loser,  id_collection_loser[order(loser_name) , ])

id_collection_loser <- unique(id_collection_loser)

##rename

names(id_collection_winner)[1] <- names(id_collection_loser)[1] <- "id"
names(id_collection_winner)[2] <- names(id_collection_loser)[2] <- "name"

##merge
id_collection <- rbind(id_collection_winner, id_collection_loser)

id_collection <- unique(id_collection)

id_collection <- id_collection[order(name) , ]

##Select duplicates
id_duplicate <- id_collection[duplicated(name)]

print(id_collection)
print(id_duplicate)


id <- "G048"
name <- "Tom Gullikson"

record <- db2[(winner_id ==id & winner_name == name) | (loser_id ==id & loser_name == name)]

string  <- paste0(record, collapse=",")

print(string)

write.csv(id_collection, file = "C:/Users/Andrea/git/repository/ATPStats/id_collection.csv", na="", quote=F, row.names = FALSE)
write.csv(id_duplicate, file = "C:/Users/Andrea/git/repository/ATPStats/id_duplicate.csv", na="", quote=F, row.names = FALSE)