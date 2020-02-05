#Read database from csv
db <- ReadData(file)

#extract year from tourney_date
db$year <- stringr::str_sub(db$tourney_id, 0 ,4)

db$selected_ranking <- NULL


# 2. split dataframe into multiple dataframe based on year
splitdf <- split(db, db$year)

print(splitdf)

splitdf$year <- NULL

print(splitdf)

# 3. individually write them into csv
for(i in 1:length(splitdf))
{
  splitdf[[i]]$year <- NULL
  filename <- paste0(i+1967, ".csv")
  write.csv(splitdf[[i]], filename, na="", quote=F, row.names = FALSE)
}