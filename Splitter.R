#Read database from csv
db <- ReadData(file)

#extract year from tourney_date
db$year <- stringr::str_sub(db$tourney_id, 0 ,4)

# 2. split dataframe into multiple dataframe based on month
splitdf <- split(db, db$year)

# 3. individually write them into csv
for(i in 1:length(splitdf))
{
  filename <- paste0(i+1967, ".csv")
  write.csv(splitdf[[i]], filename)
}