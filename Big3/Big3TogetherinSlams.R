source("Reader.R")

#Read database from csv
db <- ParallelReaderATP()
# #extract year from tourney_date
db$year <- stringr::str_sub(db$tourney_id, 0 ,4)

db$year <- as.numeric(db$year)

tennis_data <- db

# Caricamento del dataset
# Supponiamo che il dataset si chiami `tennis_data`
# Puoi importare il tuo dataset usando una funzione come read.csv(), ad esempio:
# tennis_data <- read.csv("path/to/your/dataset.csv")

# Definizione dei Big 3
big3 <- c("Roger Federer", "Rafael Nadal", "Novak Djokovic")

# Creazione della colonna `tourney_name_year`
tennis_data$tourney_name_year <- paste(tennis_data$tourney_name, substr(tennis_data$tourney_id, 1, 4), sep = " - ")

# Filtrare solo i tornei Slam (tourney_level == "G")
slam_data <- tennis_data[tennis_data$tourney_level == "G", ]

# Inizializzare le colonne per i Big 3
for (player in big3) {
  slam_data[[player]] <- 0
}

# Aggiungere flag per la presenza dei Big 3
for (player in big3) {
  slam_data[[player]] <- ifelse(slam_data$winner_name == player | slam_data$loser_name == player, 1, 0)
}

# Creare la tabella finale per edizione del torneo senza dplyr
slam_summary <- aggregate(cbind(`Roger Federer`, `Rafael Nadal`, `Novak Djokovic`) ~ tourney_name + substr(tourney_id, 1, 4), 
                          data = slam_data, 
                          FUN = max)

# Rinominare la colonna dell'anno
colnames(slam_summary)[2] <- "year"

# Ordinare gli Slam secondo l'ordine desiderato anno per anno
slam_order <- c("Australian Open", "Roland Garros", "Wimbledon", "US Open")
slam_summary <- slam_summary[order(slam_summary$year, match(slam_summary$tourney_name, slam_order)), ]

# Mostrare il risultato finale
print(slam_summary)

# Puoi salvare il risultato in un file CSV se necessario
# write.csv(slam_summary, "slam_summary.csv", row.names = FALSE)
write_tableHTML(tableHTML(slam_summary), file = paste("Test.html"))

