# Carica il pacchetto
library(flextable)
library(data.table)
library(openxlsx)
source("Reader.R")

#Read database from csv
db <- ParallelReaderATP()
# #extract year from tourney_date
db$year <- as.integer(stringr::str_sub(db$tourney_id, 1, 4))
db <- db[tourney_level == 'M']

matches <- db

# Funzione per verificare i giocatori nei tornei con data.table
check_players_in_tournaments_dt <- function(players_to_check, matches) {
  # Estrai i tornei unici con l'anno
  tournaments <- unique(matches[, .(tourney_id, tourney_name, year)])
  
  # Formatta l'anno per rimuovere la virgola
  tournaments$year <- format(tournaments$year, big.mark = "", scientific = FALSE)
  
  # Aggiungi una colonna flag
  tournaments$flag <- "⬛"  # Default: quadrato nero (nessuno dei giocatori)
  
  # Imposta il quadrato verde se almeno uno dei giocatori è presente nel torneo
  tournaments$flag <- ifelse(
    tournaments$tourney_id %in% matches[
      winner_name %in% players_to_check | loser_name %in% players_to_check
    ]$tourney_id,  # Qui selezioniamo i tourney_id dei tornei in cui il giocatore è presente
    "green",  # Quadrato verde
    tournaments$flag
  )

  # Imposta il quadrato rosso se nessuno dei tre giocatori è presente
  tournaments$flag <- ifelse(
    !tournaments$tourney_id %in% matches[
      winner_name %in% players_to_check | loser_name %in% players_to_check
    ]$tourney_id,  # Qui selezioniamo i tourney_id dei tornei in cui nessun giocatore è presente
    "red",  # Quadrato rosso
    tournaments$flag
  )
  
  return(tournaments)
}

players_to_check <- c("Roger Federer", "Rafael Nadal", "Novak Djokovic")

# Usa il risultato che hai ottenuto con flextable
result_table_dt <- check_players_in_tournaments_dt(players_to_check, matches)

# Crea la tabella con flextable
ft <- flextable(result_table_dt)

# Personalizza la tabella per la visualizzazione
ft <- ft %>%
  bold(j = "tourney_name") %>%
  bold(j = "year") %>%
  set_caption("Verifica presenza tennisti nei tornei con anno") %>%
  compose(j = "flag", value = as_paragraph(flag)) %>%  # Corretto
  compose(j = "tourney_name", value = as_paragraph(tourney_name)) %>%
  compose(j = "year", value = as_paragraph(year)) %>%
  autofit()

# Mostra la tabella
ft

# Creare un workbook (file Excel)
wb <- createWorkbook()

# Aggiungere una nuova sheet al workbook
addWorksheet(wb, "Tennis Tournaments")

# Scrivere i dati nella sheet (incluso la colonna flag)
writeData(wb, "Tennis Tournaments", result_table_dt)

# Aggiungere colorazione alle celle nella colonna "flag" in base al valore
flag_col_index <- which(names(result_table_dt) == "flag")

# Definire lo stile per il verde e il rosso
green_style <- createStyle(fgFill = "#00FF00")  # verde
red_style <- createStyle(fgFill = "#FF0000")    # rosso

# Applicare lo stile direttamente alla colonna "flag"
for (i in 2:(nrow(result_table_dt) + 1)) {
  if (result_table_dt$flag[i - 1] == "green") {
    addStyle(wb, sheet = "Tennis Tournaments", style = green_style, rows = i, cols = flag_col_index, gridExpand = TRUE)
    print("Green")
  } else if (result_table_dt$flag[i - 1] == "red") {
    addStyle(wb, sheet = "Tennis Tournaments", style = red_style, rows = i, cols = flag_col_index, gridExpand = TRUE)
    print("Red")
  }
}

# Esportare il file Excel
output_file <- "tournaments_info_with_colored_flags.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)

# Conferma che il file è stato salvato
cat("File Excel salvato come:", output_file, "\n")
