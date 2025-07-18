removeTeamEvents <- function(db) {
  
  ind_Dusseldorf <- grep("^World Team Cup", db$tourney_name)
  if (length(ind_Dusseldorf)>0)
    db <- db[-ind_Dusseldorf, ]
  
  ind_Dusseldorf <- grep("^World Team Championship", db$tourney_name)
  if (length(ind_Dusseldorf)>0)
    db <- db[-ind_Dusseldorf, ]
  
  ind_Dusseldorf <- grep("^WCT Aetna World Cup", db$tourney_name)
  if (length(ind_Dusseldorf)>0)
    db <- db[-ind_Dusseldorf, ]
  
  ind_davis <- grep("^Davis Cup", db$tourney_name)
  if (length(ind_davis)>0)
    db <- db[-ind_davis, ]
  
  ind_Laver <- grep("^Laver Cup", db$tourney_name)
  if (length(ind_Laver)>0)
    db <- db[-ind_Laver, ]
  
  ind_Nations_Cup <- grep("^Nations Cup", db$tourney_name)
  if (length(ind_Nations_Cup)>0)
    db <- db[-ind_Nations_Cup, ]
  
  ind_ATP_Cup <- grep("^ATP Cup", db$tourney_name)
  if (length(ind_ATP_Cup)>0)
    db <- db[-ind_ATP_Cup, ]
  
  ind_Fed_Cup <- grep("^Fed Cup", db$tourney_name)
  if (length(ind_Fed_Cup)>0)
    db <- db[-ind_Fed_Cup, ]
  
  ind_Next_Gen_Finals <- grep("^Next Gen Finals", db$tourney_name)
  if (length(ind_Next_Gen_Finals)>0)
    db <- db[-ind_Next_Gen_Finals, ] 
  
  ind_United_Cup <- grep("^United Cup", db$tourney_name)
  if (length(ind_United_Cup)>0)
    db <- db[-ind_United_Cup, ]
  
  return(db)
}
