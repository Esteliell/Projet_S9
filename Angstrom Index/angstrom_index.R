initialisation <- function() {
  
  library(readr)
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(ReIns)
  library(extRemes)
  
  RR_81_99 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_RR_81-99.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
  RR_2000_2021 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_RR_2000-2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  T_81_99 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_T_81-99.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  T_2000_2021 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_T_2000-2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  U_81_99 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_U_81-99.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  U_2000_2021 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_U_2000-2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  VT_81_99 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_VT_81-99.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  VT_2000_2021 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_VT_2000-2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  Incendie_81_2021 <<- read_delim("data/liste_incendies_91_2021.csv", delim=";", escape_double = FALSE, trim_ws = TRUE)
  #Type de feu : Forêt, A partir du : 01/10/1991, jusqu'au : 10/10/2021, D?partement : BOUCHES-DU-RHONE (13), Commune / code INSEE : 13
  
  ##Température et humidité ? 13h
  T_13h <- rbind(T_81_99[T_81_99$HEURE == 13,],T_2000_2021[T_2000_2021$HEURE == 13,])
  U_13h <- rbind(U_81_99[U_81_99$HEURE == 13,],U_2000_2021[U_2000_2021$HEURE == 13,])
  
  Angstrom_Index <<- data.frame(matrix(ncol=2,nrow=10968))
  colnames(Angstrom_Index) <<- c("DATE", "ANGSTROM_INDEX")
  
  for(i in 1:10968)
  {
    ligne_humidite <- U_13h[i,]
    ligne_temperature <- T_13h[T_13h$AN == ligne_humidite$AN & T_13h$MOIS == ligne_humidite$MOIS & T_13h$JOUR == ligne_humidite$JOUR,]
    
    index <- (ligne_humidite$U / 20) + (27 - ligne_temperature$T)/10
    
    Angstrom_Index[i,] <<- c(as.Date(gsub(" ", "", paste(ligne_humidite$JOUR,"/",ligne_humidite$MOIS,"/",ligne_humidite$AN)),format="%d/%m/%Y", origin="1970-01-01"), index)
    
  }
  

  nb_iteration = nrow(Angstrom_Index)
  Index_Summer <<- data.frame(DATE=numeric(), ANGSTROM_INDEX=numeric(), stringsAsFactors=FALSE)
  
  for (i in 1:nb_iteration){
    month <- month(as.Date(Angstrom_Index[i,]$DATE, origin="1970-01-01"))
    if(month == 6 || month == 7 || month == 8){
      Index_Summer[nrow(Index_Summer)+1,] <<- Angstrom_Index[i,]
    }
    
  }
  
}


