########

install.packages('dplyr')
install.packages('lubridrate')
install.packages('ggplot2')
#install.packages()

library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
require(dplyr)

########

## Indice FFWI 13h réorganiser


T_13h <- T_complet[T_complet$HEURE == 13,]
U_13h <- U_complet[U_complet$HEURE == 13,]

FFWI_Index_13 <<- data.frame(matrix(ncol=2,nrow=10968))

colnames(FFWI_Index_13) <- c("DATE", "FFWI_INDEX")

View(FFWI_Index_13)

for(i in 1:10968){
  ligne_humidite <- U_13h[i,]
  
  #ligne_temperature <- T_13h[T_13h$AN == ligne_humidite$AN & T_13h$MOIS == ligne_humidite$MOIS & T_13h$JOUR == ligne_humidite$JOUR,]
  
  
  FFWI_Index_13[i,] <- c(as.Date(gsub(" ", "", paste(ligne_humidite$JOUR,"/",ligne_humidite$MOIS,"/",ligne_humidite$AN)),format="%d/%m/%Y", origin="1970-01-01"), FFwi_values$FFwi[i])
  
}


########


Incendie_81_2021 <- read_delim("C:/Users/rachid/Desktop/Projet_Option/liste_incendies_91_2021.csv")



seuil_incendie <- 10000 #m2 seuil pour les incendies

date_debut <- as.Date("01/06/2000","%d/%m/%Y")
date_fin <- as.Date("22/12/2020","%d/%m/%Y")

nb_iteration = nrow(FFWI_Index_13)
Index_Summer <<- data.frame(DATE=numeric(), FFWI_Index_13=numeric(), stringsAsFactors=FALSE)

for (i in 1:nb_iteration){
  
  month <- month(as.Date(FFWI_Index_13[i,]$DATE, origin="1970-01-01"))
  if(month == 6 || month == 7 || month == 8){
    Index_Summer[nrow(Index_Summer)+1,] <- FFWI_Index_13[i,]
  }
  
}


########

Incendies_filtered <- Incendie_81_2021[Incendie_81_2021$`Surface parcourue (m2)` >= seuil_incendie,]


graph_ffwi <- FFWI_Index_13 %>% filter(between(as.Date(DATE,origin="1970-01-01"), date_debut,date_fin))

graph_summer_FFwi_index <- Index_Summer %>% filter(between(as.Date(DATE,origin="1970-01-01"), date_debut,date_fin))

length(graph_ffwi$FFWI_INDEX)
length(FFWI_Index_13$FFWI_INDEX)

### Plotting all dates within chosen range ###

ggplot(data = graph_ffwi,
       mapping = aes(x = as.Date(DATE, origin="1970-01-01"), y =FFWI_INDEX, color = FFWI_INDEX, xmin = date_debut,xmax=date_fin)) +
  geom_point() +
  scale_color_gradient(guide="none", low="red", high="green") +
  labs(x = "Date", y = "FFWI Index") +
  geom_point(data = graph_ffwi %>% filter(as.Date(DATE, origin="1970-01-01") %in% as.Date.character(Incendies_filtered$Alerte,format="%d/%m/%Y %H:%M", origin="1970-01-01")),
             pch=16, size=2, colour="black")

### Plotting all summer dates within chosen range ###

ggplot(data = graph_summer_FFwi_index,
       mapping = aes(x = as.Date(DATE, origin="1970-01-01"), y = FFWI_Index_13, color = FFWI_Index_13, xmin = date_debut,xmax=date_fin)) +
  geom_point() +
  scale_color_gradient(guide="none", low="red", high="green") +
  labs(x = "Date", y = "FFWI Index") +
  geom_point(data = graph_summer_FFwi_index %>% filter(as.Date(DATE, origin="1970-01-01") %in% as.Date.character(Incendies_filtered$Alerte,format="%d/%m/%Y %H:%M", origin="1970-01-01")),
             pch=16, size=2, colour="black")





