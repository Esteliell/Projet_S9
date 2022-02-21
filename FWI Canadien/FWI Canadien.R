library(readr)
library(firebehavioR)
library(cffdrs)
library(ReIns)
library(ggplot2)
setwd("D:/Documents/Marseille/Centrale Marseille/3A/Projet Incendi/Projet_S9")

RR_91_99 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_RR_81-99.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
RR_2000_2021 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_RR_2000-2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
T_91_99 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_T_81-99.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
T_2000_2021 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_T_2000-2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
U_91_99 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_U_81-99.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
U_2000_2021 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_U_2000-2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
VT_91_99 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_VT_81-99.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
VT_2000_2021 <- read_delim("data/Construction_FWI_horaire_METEO_STATION_VT_2000-2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Incendie_91_2021 <<- read_delim("data/liste_incendies_91_2021.csv", delim=";", escape_double = FALSE, trim_ws = TRUE)

# fwi <- function(input, init = data.frame(ffmc = 85, dmc = 6, dc = 15, lat = 55), batch = TRUE, out = "all", lat.adjust = TRUE, uppercase = TRUE)
# donnees necessaires : temperature (celsius, notre T), humidite relative (en % notre U, vitesse du vent (km/h, notre V/VT), précipitations (en mm, notre RR)

RR_12h <- rbind(RR_91_99[RR_91_99$HEURE == 12,],RR_2000_2021[RR_2000_2021$HEURE == 12,])
T_12h <- rbind(T_91_99[T_91_99$HEURE == 12,],T_2000_2021[T_2000_2021$HEURE == 12,])
U_12h <- rbind(U_91_99[U_91_99$HEURE == 12,],U_2000_2021[U_2000_2021$HEURE == 12,])
VT_12h <- rbind(VT_91_99[VT_91_99$HEURE == 12,],VT_2000_2021[VT_2000_2021$HEURE == 12,])
# View(fwi(T_81_99))
# help(fwi)
names(RR_12h)[6] <- "prec"
names(T_12h)[6] <- "temp"
names(U_12h)[6] <- "rh"
names(VT_12h)[6] <- "ws"
donnee_12h <- merge(merge(merge(RR_12h, T_12h), U_12h), VT_12h)
fwi_resultat = fwi(input = na.omit(donnee_12h))


# Partie copier coller

#date_debut <- as.Date("01/06/2000","%d/%m/%Y")
#date_fin <- as.Date("22/12/2020","%d/%m/%Y")

#ggplot(data = fwi_index, mapping = aes(x = as.Date(DATE, origin="1970-01-01"), y = ANGSTROM_INDEX, color = ANGSTROM_INDEX, xmin = date_debut,xmax=date_fin)) +
#  geom_point() +
#  scale_color_gradient(guide="none", low="red", high="green") +
#  labs(x = "Date", y = "Angstrom Index") +
#  geom_point(data = fwi_index %>% filter(as.Date(DATE, origin="1970-01-01") %in% as.Date.character(Incendies_filtered$Alerte,format="%d/%m/%Y %H:%M", origin="1970-01-01")),
#             pch=16, size=2, colour="black")
