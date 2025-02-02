# 1)Préparer les données sous formes des data frame

# la précipitation 

library(readr)

RR_2000_2021 <- read_delim("C:/Users/rachid/Desktop/Projet_Option/RR 2000-2021.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
#View(RR_2000_2021)

RR_81_99 <- read_delim("C:/Users/rachid/Desktop/Projet_Option/RR 81-99.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
#View(RR_81_99)

# Le vent

VT_81_99 <- read_delim("C:/Users/rachid/Desktop/Projet_Option/VT 81-99.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(VT_81_99)


VT_2000_2021 <- read_delim("C:/Users/rachid/Desktop/Projet_Option/VT 2000-2021.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
#View(VT_2000_2021)

# La température 

T_81_99 <- read_delim("C:/Users/rachid/Desktop/Projet_Option/T 81-99.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(T_81_99)

T_2000_2021 <- read_delim("C:/Users/rachid/Desktop/Projet_Option/T 2000-2021.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
#View(T_2000_2021)

# l'humidité 

U_81_99 <- read_delim("C:/Users/rachid/Desktop/Projet_Option/U 81-99.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
#View(U_81_99)


U_2000_2021 <- read_delim("C:/Users/rachid/Desktop/Projet_Option/U 2000-2021.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)


#2) Concaténation des données

U_complet<- rbind(U_81_99,U_2000_2021)
 

VT_complet<- rbind(VT_81_99,VT_2000_2021)

T_complet<- rbind(T_81_99,T_2000_2021)

RR_complet<- rbind(RR_81_99,RR_2000_2021)


mean(na.omit(T_complet$T))

mean(na.omit(VT_complet$VT))

mean(na.omit(U_complet$U))

mean(na.omit(RR_complet$RR))

#4) Enlever les valeurs NAN et les ramplacer par la moyen:

RR_complet[is.na(RR_complet)]<-0.06693427
T_complet[is.na(T_complet)]<-14.59049
VT_complet[is.na(VT_complet)]<-13.60234
U_complet[is.na(U_complet)]<-67.76571

#sum(is.na(U_complet))

length(RR_complet$RR)
length(T_complet$T)


# 3) Calcul Indice FFWI :

#a) La fonction pour calculer EMC


EMC <- function(H,t) {
  
  
  if (H < 10 )  {
    resu <- 0.03229 + 0.281073*H - 0.000578*H*t
    
  } 
  
  else { 
    if (H >= 50) {
    resu <- 21.0606 + 0.005565*H*H - 0.00035*H*t - 0.483199*H
    
    } 
  
   else {
    resu <- 2.22749 + 0.160107*H - 0.01478*t
  } }
  
  return(resu)
}

# b) La fonction nu :

nu <- function(H,t){
  
  resu2=1 -2*((EMC(H,t))/30) + 1.5*((EMC(H,t))/30)*((EMC(H,t))/30) + 0.5*((EMC(H,t))/30)*((EMC(H,t))/30)*((EMC(H,t))/30)
  
  return(resu2)
}


# c) La fonction FFWI : 

FFWI<- function(H,t,V){return((nu(H,t)*sqrt(1+V*V))/0.3002)}

FFWI(U_complet$U[length(U_complet)],T_complet$T[length(T_complet)],VT_complet$VT[length(VT_complet)])


# 4) Pr�partion des s�ries m�t�orologiques sous forme  data frame

 FFwi_values<-cbind(T_complet)
 names(FFwi_values)[names(FFwi_values) == "T"] <- "FFwi"
 
 FFwi_values$FFwi[1]<-0
 FFwi_values$FFwi[2]<-0

 #print(T_complet)
 
 #names(FFwi_values)[names(FFwi_values) == "T"] <- "FFwi"


  for (i in 1:length(FFwi_values$FFwi))  {
    
    FFwi_values$FFwi[i]<- FFWI(U_complet$U[i],T_complet$T[i],VT_complet$VT[i])
 
  }
 
#length(FFwi_values$FFwi)
 
#sum(is.na(FFwi_values))
  
# 5) Analyse statistique 
 
#print(FFwi_values)

summary(FFwi_values)

year2plot = 2019
plot(FFwi_values$FFwi[FFwi_values$AN == year2plot], type = "h")


hist(FFwi_values$FFwi, freq=F, col='green', breaks=35)
lines(density(FFwi_values$FFwi), col='red', lwd=3)

#6)Analyse comparative des indices 

#FFWI

#install.packages('ReIns')

library(ReIns)
require(ReIns)

par(mfrow=c(1,2))

Hill(FFwi_values$FFwi, k = TRUE, logk = FALSE, plot = TRUE, add = FALSE, 
     main = "Hill estimates of the EVI")

Moment(FFwi_values$FFwi,logk = TRUE, plot = TRUE,main = "Moment estimates of the EVI")           # The moment estimator for the EVI is introduced by Dekkers et al. (1989) and 


# Faire analayse pour les diff�rents quantiles : 95% voir aussi 98% /99%


# Angstrom index

##Temp�rature et humidit� ? 13h

T_13h <- T_complet[T_complet$HEURE == 13,]
U_13h <- U_complet[U_complet$HEURE == 13,]

Angstrom_Index <- data.frame(matrix(ncol=2,nrow=10968))
colnames(Angstrom_Index) <- c("DATE", "ANGSTROM_INDEX")

for (i in 1:10968) {
  ligne_humidite <- U_13h[i,]
  ligne_temperature <- T_13h[T_13h$AN == ligne_humidite$AN & T_13h$MOIS == ligne_humidite$MOIS & T_13h$JOUR == ligne_humidite$JOUR,]
  
  index <- (ligne_humidite$U / 20) + (27 - ligne_temperature$T)/10
  
  Angstrom_Index[i,] <- c(as.Date(gsub(" ", "", paste(ligne_humidite$JOUR,"/",ligne_humidite$MOIS,"/",ligne_humidite$AN)),format="%d/%m/%Y", origin="1970-01-01"), index)
  
}




















