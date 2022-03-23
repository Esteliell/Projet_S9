library(readr)
devtools::install_github("EcoFire/firebehavioR")
library(firebehavioR)
library(ggplot2)
library(lubridate)
library(extRemes)
library(evd)
force=TRUE


RR_81_99 <- read_delim("C:/Users/adrie/Downloads/Construction_FWI_horaire_METEO_STATION_RR_81-99.csv", 
                                                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
RR_2000_2021 <- read_delim("C:/Users/adrie/Downloads/Construction_FWI_horaire_METEO_STATION_RR_2000-2021.csv", 
                                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
T_81_99 <- read_delim("C:/Users/adrie/Downloads/Construction_FWI_horaire_METEO_STATION_T_81-99.csv", 
                                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
T_2000_2021 <- read_delim("C:/Users/adrie/Downloads/Construction_FWI_horaire_METEO_STATION_T_2000-2021.csv", 
                                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
U_81_99 <- read_delim("C:/Users/adrie/Downloads/Construction_FWI_horaire_METEO_STATION_U_81-99.csv", 
                                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
U_2000_2021 <- read_delim("C:/Users/adrie/Downloads/Construction_FWI_horaire_METEO_STATION_U_2000-2021.csv", 
                                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
VT_81_99 <- read_delim("C:/Users/adrie/Downloads/Construction_FWI_horaire_METEO_STATION_VT_81-99.csv", 
                                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
VT_2000_2021 <- read_delim("C:/Users/adrie/Downloads/Construction_FWI_horaire_METEO_STATION_VT_2000-2021.csv", 
                                                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

H_RR_81_99 <- RR_81_99[RR_81_99$HEURE == 15,]
H_RR_2000_2021 <- RR_2000_2021[RR_2000_2021$HEURE == 15,]
H_T_81_99 <- T_81_99[T_81_99$HEURE == 15,]
H_T_2000_2021 <- T_2000_2021[T_2000_2021$HEURE == 15,]
H_U_81_99 <- U_81_99[U_81_99$HEURE == 15,]
H_U_2000_2021 <- U_2000_2021[U_2000_2021$HEURE == 15,]
H_VT_81_99 <- VT_81_99[VT_81_99$HEURE == 15,]
H_VT_2000_2021 <- VT_2000_2021[VT_2000_2021$HEURE == 15,]

RR_15h <- rbind(RR_81_99[RR_81_99$HEURE == 15,],RR_2000_2021[RR_2000_2021$HEURE == 15,])
T_15h <- rbind(T_81_99[T_81_99$HEURE == 15,],T_2000_2021[T_2000_2021$HEURE == 15,])
U_15h <- rbind(U_81_99[U_81_99$HEURE == 15,],U_2000_2021[U_2000_2021$HEURE == 15,])
VT_15h <- rbind(VT_81_99[VT_81_99$HEURE == 15,],VT_2000_2021[VT_2000_2021$HEURE == 15,])


Diff_date <- as.numeric(difftime(as.Date("1991-10-01"), as.Date("1970-01-01"), units = "days"))

col_datee <- vector(mode = "logical", nrow(RR_15h))
for (i in 1:nrow(RR_15h)){
  col_datee[i] <- Diff_date + i - 1
}

##A <- as.Date(col_datee, origin = "1970-01-01")
A <- as.data.frame(col_datee)
A <- as.Date(A$col_datee, origin = "1970-01-01")

rr <- RR_15h[,6]
t <- T_15h[,6]
u <- U_15h[,6]
vt <- VT_15h[,6]


McArthur_Data <- cbind.data.frame(A,t,u,vt,rr)


years=as.numeric(format(as.Date(McArthur_Data$col_datee, format = "%Y-%m-%d"), "%Y"))
cumuls = aggregate(McArthur_Data$RR, by = list(annee = years), FUN = sum)
cumul <- na.omit(cumuls)
map = mean(cumul[,2])

ff = rbind(
  data.frame(ffm = ffm("simard",McArthur_Data$RR, McArthur_Data$T)$fm1hr,method="simard"),
  data.frame(ffm = ffm("wagner",McArthur_Data$RR, McArthur_Data$T)$fm1hr,method="wagner"),
  data.frame(ffm = ffm("anderson",McArthur_Data$RR, McArthur_Data$T)$fm1hr,method="anderson")
)


ggplot(ff, aes(ffm, color = method, fill = method)) + geom_density(alpha = 0.5) + 
         xlab("Fine fuel moisture (%)") + theme_classic()

ff$A = rep(McArthur_Data$A,3)

McArthur_Data_ma = subset(McArthur_Data, U <= 70 & T >= 10 & U > 42.5 - 1.25 * T & 
                     U < 94.5 - 1.35 * T)
ff.ma = data.frame(ffm = ffm("mcarthur", McArthur_Data_ma$rh, McArthur_Data_ma$temp_c)$fm1hr, method = "mcarthur", 
                   dateTime = McArthur_Data_ma$A)
ff = rbind(
  data.frame(ffm = ffm("simard",McArthur_Data$RR, McArthur_Data$T)$fm1hr,method="simard"),
  data.frame(ffm = ffm("wagner",McArthur_Data$RR, McArthur_Data$T)$fm1hr,method="wagner"),
  data.frame(ffm = ffm("anderson",McArthur_Data$RR, McArthur_Data$T)$fm1hr,method="anderson")
)
ff$A = rep(McArthur_Data$A, 3)
ff = rbind(ff, ff.ma)
ff$A <- strptime(ff$A, "%Y/%m/%d")
##ff$A <- as.POSIXct(ff$A)

ffm.plot = ggplot(ff, aes(x = A, y = ffm, color = method)) + geom_smooth(span = 0.1, 
                                                                                method = "loess", se = F) + theme_classic() + labs(x = "Time", y = "Fuel moisture (%)")
print(ffm.plot)

##ggplot(ff, aes(ffm, color = method, fill = method)) + geom_density(alpha = 0.5) + 
##  xlab("Fine fuel moisture (%)") + theme_classic()


##McArthur_Data <- data.frame(DATE = numeric(), T = numeric(), RR = numeric(), U = numeric(), VT = numeric(), stringsAsFactors =  FALSE)

##for (i in 1:nrow(RR_15h)){
  
##ref_date <- T_15h[i,]
##t <- T_15h[i,]$T
##rr <- RR_15h[i,]$RR
##u <- U_15h[i,]$U
##vt <- VT_15h[i,]$VT
  
##McArthur_Data[nrow(RR_15h)+1,] <- c(as.Date(gsub(" ", "", paste(ref_date$JOUR,"/",ref_date$MOIS,"/",ref_date$AN)), format="%Y/%m/%d", origin = "1970-01-01"),T,RR,U,VT)


##data.frame à la place de c(as.date()) : affiche une erreur 
##data.frame(gsub(" ", "", paste(ref_date$JOUR,"/",ref_date$MOIS,"/",ref_date$AN)), format="%d/%m/%Y", origin = "1970-01-01")

##McArthur_Data$A <- as.Date(McArthur_Data$A, format = '%Y-%m-%d')

McArthur_Data.daily =   McArthur_Data[format(strptime(McArthur_Data$A, "%Y/%m/%d"),)]
indices = data.frame(c(1:10968))
indices = fireIndex(McArthur_Data$T, McArthur_Data$VT, McArthur_Data$U)
##normalize=function(x,low=0,high=1){low+(x-min(x,na.rm=T))*(high-low)/(max(x,na.rm=T)-min(x,na.rm=T))}
#indices = data.frame(sapply(indices,normalize),  Date = strptime(McArthur_Data$A, "%Y/%m/%d"))
indices$Date <- NULL
indices$angstrom <- NULL
indices$hotDryWindy <- NULL
indices$fuelMoisture <- NULL
indices$fosberg <- NULL
indices$chandler <- NULL
indices$grasslandMk4 <- NULL
indices$Chandler <- NULL

##indices = setNames(reshape2::melt(indices,id="Date"),c("Date","Index","Value"))

indices <- cbind(A,indices)
##ggplot(indices,aes(Date,Value,group=Index,color=Index))+geom_smooth(span = .1,  method = "loess", se = F) +theme_classic()+coord_cartesian(expand=F)
plot(indices$A, indices$grasslandMk5, type ="h") 
indice_1AN <- indices$grasslandMk5[10602:10967]
plot(indices$A[10602:10967], indice_1AN, type = "b")
plot(McArthur_Data$A[10602:10967], McArthur_Data$T[10602:10967], type = "b")
plot(McArthur_Data$A[10602:10967], McArthur_Data$U[10602:10967], type = "b")
plot(McArthur_Data$A[10602:10967], McArthur_Data$VT[10602:10967], type = "b")

#data(McArthur_Data)
#daily.precip = McArthur_Data
#daily.precip$A = strptime(daily.precip$A, "%Y/%m/%d")
#daily.precip = setNames(aggregate(daily.precip$RR, by = list(as.character(daily.precip$Date)), 
#                                  FUN = sum), c("Date", "DailyPrecip"))
#McArthur_Data.daily = McArthur_Data[format(strptime(McArthur_Data$dateTime, "%m/%d/%Y %H:%M"), "%H:%M") == 
#                                        "14:35", ]
#McArthur_Data.daily$DailyPrecip = daily.precip$DailyPrecip
TT <- cbind.data.frame(McArthur_Data$T,rr,u,vt)
TT <- na.omit(TT)
#indices = fireIndexKBDI(TT$`McArthur_Data$T`, TT$RR, 
#                                             "%Y/%m/%d"))
#indices = setNames(reshape2::melt(indices, id = "Date"), c("Date", "Index", "Value"))
#ggplot(indices, aes(Date, Value, group = Index, color = Index)) + geom_smooth(span = 0.1, 
#                                                                              method = "loess", se = F) + theme_classic() + coord_cartesian(expand = F)


#McArthur_Data_ma = subset(McArthur_Data, U <= 70 & T >= 10 & U > 42.5 - 1.25 * T & 
#                     U < 94.5 - 1.35 * T)
#ff.ma = data.frame(ffm = ffm("mcarthur", McArthur_Data_ma$U, McArthur_Data_ma$T)$fm1hr, method = "mcarthur", 
#                   A = McArthur_Data_ma$A)





##format(tmp, "%Y")
##years=as.numeric(format(as.Date(McArthur_Data$A, format = "%Y-%m-%d"), "%Y"))
##cumuls = aggregate(McArthur_Data$RR, by = list(annee = years), FUN = sum)
##map = mean(cumuls)




summary(indices)
indices <- na.omit(indices)
hist(indices$grasslandMk5, freq=F, breaks = 35,xlab = "valeurs du McArthur",col = "blue",border = "black")
lines(density(indices$grasslandMk5), col='red', lwd = 3)
#den <- density(indices$grasslandMk5)
#plot(den, frame = FALSE, col = "blue",main = "Density plot")

acf(indices$grasslandMk5, na.action = na.pass, lag.max = 365)
acf(indices$grasslandMk5, na.action = na.pass, lag.max = 21)

###

nb_iteration = nrow(indices)
Index_Summer <<- data.frame(DATE=numeric(), indices=numeric(), stringsAsFactors=FALSE)
for (i in 1:nb_iteration){
  
  month <- month(as.Date(indices[i,]$A, origin="1970-01-01"))
  if(month == 6 || month == 7 || month == 8){
    Index_Summer[nrow(Index_Summer)+1,] <- indices[i,]
  }
  
}


###

prob = .95 # la probablité correspondante au seuil u
tmp1 = atdf(as.ts(indices$grasslandMk5), u = prob, plot = FALSE, lag.max = 40, type = "rho")
par(mar = c(5, 5, .5, .5), cex.lab = 2, cex.axis = 2, lwd = 2, cex = 1)
plot(tmp1, main = "", xlab = "h", ylab = expression(chi(u,h)))
lines(c(0, 400), rep(1-prob,2), lwd = 2, lty = 2, col = "red")

prob = .95 # probability for quantile to be used as threshold
tmp3 = atdf(as.ts(Index_Summer$indices), u = prob, plot = FALSE, lag.max = 60, type = "rho")
par(mar = c(5, 5, .5, .5), cex.lab = 2, cex.axis = 2, lwd = 2, cex = 1)
plot(tmp3, main = "", xlab = "h", ylab = expression(chi(u,h)))
lines(c(0, 100), rep(1-prob,2), lwd = 2, lty = 2, col = "red")


###

moment_data <- na.omit(indices)
moment_data <- moment_data[moment_data$grasslandMk5 > 0,]
M <- Moment(moment_data$grasslandMk5, logk = FALSE, plot = TRUE, main = "Moment estimate of the McArthur Index")
plot(M$k[M$k %in% 25:500], M$gamma[M$k %in% 25:500], main="", xlab="Number of extreme events", ylab="Tail index")





extremal_correlation_input_output <- function(input,output, quant){
  u1 = quantile(output, quant, na.rm = TRUE)
  u2 = quantile(input, quant, na.rm = TRUE)
  xi.est.quantile = sum(output > u1 & input > u2)/sum(output > u1)
  
  xi.est.quantile
}


quant = 0.95
indices_omit <- fireIndex(McArthur_Data$T, McArthur_Data$VT, McArthur_Data$U)
indices_omit$Date <- NULL
indices_omit$angstrom <- NULL
indices_omit$hotDryWindy <- NULL
indices_omit$fuelMoisture <- NULL
indices_omit$fosberg <- NULL
indices_omit$chandler <- NULL
indices_omit$grasslandMk4 <- NULL
indices_omit$Chandler <- NULL
indices_omit <- cbind.data.frame(A,indices_omit,McArthur_Data$T,McArthur_Data$U,McArthur_Data$VT,McArthur_Data$RR)
indices_omit <- na.omit(indices_omit)

indices_omit_reverse <- indices_omit
indices_omit_reverse$grasslandMk5 <- indices_omit_reverse$grasslandMk5 *-1
min_indices_omit_reverse <- min(indices_omit_reverse$grasslandMk5)
indices_omit_reverse$grasslandMk5 <- indices_omit_reverse$grasslandMk5 - min_indices_omit_reverse +1

extremal_correlation_input_output(indices_omit$`McArthur_Data$T`, indices_omit$grasslandMk5,quant)
extremal_correlation_input_output(indices_omit_reverse$`McArthur_Data$U`, indices_omit_reverse$grasslandMk5,quant)
extremal_correlation_input_output(indices_omit$`McArthur_Data$VT`, indices_omit$grasslandMk5,quant)
extremal_correlation_input_output(indices_omit$`McArthur_Data$RR`, indices_omit$grasslandMk5,quant)









extract_function = function(vec, nr_of_na_allowed = 60){
  if(sum(is.na(vec)) > nr_of_na_allowed){
    return(NA)
  }else{
    return(max(vec, na.rm = TRUE))
  }
}

tmp = aggregate(indices$grasslandMk5, by = list(year = indices$A), FUN = extract_function)
mean(is.na(tmp$x)) # proportion of years with too many NA data
# observations of maxima
obs = tmp$x
plot(tmp$year, tmp$x, type = "b", pch = 19, xlab = "Year", ylab = "Maximum")

fit = fevd(obs, type = "GEV", method = "MLE", period.basis = "year", use.phi = TRUE)
summary(fit)
plot(fit)
plot(fit, type = "qq", main = "")
plot(fit, type = "rl", main = "")

# fit a stationary model:
fit = fevd(obs, type = "GEV", method = "MLE", period.basis = "year", use.phi = TRUE)
summary(fit)
plot(fit)

# données maximales annuelles :
MAX = data.frame()
maxima = aggregate(indices$grasslandMk5, by = list(year = indices$A), FUN = extract_function)
print(max(maxima[98:458,2]))
for (i in 1:366){
  for (k in 1:28){
    MAX <- c(max(maxima[(i*k):(i*(k+1)),2]))
  }
}
maxima = na.omit(maxima)

# modèle stationnaire de la GEV :
fit_stationary = fevd(maxima$x, type = "GEV", method = "MLE", period.basis = "year", proposalParams = 1, use.phi = TRUE)
summary(fit_stationary)
# La matrice de covariance est bien symétrique

plot(fit_stationary, type = "qq2")

# La droite y=x sort de l'intervalle de confiance à 95%, ce qui veut dire que 
# les données ne sont pas parfaitement représentative du modèle.
plot(fit_stationary, type = "density", main = "")
# La courbe expérimentale respecte plutôt bien la forme du modèle stationnaire.
# Il reste à comparer avec le modèle non-stationnaire.

# Nous allons utiliser un modèle non-stationnaire à dépendance linéaire en temps pour la moyenne et l'écart-type de la GEV:
fit_linaire = fevd(maxima$x, data = data.frame(year = maxima$year), scale.fun = ~1+year, location.fun = ~1+year, type = "GEV", method = "MLE", period.basis = "year", use.phi = TRUE)
summary(fit_linaire)
plot(fit_linaire, type = "qq2")
# La droite y=x est beaucoup plus proche de la régression linéaire et reste dans
# l'intervalle de confiance à 95%.
plot(fit_linaire, type = "density", main = "")




########


#U_complet<- rbind(U_81_99,U_2000_2021)

#VT_complet<- rbind(VT_81_99,VT_2000_2021)
#T_complet<- rbind(T_81_99,T_2000_2021)
#RR_complet<- rbind(RR_81_99,RR_2000_2021)
#length(RR_complet$RR)
#length(T_complet$T)
RR_81_99 <- na.omit(RR_81_99)
T_81_99 <- na.omit(T_81_99)
U_81_99<- na.omit(U_81_99)
VT_81_99<- na.omit(VT_81_99)
RR_2000_2021<- na.omit(RR_2000_2021)
T_2000_2021<- na.omit(T_2000_2021)
U_2000_2021<- na.omit(U_2000_2021)
VT_2000_2021<- na.omit(VT_2000_2021)

RR_complet <- rbind(RR_81_99[RR_81_99$HEURE == 15,],RR_2000_2021[RR_2000_2021$HEURE == 15,])
T_complet <- rbind(T_81_99[T_81_99$HEURE == 15,],T_2000_2021[T_2000_2021$HEURE == 15,])
U_complet <- rbind(U_81_99[U_81_99$HEURE == 15,],U_2000_2021[U_2000_2021$HEURE == 15,])
VT_complet <- rbind(VT_81_99[VT_81_99$HEURE == 15,],VT_2000_2021[VT_2000_2021$HEURE == 15,])

263232-length(na.omit(T_complet$T))
263232-length(na.omit(VT_complet$VT))
263232-length(na.omit(U_complet$U))
263232-length(na.omit(RR_complet$RR))

RR_complet[is.na(RR_complet)]<-0.06693427
T_complet[is.na(T_complet)]<-14.59049
VT_complet[is.na(VT_complet)]<-13.60234
U_complet[is.na(U_complet)]<-67.76571
length(na.omit(RR_complet$RR))
length(na.omit(T_complet$T))
length(na.omit(VT_complet$VT)) 
length(na.omit(U_complet$U))

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

nu <- function(H,t){
  
  resu2=1 -2*((EMC(H,t))/30) + 1.5*((EMC(H,t))/30)*((EMC(H,t))/30) + 0.5*((EMC(H,t))/30)*((EMC(H,t))/30)*((EMC(H,t))/30)
  
  return(resu2)
}

FFWI<- function(H,t,V){return((nu(H,t)*sqrt(1+V*V))/0.3002)}
FFWI(U_complet$U[length(U_complet)],T_complet$T[length(T_complet)],VT_complet$VT[length(VT_complet)])



FFwi_values<-cbind(T_complet)
names(FFwi_values)[names(FFwi_values) == "T"] <- "FFwi"

FFwi_values$FFwi[1]<-0
FFwi_values$FFwi[2]<-0
for (i in 1:length(FFwi_values$FFwi))  {
  
  FFwi_values$FFwi[i]<- FFWI(U_complet$U[i],T_complet$T[i],VT_complet$VT[i])
  
}


FWI <- cbind.data.frame(indices$A,FFwi_values$FFwi)

x <- cbind.data.frame(indices$grasslandMk5,FWI$`FFwi_values$FFwi`)
x <- na.omit(x)

cor(x, method = c("pearson", "kendall", "spearman"))

chiplot(x, qlim = c(0.8, .9), xlim = c(0.8, .9), which = 1)

