library(readr)
devtools::install_github("EcoFire/firebehavioR")
library(firebehavioR)
library(ggplot2)
library(lubridate)
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
indices = fireIndex(t, vt, u)
normalize=function(x,low=0,high=1){low+(x-min(x,na.rm=T))*(high-low)/(max(x,na.rm=T)-min(x,na.rm=T))}
indices = data.frame(sapply(indices,normalize),  Date = strptime(McArthur_Data.daily$A, "%Y/%m/%d"))
indices = setNames(reshape2::melt(indices,id="Date"),c("Date","Index","Value"))

ggplot(indices,aes(Date,Value,group=Index,color=Index))+geom_smooth(span = .1,  method = "loess", se = F) +theme_classic()+coord_cartesian(expand=F)


data(McArthur_Data)
daily.precip = McArthur_Data
daily.precip$A = strptime(daily.precip$A, "%Y/%m/%d")
daily.precip = setNames(aggregate(daily.precip$RR, by = list(as.character(daily.precip$Date)), 
                                  FUN = sum), c("Date", "DailyPrecip"))
#McArthur_Data.daily = McArthur_Data[format(strptime(McArthur_Data$dateTime, "%m/%d/%Y %H:%M"), "%H:%M") == 
#                                        "14:35", ]
#McArthur_Data.daily$DailyPrecip = daily.precip$DailyPrecip
indices = fireIndexKBDI(temp = McArthur_Data$T, precip = McArthur_Data$RR, 
                        map = 610, U = McArthur_Data$u, u = McArthur_Data$VT)
indices = data.frame(sapply(indices,normalize), A = strptime(McArthur_Data$A, 
                                                                "%Y/%m/%d"))
indices = setNames(reshape2::melt(indices, id = "Date"), c("Date", "Index", "Value"))
ggplot(indices, aes(Date, Value, group = Index, color = Index)) + geom_smooth(span = 0.1, 
                                                                              method = "loess", se = F) + theme_classic() + coord_cartesian(expand = F)


McArthur_Data_ma = subset(McArthur_Data, U <= 70 & T >= 10 & U > 42.5 - 1.25 * T & 
                     U < 94.5 - 1.35 * T)
ff.ma = data.frame(ffm = ffm("mcarthur", McArthur_Data_ma$U, McArthur_Data_ma$T)$fm1hr, method = "mcarthur", 
                   A = McArthur_Data_ma$A)





##format(tmp, "%Y")
##years=as.numeric(format(as.Date(McArthur_Data$A, format = "%Y-%m-%d"), "%Y"))
##cumuls = aggregate(McArthur_Data$RR, by = list(annee = years), FUN = sum)
##map = mean(cumuls)
