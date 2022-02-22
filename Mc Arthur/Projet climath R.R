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

ff = rbind(
  data.frame(ffm = ffm("simard",rrRAWS$rh, rrRAWS$temp_c)$fm1hr,method="simard"),
  data.frame(ffm = ffm("wagner",rrRAWS$rh, rrRAWS$temp_c)$fm1hr,method="wagner"),
  data.frame(ffm = ffm("anderson",rrRAWS$rh, rrRAWS$temp_c)$fm1hr,method="anderson")
)


ggplot2::ff(ffm, color = method, fill = method) + geom_density(alpha = 0.5) + 
         xlab("Fine fuel moisture (%)") + theme_classic()


McArthur_Data <<- data.frame(DATE = numeric(), T = numeric(), RR = numeric(), U = numeric(), VT = numeric(), stringsAsFactors =  FALSE)

for (i in 1:nrow(RR_15h)){
  
  ref_date <- T_15h[i,]
  t <- T_15h[i,]$T
  rr <- RR_15h[i,]$RR
  u <- U_15h[i,]$U
  vt <- VT_15h[i,]$VT
  
  McArthur_Data[nrow(McArthur_Data)+1,] <<- data.frame(gsub(" ", "", paste(ref_date$JOUR,"/",ref_date$MOIS,"/",ref_date$AN)), format="%d/%m/%Y", origin="1970-01-01")
}

##McArthur_Data[nrow(McArthur_Data)+1,] <<- c(as.Date(gsub(" ", "", paste(ref_date$JOUR,"/",ref_date$MOIS,"/",ref_date$AN)), format="%d/%m/%Y", origin="1970-01-01"),t,rr,u,vt)

tableau <- cbind(data.frame(T_15h),
                 data.frame(U_15h),
                 data.frame(VT_15h),
                 data.frame(RR_15h)
)

tableaubis <- tableau[, -c(7:11)]
tableauter <- tableaubis[, -c(8:12)]
tab <- tableauter[, -c(9:13)]

rrRAWS.daily =   rrRAWS[format(strptime(rrRAWS$dateTime, "%m/%d/%Y %H:%M"), "%H:%M")=="15:00",]
indices = fireIndex(temp=rrRAWS.daily$temp_c, u= rrRAWS.daily$windSpeed_kmh, rh = rrRAWS.daily$rh)
normalize=function(x,low=0,high=1){low+(x-min(x,na.rm=T))*(high-low)/(max(x,na.rm=T)-min(x,na.rm=T))}
indices = data.frame(sapply(indices,normalize),  Date = strptime(rrRAWS.daily$dateTime, "%m/%d/%Y %H:%M"))
indices = setNames(reshape2::melt(indices,id="Date"),c("Date","Index","Value"))

ggplot(indices,aes(Date,Value,group=Index,color=Index))+geom_smooth(span = .1,  method = "loess", se = F) +theme_classic()+coord_cartesian(expand=F)


data(rrRAWS)
daily.precip = rrRAWS
daily.precip$Date = strptime(daily.precip$dateTime, "%m/%d/%Y")
daily.precip = setNames(aggregate(daily.precip$precip_mm, by = list(as.character(daily.precip$Date)), 
                                  FUN = sum), c("Date", "DailyPrecip"))
rrRAWS.daily = rrRAWS[format(strptime(rrRAWS$dateTime, "%m/%d/%Y %H:%M"), "%H:%M") == 
                        "14:35", ]
rrRAWS.daily$DailyPrecip = daily.precip$DailyPrecip
indices = fireIndexKBDI(temp = rrRAWS.daily$temp_c, precip = rrRAWS.daily$DailyPrecip, 
                        map = 610, rh = rrRAWS.daily$rh, u = rrRAWS.daily$windSpeed_kmh)
indices = data.frame(sapply(indices,normalize), Date = strptime(rrRAWS.daily$dateTime, 
                                                                "%m/%d/%Y %H:%M"))
indices = setNames(reshape2::melt(indices, id = "Date"), c("Date", "Index", "Value"))
ggplot(indices, aes(Date, Value, group = Index, color = Index)) + geom_smooth(span = 0.1, 
                                                                              method = "loess", se = F) + theme_classic() + coord_cartesian(expand = F)


rrRAWS_ma = subset(rrRAWS, rh <= 70 & temp_c >= 10 & rh > 42.5 - 1.25 * temp_c & 
                     rh < 94.5 - 1.35 * temp_c)
ff.ma = data.frame(ffm = ffm("mcarthur", rrRAWS_ma$rh, rrRAWS_ma$temp_c)$fm1hr, method = "mcarthur", 
                   dateTime = rrRAWS_ma$dateTime)

