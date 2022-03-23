acf(Angstrom_Index, na.action = na.pass)

hist(na.omit(Angstrom_Index$ANGSTROM_INDEX), freq=F, col='green', breaks=35, xlab='Valeurs de Angstrom Index', main='La distribution de Angstrom Index')
lines(density(na.omit(Angstrom_Index$ANGSTROM_INDEX), col='red',lwd=3))
      
      acf(Angstrom_Index$ANGSTROM_INDEX, na.action = na.pass, lag.max = 21, main="Auto-corrélation temporelle de l'indice")
      acf(Index_Summer_Angstrom$ANGSTROM_INDEX, na.action = na.pass, lag.max = 21, main ="Auto-corrélation temporelle de l'indice pris sur les périodes estivales")
      
summary(na.omit(Angstrom_Index$ANGSTROM_INDEX))
      
#in date numerics, +1 = +1 day

acf(Angstrom_Index$ANGSTROM_INDEX, na.action = na.pass, lag.max = 21)

acf(Index_Summer$ANGSTROM_INDEX, na.action = na.pass, lag.max = 21)

# ==> bonne corrélation sur les dates été (juin inclus)

moment_data <- na.omit(Angstrom_Index)
moment_data <- moment_data[moment_data$ANGSTROM_INDEX > 0,]

M <- Moment(moment_data$ANGSTROM_INDEX, logk = TRUE, plot = TRUE, main = "Moment estimate of the Angstrom Index")

#d'abord *-1 puis additionner le minimum de toutes les valeurs
#une fois fait envoyer par mail pour avoir un retour
#autres estimateurs: Hill estimator, 

moment_data <- na.omit(Angstrom_Index)
moment_data$ANGSTROM_INDEX <- moment_data$ANGSTROM_INDEX *-1
min_moment <- min(moment_data$ANGSTROM_INDEX)
moment_data$ANGSTROM_INDEX <- moment_data$ANGSTROM_INDEX - min_moment +1
moment_data <- moment_data[moment_data$ANGSTROM_INDEX > 0,]
M <- Moment(moment_data$ANGSTROM_INDEX, logk = FALSE, plot = TRUE, main = "Moment estimate of the Angstrom Index 2")

plot(M$k[M$k %in% 25:500], M$gamma[M$k %in% 25:500])

###

moment_summer <- na.omit(Index_Summer)
prob = .95 # probability for quantile to be used as threshold
tmp = atdf(as.ts(moment_summer$ANGSTROM_INDEX), u = prob, plot = FALSE, lag.max = 21, type = "rho")
par(mar = c(5, 5, .5, .5), cex.lab = 2, cex.axis = 2, lwd = 2, cex = 1)
plot(tmp, main = "", xlab = "h", ylab = expression(chi(u,h)))
lines(c(0, 100), rep(1-prob,2), lwd = 2, lty = 2, col = "red")

#### ANALYSE INDICES D'ENTREES ####

library(dplyr)

join_index_temperature <- inner_join(na.omit(Angstrom_Index),na.omit(T_13h),by=c("AN","MOIS", "JOUR"))
join_index_temperature <- inner_join(join_index_temperature, na.omit(U_13h),by=c("AN","MOIS", "JOUR"))
join_index_temperature <- select(test, "DATE", "AN", "MOIS", "JOUR", "ANGSTROM_INDEX", "T","U")

join_index_temperature2 <- join_index_temperature
join_index_temperature2$ANGSTROM_INDEX <- join_index_temperature2$ANGSTROM_INDEX *-1
min_join_index_temperature2 <- min(join_index_temperature2$ANGSTROM_INDEX)
join_index_temperature2$ANGSTROM_INDEX <- join_index_temperature2$ANGSTROM_INDEX - min_join_index_temperature2 +1

extremal_correlation_input_output <- function(input,output, quant){
  u1 = quantile(output, quant, na.rm = TRUE)
  u2 = quantile(input, quant, na.rm = TRUE)
  xi.est.quantile = sum(output > u1 & input > u2)/sum(output > u1)
  
  xi.est.quantile
}

quant = 0.95
extremal_correlation_input_output(join_index_temperature2$T, join_index_temperature2$ANGSTROM_INDEX,quant)
extremal_correlation_input_output(join_index_temperature$U,join_index_temperature$ANGSTROM_INDEX,quant)

#voir avec des classifications

extract_function = function(vec, nr_of_na_allowed = 30){
  if(sum(is.na(vec)) > nr_of_na_allowed){
    return(NA)
  }else{
    return(max(vec, na.rm = TRUE))
  }
}

library(evd)
tmp = aggregate(Angstrom_Index$ANGSTROM_INDEX, by = list(year = Angstrom_Index$AN), FUN = extract_function)
mean(is.na(tmp$x)) # proportion of years with too many NA data
# observations of maxima
obs = tmp$x

ggplot(data = tmp,
       mapping = aes(x = year, y = x)) + geom_line()  + labs(
         title    = "Maxima annuel sur les années avec moins de 30 NA",
         x        = "Year",
         y        = "Maximum")
fit = fevd(na.omit(obs), type = "GEV", method = "MLE", period.basis = "year", use.phi = TRUE)
summary(fit)
plot(fit, type="density", main="")
