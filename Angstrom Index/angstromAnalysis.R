acf(Angstrom_Index, na.action = na.pass)

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

extremal_correlation_input_output <- function(input, quant){
  u1 = quantile(join_index_temperature2$ANGSTROM_INDEX, quant, na.rm = TRUE)
  u2 = quantile(input, quant, na.rm = TRUE)
  xi.est.quantile = sum(join_index_temperature2$ANGSTROM_INDEX > u1 & input > u2)/sum(join_index_temperature2$ANGSTROM_INDEX > u1)
  
  xi.est.quantile
}

quant = 0.95
extremal_correlation_input_output(join_index_temperature2$T,quant)
extremal_correlation_input_output(join_index_temperature2$U,quant)
