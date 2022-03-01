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

