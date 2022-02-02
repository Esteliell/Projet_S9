acf(Angstrom_Index, na.action = na.pass)

#in date numerics, +1 = +1 day

acf(Angstrom_Index$ANGSTROM_INDEX, na.action = na.pass, lag.max = 21)

acf(Index_Summer$ANGSTROM_INDEX, na.action = na.pass, lag.max = 21)

# ==> bonne corrélation sur les dates été (juin inclus)

#d'abord *-1 puis additionner le minimum de toutes les valeurs
#une fois fait envoyer par mail pour avoir un retour
#autres estimateurs: Hill estimator, 

moment_data <- na.omit(Angstrom_Index)
moment_data <- moment_data[moment_data$ANGSTROM_INDEX >= 0,]
M <- Moment(moment_data$ANGSTROM_INDEX, logk = TRUE, plot = TRUE, main = "Moment estimate of the Angstrom Index")
