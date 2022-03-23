library(dplyr)


test <- inner_join(na.omit(Angstrom_Index),na.omit(fwi_resultat),by=c("AN","MOIS", "JOUR"))
test <- select(test, "DATE", "AN", "MOIS", "JOUR", "ANGSTROM_INDEX", "FWI")

test2 <- test
test2$ANGSTROM_INDEX <- test2$ANGSTROM_INDEX *-1
min_test <- min(test2$ANGSTROM_INDEX)
test2$ANGSTROM_INDEX <- test2$ANGSTROM_INDEX - min_test +1

cor(test2$ANGSTROM_INDEX, test2$FWI, method = "spearman")
#pearson et spearman. Pearson = cor classique (sensible ? series tr?s extr?mes /aberrantes), spearman + robuste la-dessus

tail_correlation <- function(quant){
  u1 = quantile(test2$ANGSTROM_INDEX, quant)
  u2 = quantile(test2$FWI, quant)
  xi.est.quantile = sum(test2$ANGSTROM_INDEX > u1 & test2$FWI > u2)/sum(test2$ANGSTROM_INDEX > u1)
  
  xi.est.quantile
}

tail_correlation(0.95)
tail_correlation(0.98)
tail_correlation(0.99)
#faire lanalyse pour diff?rents nvx quantiles : 95%, voir 98% / 99% 
# !! angstrom valeurs faibles, on prend 5% et on regarde le < mais plus simple : on prend -angstrom comme auto-correl

#pickands dependance function

chiplot(cbind(test2$ANGSTROM_INDEX, test2$FWI), qlim = c(0.8, .99), xlim = c(0.8, .9995), which = 1)

M1 = aggregate(test2$ANGSTROM_INDEX, by = list(year = test2$AN), FUN = max)$x
M2 = aggregate(test2$FWI, by = list(year = test2$AN), FUN = max)$x

omega = 0:26/26 # define values where to evaluate A
pickands = abvnonpar(omega, data = cbind(M1,M2), method = "tdo", plot = TRUE) 
