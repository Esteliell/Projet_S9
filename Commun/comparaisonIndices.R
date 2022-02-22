Angstrom <- na.omit(Angstrom_Index$ANGSTROM_INDEX)
fwi <- na.omit(fwi_resultat$FWI)


#pour adapter la longueur : boucle sur la date pour créer un simili-bind et enlever les différents NA pour avoir la même length
# et pouvoir utiliser les méthodes d'analyse entre séries

cor(Angstrom_Index$ANGSTROM_INDEX, fwi_resultat$FWI, method = c("pearson", "kendall", "spearman"))
#pearson et spearman. Pearson = cor classique (sensible à series très extrêmes /aberrantes), spearman + robuste la-dessus

u1 = quantile(Angstrom, 0.95)
u2 = quantile(fwi, 0.95)
xi.est.95 = sum(Angstrom > u1 & fwi > u2)/sum(Angstrom > u1)
#faire lanalyse pour différents nvx quantiles : 95%, voir 98% / 99% 
# !! angstrom valeurs faibles, on prend 5% et on regarde le < mais plus simple : on prend -angstrom comme auto-correl

