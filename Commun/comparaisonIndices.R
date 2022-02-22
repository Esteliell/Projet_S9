Angstrom <- na.omit(Angstrom_Index$ANGSTROM_INDEX)
fwi <- na.omit(fwi_resultat$FWI)

cor(Angstrom_Index$ANGSTROM_INDEX, fwi_resultat$FWI, method = c("pearson", "kendall", "spearman"))

u1 = quantile(Angstrom, 0.95)
u2 = quantile(fwi, 0.95)
xi.est.95 = sum(Angstrom > u1 & fwi > u2)/sum(Angstrom > u1)

