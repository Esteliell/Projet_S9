
# 3) Extracting and modeling annual maxima ####

# There are many possibilities to extract the yearly maxima, for instance by using for-loops. 
# Here, we make use of the aggregate-function of R. 
# Note that there are some missing data. We calculate the yearly maxima using only the available observations (if there are less than 60 missing observations in a year).

# function to extract the maximum of a block of observations:
# vec = vector of observations in a block
# nr_of_na_allowed = number of observations in a block that may be NA 
# (otherwise, we return NA for the block maximum)


extract_function = function(vec, nr_of_na_allowed = 60){
  if(sum(is.na(vec)) > nr_of_na_allowed){
    return(NA)
  }else{
    return(max(vec, na.rm = TRUE))
  }
}

# yearly humidity  maxima ####

tmpU = aggregate(U_complet$U, by = list(year = U_complet$AN), FUN = extract_function)
#view(tmp)
mean(is.na(tmpU$x)) # proportion of years with too many NA data
# observations of maxima
obsU = tmpU$x
#plot(tmpU$year, tmpU$x, type = "b", pch = 19, xlab = "Year", ylab = "Maximum")


#yearl temperature maxima ####

tmpT = aggregate(T_complet$T, by = list(year = T_complet$AN), FUN = extract_function)
#view(tmp)
mean(is.na(tmpT$x)) # proportion of years with too many NA data
# observations of maxima
obsT = tmpT$x
#plot(tmpT$year, tmpT$x, type = "b", pch = 19, xlab = "Year", ylab = "Maximum")


# yearly Wind maxima ####

tmpVT = aggregate(VT_complet$VT, by = list(year = VT_complet$AN), FUN = extract_function)
#view(tmp)
mean(is.na(tmpVT$x)) # proportion of years with too many NA data
# observations of maxima
obsVT = tmpVT$x
#plot(tmpVT$year, tmpVT$x, type = "b", pch = 19, xlab = "Year", ylab = "Maximum")


# yearly ffwi maxima ####

tmp = aggregate(FFwi_values$FFwi, by = list(year = FFwi_values$AN), FUN = extract_function)
#view(tmp)
mean(is.na(tmp$x)) # proportion of years with too many NA data
# observations of maxima
obs = tmp$x
#plot(tmp$year, tmp$x, type = "b", pch = 19, xlab = "Year", ylab = "Maximum")


# We here use the extRemes package. Several other packages implement maximum likelihood estimation of the GEV and GPD (e.g., packages evd, fExtremes)
#install.packages('extRemes')

par(mfrow=c(2,2))

plot(tmp$year, tmp$x, type = "b", pch = 19, xlab = "Year", ylab = "Maximum")
plot(tmpVT$year, tmpVT$x, type = "b", pch = 19, xlab = "Year", ylab = "Maximum")
plot(tmpT$year, tmpT$x, type = "b", pch = 19, xlab = "Year", ylab = "Maximum")
plot(tmpU$year, tmpU$x, type = "b", pch = 19, xlab = "Year", ylab = "Maximum")




library(extRemes)

# view the help package of the fevd function (which offers a lot of functionality!)
?fevd


# fit a stationary model to ffwi:

fit = fevd(obs, type = "GEV", method = "MLE", period.basis = "year", use.phi = TRUE)
summary(fit)
plot(fit)
plot(fit, type = "qq", main = "")


#créer deux colonnes puis enlever les Nan au meme temps 





