
# set working directory
WORK = "~/research/enseignement/Ecole-Centrale-Marseille/code/"
setwd(WORK)

# 1) Import data (as before) ####
data_df = read.csv("marseille-marignane.csv")
dim(data_df)
head(data_df)

# let's keep only the dates and the observed weather variables:
data_df = data_df[c("DATE", "TAVG", "TMIN", "TMAX", "PRCP")]

summary(data_df)
# some NA values, especially in TAVG

class(data_df$DATE)
#transform to Date class:
data_df$DATE = as.POSIXlt(data_df$DATE)
years = 1900 + data_df$DATE$year
months = data_df$DATE$mon+1
table(months)
data_df$years = years
data_df$months = months

# we remove data before 1975 and data of 2020 (too many NA values):
data_df = data_df[data_df$years >= 1975 & data_df$years < 2020, ]
mean(is.na(data_df))


# 5) Methods for dependent series ####

library(extRemes)
library(evd)

# 5.1) Estimation of the tail correlation function for TMAX ####

prob = .975 # probability for quantile to be used as threshold
tmp = atdf(as.ts(data_df$TMAX), u = prob, plot = FALSE, lag.max = 50, type = "rho")
par(mar = c(5, 5, .5, .5), cex.lab = 2, cex.axis = 2, lwd = 2, cex = 1)
plot(tmp, main = "", xlab = "h", ylab = expression(chi(u,h)))
lines(c(0, 100), rep(1-prob,2), lwd = 2, lty = 2, col = "red")
# (We can see that temporal extremal dependance looks quite strong over relatively large lags.) 
# What if we use a higher threshold?
prob = .995 # probability for quantile to be used as threshold
tmp = atdf(as.ts(data_df$TMAX), u = prob, plot = FALSE, lag.max = 50, type = "rho")
par(mar = c(5, 5, .5, .5), cex.lab = 2, cex.axis = 2, lwd = 2, cex = 1)
plot(tmp, main = "", xlab = "h", ylab = expression(chi(u,h)))
lines(c(0, 100), rep(1-prob,2), lwd = 2, lty = 2, col = "red")
# (Strength of temporal dependence seems to decrease when taking a higher threshold.)
# What if we use a nonstationary, montly threshold?
# (here we have to use a trick by shifting the observations according to the threshold)
u_by_month = aggregate(data_df$TMAX, FUN = quantile, probs = 0.975, by = list(month = data_df$months), na.rm = TRUE)$x
data_df$u_by_month = u_by_month[data_df$months]
# to get a nonstationary threshold, we shift the data by the nonstationary threshold:
prob = .975 
tmp = atdf(as.ts(data_df$TMAX-data_df$u_by_month), u = prob, plot = FALSE, lag.max = 50, type = "rho")
plot(tmp, main = "", xlab = "h", ylab = expression(chi(u,h)))
lines(c(0, 100), rep(1-prob,2), lwd = 2, lty = 2, col = "red")
# The temporal extremal dependence is less strong now. This means that the nonstationarity in the original data has generated strong clustering of extremes.

# 5.2) Estimation of the extremal index for TMAX ####

u = quantile(data_df$TMAX, .975)
# here, we assume that two clusters are distinct if they are separated by at least two non-exceedances
ei_est = exi(data_df$TMAX, u = u, r = 2)
ei_est
# what is the mean cluster length?
1/ei_est

# can we have confidence intervals?
# (not with the function exi, but with the extremalindex function)
ei_est = extremalindex(data_df$TMAX, threshold = u, method = "runs", run.length = 2)
ei_est
ci(ei_est, alpha = .05, R = 502) # bootstrap-based confidence interval

# let's have a detailed look at the clusters:
cl = clusters(data_df$TMAX, u = u, r = 2)
class(cl)
length(cl) # number of clusters
# cluster 1:
cl[[1]]
# cluster 15:
cl[[15]]
# extract cluster lengths:
cl_lengths = sapply(cl, length)
hist(cl_lengths)
# the clusters-function allows us to plot clusters:
# (but we have to customize it since the data series is very long)
# (here, plot only data for two years:)
clusters(data_df$TMAX, u = u, r = 2, plot = TRUE, xlim = c(0,2*365), col = "red", lvals = FALSE)

# 5.3) Estimation of a GPD for excesses of cluster maxima of TMAX ####

# first, extract the cluster maxima using the runs method:
obs_gpd = clusters(data_df$TMAX, u = u, r = 2, cmax = TRUE)
length(obs_gpd)
fit = fevd(x = obs_gpd, threshold = u, type = "GP", method = "MLE", use.phi = TRUE)
summary(fit)
plot(fit)
# compare this to a fit without declustering: 
fit = fevd(x = data_df$TMAX, threshold = u, type = "GP", method = "MLE", use.phi = TRUE)
summary(fit) # parameter estimates look quite similar
plot(fit)

# 6) Methods for multivariate extremes ####

# We here consider bivariate extremes of TMIN and TMAX. 

dev.off() # close previous plots

# 6.1) Empirical estimation of the tail correlation measure chi 
chiplot(cbind(data_df$TMIN, data_df$TMAX), qlim = c(0.8, .9995), xlim = c(0.8, .9995), which = 1)
# ("which = 1" corresponds to the plot for chi)
# Tail correlation (chi) decreases slightly for higher threshold levels u but remains separated from 0 even at high quantiles. 
# Therefore, the presence of asymptotic dependence is a good working assumption. 
# Note: the taildep-function (package extRemes) provides similar functionality as chiplot.

# 6.2) Nonparametric estimation of Pickands dependence function for componentwise maxima

# first, extract the vectors of componentwise annual maxima: 
M1 = aggregate(data_df$TMAX, by = list(year = data_df$years), FUN = max)$x
M2 = aggregate(data_df$TMIN, by = list(year = data_df$years), FUN = max)$x

# now compute a semi-parametric estimate of the Pickands function A using the block maxima:
dev.off() # reset plotting device
omega = 0:26/26 # define values where to evaluate A
pickands = abvnonpar(omega, data = cbind(M1,M2), method = "cfg", plot = TRUE)  
# estimated bivariate extremal coefficient:
extcoef = 2*abvnonpar(0.5, data = cbind(M1,M2), method = "cfg")  
extcoef 

# 5.3) Estimation of a parametric model for bivariate componentwise block maxima

# fit a bivariate logistic model with GEV margins
fit1 = fbvevd(cbind(M1,M2), model = "log")
fit1
plot(fit1) # various illustrations
# some details about the plots:
?plot.bvevd

# fit a bivariate Huesler-Reiss model with GEV margins
fit2 = fbvevd(cbind(M1,M2), model = "hr")
fit2 
# AIC is slightly lower than for logistic model
# --> we may prefer the HR model
plot(fit2)

# check differences in estimated Pickands dependence functions:
plot(fit1, which = 4, lwd = 2)
plot(fit2, which = 4, lwd = 2, add = TRUE)
# (difference between two models looks very small)
# There is some difference to the empirical estimate of the Pickands function, 
# but due to the small sample size the uncertainty may be high. 

# Exercises : ####
# 1) Explore the serial extremal dependence in precipitation (PRCP).
# 2) Study the bivariate extreme value behavior between precipitation (PRCP) and "minus TMAX" during the months of June, July and August. For this, you can define two variables to work with:
var1 = data_df$PRCP[data_df$months %in% 6:8]
var2 = -data_df$TMAX[data_df$months %in% 6:8]

