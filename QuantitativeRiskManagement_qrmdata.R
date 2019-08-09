library(qrmdata)
library(xts)

mydata = "..."


data(mydata)

# Log Returns
logdata = diff(log(mydata))

# Plotting multiple time series
plot.zoo(mydata, plot.type = "single", col = 1:4, type = "h")

# Aggregating Returns
aggdata = apply.weekly(logdata, sum) # use colSums for multiariate series

# Scatter plot of object variables
pairs(as.zoo(logdata))

# Histograms
hist(logdata, nclass = 20, probability = T)

# Lines
mu = 0
sigma = 1
lines(logdata, dnorm(logdata, mean = mu, sd = sigma), col = 'red')

# Density KDE
plot(density(logdata))

#############################################################################
# Normality
#############################################################################

# QQ Plot
# generate random normal data
ndata = rnorm(1000, mean = mu, sd = sigma)
qqnorm(ndata)
qqline(ndata)

# t-variables generator
tdata = rt(1000, df = 4)

# generate uniform variables
unidata = runif(1000)

# JB Test
jarque.test(logdata)

# Moving Returns
rolldata = rollapplyr(logdata, width = 5, FUN = sum)[-(*, *)]
# overlaps data, creates strog correelations, moving returns, e.g. 5-day rolling average

#############################################################################
# t-distribution
#############################################################################

# nu = v = degrees of freedom
tdist = fit.st(logdata)
params = logdata$par.ests # the fitted models parameter estimates (v, mu, sigma)
nu = params[1]
mu = params[2]
sigma = params[3]

# plot distribution
yvals = dt((logdata-mu)/sigma, df = nu)/sigma
lines(logdata, yvals, col = 'red')

#############################################################################
# Time-Series Volatility
#############################################################################

####################
# Clustering
####################

n = length(logdata)
normdata = rnorm(n) * nparams[2] + nparams[1] # create a normal sample of size n
tdata = rt(n, df = params[1]) * params[3] + params[2]
xtsdata = xts(logdata)
mergedata1 = merge(tdata, xtsdata)
plot.zoo(mergedata1, type = 'h', ylim = range(mergedata1))

####################
# Serial Correlation - stationarity assumed
####################

k =  # lagsize, degrees of freedom
# acfs = p_hat(k)
acf(logdata)
acf(abs(logdata)) # absolute value of acf's
acf(logdata^2) # squared acf's

# Ljung-Box Test
# carry out test on abs() returns
Box.test(logdata, lag = 10, type = "Ljung")
Box.test(abs(logdata), lag = 10, type = "Ljung")
Box.test(aggdata, lag = 10, type = "Ljung") # change data periodicity

apply(logdata, 2, Box.test, lag = 10, test = 'Ljung') # multivariate time-series

#############################################################################
# VaR and ES
#############################################################################

# exact normmal distribution VaR and ES
qnorm(logdata)
ESnorm(logdata)
# specified variables
p = 0.99
qnorm(p, mean = 0, sd = 1)
ESnorm(p, mu = 0, sd = 1)
xvals = seq(from -4*sigma, to 4*sigma, length.out = 100) # generate 100 random values
u = dnorm(xvals, mean = mu, sd = sigma)
plot(xvals, ndens, type = 'L')
VaRp = qnorm(p, mean = mu, sd = sigma)
ESp = ESnorm(p, mean= mu, sd = sigma)
abline(u = VaRp, col = 'red')
abline(u = ESp, col = 'blue')
riskfactors = merge(..., ..., ..., all = F)[/"time_passed"]
plot.zoo(riskfactors)

# for loss data from simulation
q = p = 0.95
VaRq = quantile(lossdata, q)
ESq = mean(lossdata(lossdat > quantile(lossdata, q)))

#############################################################################
# Black-Scholes-Merton Model
#############################################################################

t =
S =
r =
sigma =
K =
T =
EuroCall = Black_Scholes(t, S, r, sigma, K, T, "call")

# implied volatility data
# VIX index data is avaliable in QRMDATA
