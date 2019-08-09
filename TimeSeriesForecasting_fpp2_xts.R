library(fpp2) # ggplot package

mydata = ...
startdate = ["YYYY-MM-DD"]
enddate = ["YYYY-MM-DD"]
frequency = 4 # quaterly

# Time series objects
x = ts(mydata, start = startdate, end = enddate, frequency = frequency)

#############################################################################
# Plotting timeseries objects
#############################################################################

autoplot(x, series = "mydata")
ggseasonplot(x, polar = T) # seasonal data plotted against its seasons
ggsubseriesplot(x) # multiple seasonal data plot
ggACF(x) # plots time series' autocorrelation functions
gglagplot(x) # plot against a lagged observation series
# add to plot to comapare
+ autolayer(fitted(forecast))

#############################################################################
# Naive Forecasts
#############################################################################

# uses the previous observation, thus it will be lagged 1 period
xf = naive(x, h=10)
# snaive uses a median style approach to forecasting
xfs = snaive(x, h=2*frequency)

####################
# Residuals
####################

# plotting residuals
autoplot(residual(forecast))

# check residuals
print(checkresiduals(forecast)) # p-vale should be > 0.05

# residual pipe funaction
forecast %>% naive() %>% checkresiduals()

# setting training and testing time sets
training = window(mydata, end = t)
training = subset(mydata, start = startdate, end = ...)
test = window(mydata, start = t+1)
test = subsert(mydata, start = t+1, end = enddate)

####################
# Forecast accuracy
####################

# MAE -
# MSE -
# MAPE - best for comparisons, isnt effective for 0's or small values
# MASE - best case of scaled MAE to compare

accuracy(forecast, test) # tests accuracy on test data set

# forecasts based on mean of all observations
meanfc = meanf(x, ...)

#############################################################################
# Cross-Validation
#############################################################################

#-----x
#----------                 --- training data
#---------------x             x test data
#---------------------x

# gives the MSE of the forecast, lower MSE is better
cv = tsCV(training, forecastfunction = naive, h=1)
mean(cv^2, na.rm = F) # for data sets in matrix form, use colMeans() to evaluate

#############################################################################
# Exponentially weighted forecasts
#############################################################################

# Simple exponential smoothing (SES)
# alpha - weighting, higher alpha gives more emphasis on newer observations and decay faster

sesfc = ses(training, h=10)

####################
# Exponential smoothing with trend
####################

esfc = holt(mydata, damped = T, h = ..., PI = F)

####################
# Exponential smoothing with trend and seasonality - Holt-Winters Method
####################

# additive
addfc = hw(training, seasonal = "additive")
#multiplicative
multfc = hw(training, seasonal = "multiplicative")

#############################################################################
# ETS forecasts
#############################################################################

# methods of stabilising variance (transformations)
    # sqrt(x)
    # cuberoot(x)
    # log(x)
    # inverse(x) = -1/x

etsfc = ets(training)

#############################################################################
# Box Cox for ARIMA models
#############################################################################

lambda = BoxCox.lambda(training)
# gives lambda(x)

training %>% est(lambda = lambda) %>% forecast(h=10) %>% autoplot()

#############################################################################
# ARIMA models
#############################################################################

autoarimafc = auto.arima(training) # auto selects (p,d,q) values
summary(autoarimafc) # tune model to have lowest AIC value

training %>% Arima(order = c(0,0,0), include.constant = T, stepwise = F) %>% forecast() %>% autoplot                        # includes drift function

training %>% arimafunc(h=10) %>% autoplot()

####################
# Seasonal ARIMA models
####################

# P, D and Q are seasonally lagged versions of p, d and q, with m observations
seasarimafc = auto.arima(training, lambda = 0) # allows seasonality to change over time
# with d = 2 (i.e. 2 differences), drift is naturally included and a trend will be present

#############################################################################
# Dynamic Regression - the error term being an ARIMA model
#############################################################################

dynreg = auto.arima(mydata, xreg = mydata["predictor_var"])

dynregfc = forecast(dynreg, xreg = rep(prediction, repetition_times )) #

####################
# Dynamic Harmonic Regression
####################
# in auto.arima, set seasonal = F and lambda = 0 and xreg = fourier(y, K=K)
# Fourier terms need to be calcuated separately from the model
harmonics = fourier(mydata, K=K)
xreg = harmonics
harmfc = forecast(mydata, xreg = xreg)
# increase K until the models AIC stops decreasing

#############################################################################
# TBATS models
#############################################################################
# Trigonometric terms for seasonality (harmonics), Box Cox, ARMA, Trend, Seasonal

tbatfc = tbats(training)
forecast(tbatfc)

# output analysis
# Forecast: TBATS(1, {0,0}, -, {<52.18, 14>})
# (Box Cox paramter - 1 means no transformation was required,
# {ARMA error - p,q},
# Damping parameter for trend '-' means NULL is the d from ARIMA,
# {<seasonal term (52.18 weeks per year), 14 fourier-like terms selected>} Fourier terms
#)
