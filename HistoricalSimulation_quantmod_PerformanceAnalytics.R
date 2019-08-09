library(quantmod)
library(PerformanceAnalytics)

t = 5
periodcity = daily

#############################################################################
# Portfolio Returns
#############################################################################

# use getSymbols from quantmod or other to assign the tickers to this variable
objects = c("...", "...", ...)
weights = c("...", "...", ...)

# getPrices of the tickers
# prices = getPrices(objects)
prices = NULL
for (Ticker in objects)
    prices = cbind(prices, getSymbols.yahoo(Ticker, from "...", periodcity = periodcity, autoassign = F)[ ,t])

# change colummn names back to object vector
colnames(prices) = objects
prices = prices[apply(prices, 1, function(x)all(!is.na(x))),]
# calculate asset return
assetret = na.omit(ROC(prices, type = 'discrete'))
# calculate portfolio returns
portret = Return.portfolio(assetret, weights = weights)

#############################################################################
# Simualation
#############################################################################

p =
method = "historical", #"gaussian", #"modified", #"kernal"

VaR(portret, p = p, method = method)
ES(portret, p = p, method = method)
