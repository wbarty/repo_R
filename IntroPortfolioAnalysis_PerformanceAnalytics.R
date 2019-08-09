library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tseries)

data = xts(data('...'))
n = length(data)
rf = ...

# equal weightings
weights = rep(1/n, n)

# asset returns
assret = Return.calculate(R = data, weights = weights, rebalance_on = 'days')
# portfolio returns
portret = Return.portfolio(R = data, weights = weights, rebalance_on = 'days')

# geometric mean of returns
mean.geometric(data)

# Annualised returns
retann = Return.annualized(portret)
sdann = StdDev.annualized(portret)
spann = SharpeRatio.annualized(portret)

# charting
charts.RollingPerformance(portret)

# Semi-Deviation - non-symmetric measure of risk, useful for skewed downside risk
semdev = SemiDeviation(portret)

#############################################################################
# Modern Portfolio Theory
#############################################################################

# Optimal Weights
optp = portfolio.optim(portret) # optimise portfolio
portw = optp$pw
names(optw) = colnames(portret)
optw = portw[portw > 0.01] # select weights that aren't too small
optp$pm # portfolio returns and volatility

portfolio.optim(data, pm = mean(data, shorts = F, reshigh = NULL)) # for specific/target returns  portfolio.optim(data, pm = 1.1 * mean(data, shorts = F, reshigh = NULL)) for 1-% above the mean data

#############################################################################
# Efficient Frontier
#############################################################################

stockmu = colMeans(portret)
grid = seq(0.01, max(stockmu), length_out = 50)
upm = upsd = rep(NA, length(grid)) # grid of target values
mweights = matrix(NA, 50, 30) # empty matrix to store weights

for(i in 1:length(grid)){
    opt = portfolio.optim(x = portret, pm = grid)
    vpm = opt$pm
    vpsd = opt$ps
    mweights[i, ] = opt$pw
}

# Minimum Variance Portfolio
weights_minvar = mweights[upsd == min(upsd)]

# Sharpe Ratio
vsr = (vpm-rf)/upsd
# Tangency Portfolio
weights_max_sr = mweights[vsr == max(vsr)]
