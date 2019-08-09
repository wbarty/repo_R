library(PortfolioAnalytics)
library(quadprog)

mydata = data(...)
mydata = mydata[, selected_columns]
mydata = diff(log(mydata))

#############################################################################
# Portfolio Specification/Extraction
#############################################################################

port_spec = portfolio.spec(colnames(mydata))

port_spec = add.constraint(portfolio = port_spec, type = 'full_investment') # weights sum to 1
port_spec = add.constraint(portfolio = port_spec, type = 'long_only') # no short sales
?add.constraint

port_spec = add.objective(portfolio = port_spec, type = 'return', name = 'mean') # maximises 'return'
port_spec = add.objective(portfolio = port_spec, type = 'risk', name = 'StdDev/ES/VaR/drawdown') # minimises 'risk'
#port_spec = add.objective(R = mydata, portfolio = port_spec, momentFUN = 'custom_func', optimize_method = 'random', rp=rp) SEE BELOW FOR APPLICATION
?add.objective
# Moments - mu, sigma, m3, m4

optp = optimize.portfolio(mydata, portfolio = port_spec, optimize_method = 'random', trace = T)
optp = optimize.portfolio(mydata, portfolio = port_spec, optimize_method = 'ROI', trace = T) # ROI is a closed-form solver
optp = optimize.portfolio(mydata, portfolio = port_spec, optimize_method = 'DEoptim', trace = T)
optp = optimize.portfolio(mydata, portfolio = port_spec, optimize_method = 'GenSA', trace = T)
optp = optimize.portfolio(mydata, portfolio = port_spec, optimize_method = 'pso', trace = T)

####################
# Reblancing Portfolio
####################

opt_rebal = optimize.portfolio.rebalancing(R = xts(mydata),
    portfolio = port_spec,
    optimize_method = '...',
    rebalance_on = 'days',
    momentFUN = 'set.portfolio.moments',
    trace = T,
    training_period = '60', # number of observations for initial optimisation
    rolling_window = '60', # number of observations for each rebalancing
    search_size = '10' # number of portfolios to test
     )

chart.RiskReward(optp, risk.col = 'StdDev', return.col = 'mean', chart.assets = T)

extractWeights(optp)
chart.Weights(optp)
print(optp)

#############################################################################
# Solvers
#############################################################################

# Closed-Form - more specific but is faster and accurate (quandratic programming solver, ROI)
# Global Solver - can be applied generally, takes longer and less accurate, see above - DEoptim, random, GenSA, pso

####################
# Quadratic Programming Solver (Maximisation)
####################

const_matrix = cbind(1, diag(ncol(mydata)), -diag(ncol(mydata)))
const_vector = c(1, rep(0, ncol(mydata)), -rep(1, ncol(mydata)))
obj_matrix = 10 * cov(mydata)
obj_vector = colMenas(mydata)
equality_const = x # tune to data
opt = solve.QP(obj_matrix, obj_vector, const_matrix, const_vector, x)

#############################################################################
# Analyis of results
#############################################################################

returns = Return.Portfolio(mydata, weights = extractWeights(opt_rebal))
chart.PerformanceSummary(returns)

####################
# Visualisation
####################

plot()
chart.Concentration()
chart.EfficientFrontier()
chart.RiskReward()
chart.RiskBudget()
chart.Weights()
chart.PerformanceSummary()

####################
# Extracting Data
####################

extractObjectiveMeasures()
extractStats()
extractWeights()
extractAIC()
extractCokurtosis()
extractCoskewness()
extractCovariance()
extractEfficientFrontier()
print()
summary()

####################
# Annualised Sharpe Ratio
####################

asr = function(R, weights, sigma, scale, rf){
    r = Return.annualized(Return.Portfolio(R, weights), scale = scale)
    excessr = r - rf
    port_sd = sqrt(as.numeric(t(weights) %*% sigma %*% weights))*sqrt(scale)
    return(excessr/port_sd)
}

port_spec = add.objective(R = mydata, portfolio = port_spec, momentFUN = 'asr', weights = weights, arguments = list(scale = 12, rf = 0.02) optimize_method = 'random', rp=rp)

