library(quantstrat)
library(quantmod) # for data collection

# Trend Trading - product with keep moving in current direction
# Reversion Trading - product will revert
data = getSymbols("data")
summary(data)
Sys.setenv(TZ = "products_TimeZone")
currency("...")

stock("data", currency = currency, multiplier = 1)

tradesize = 100
initeq = 1000 # trade size < initial equity
initdate = ["YYYY-MM-DD"]

# Initialise quantstrat
strategy.st = portfolio.st = account.st = 'strat'
rm.strat(strategy.st)
initPortf(portfolio.st, symbols = "data", initDate = initdate, currency = currency)
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = currency. initEq = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = T)

#############################################################################
# Indicators
#############################################################################

# 200-day SMA
sma = SMA(CL(data), n = 200)
plot(Cl(data))
lines(sma, col = 'red')

# Relative Strength Index (RSI50)
rsi = RSI(price = Cl(data), n = 50)

# Calling Indicators
add.indicator(strategy = strategy.st, name = 'SMA', arguments = list(x = quote(Cl(mktdata)), n = 200, label = 'SMA200'))

# Apply Indicators
test = applyIndicators(strategy = strategy.st, mktdata = OHLC(data))
head(test)
tail(test)

#############################################################################
# Signals
#############################################################################

# 4 Types:
    # sigComparison - relationship between 2 inidicators, returns 1 is relationship is T
    # sigCrossover - similar to comparison, returns 1 on first occurance
    # sigThreshold - compares range bound indicator to static quantity
    # sigFormula - can combine other signals, is more flexible

# Comparison/Crossover
# e.g. 50-day MA crossing 200-day MA
add.signal(strategy.st, name = "function", arguments = list(columns = c('SMA50', 'SMA200'), relationship = 'gt'))
# gt for greater than which for this signal is a buy strat, gte for great than or equal to

# Threshold
add.signal(threshold = 20, cross = T, relationship = 'lt') # cross = T mimics sigCrossover, F mimiccs sigComparison

# Formula
# e.g. drawdown - largest trough since last high peak
add.signal(arguments = list(formula = "IF statement"), name = 'sigFormula', cross = T)

#############################################################################
# Rules
#############################################################################

# creates the transaction given you make one based on a signal
add.rule(strategy.st, name = 'ruleSignal', arguments = list(sigCol = 'filterexit',
 sigval = T,
 orderqty = 'all', # reduces position to 0
 ordertype = 'market' 'limit' 'stop-loss',
 orderside = 'long' 'short'
 ),
replace = F, # when a signal is acted upon should all other trades that day be cancelled
prefer = 'open' 'close' 'high' 'low' # when to entr into a position
type = 'exit')
# filterexit - searches through signal columns to find strategy, sigval - tells rule what to look for in signal, type - exit for selling, entry for buying

#############################################################################
# Run Strategy
#############################################################################

applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
# Update Portfolio
updatePortf(portfolio.st)
daterange = time(getPortfolio(portfolio.st)$summary)[-1] # removes first (initialisation) day
# Update Account
updateAcct(account.st, daterange)
updateEndEq(account.st)
# Trade Stats
tstats = tradestats(Portfolios = portfolio.st)
tstats$Profit.Factor # see others

#############################################################################
# Visualisation
#############################################################################

chart.Posn(portfolio = portfolio.st, Symbol = 'data')
# includes positions, prices and drawdowns
# blue = buy position > 0
# green = P/L
# drawdown = amount of money lost since last high (want it to stay around 0)

# add indicators to chart
# need to reclaculate indicators
sma = SMA(CL(data), n = 50, name = 'sma50')
add_TA(sma50, on = 1, col = 'blue')
...
# zoom in on chart
zoom_chart('date1/date2')

#############################################################################
# Analytics
#############################################################################

:blotter:
# P/L
portPL = .blotter$portfolio.strat$summary$Net.Trading.PL
head(portPL)

# Sharpe Ratio for prices
SharpeRatio.annualized(portPL, geometric = F)
# Sharpe Ratio for Returns
accountrets = PortfReturns(account.st)
SharpeRatio.annualized(accountrets, geometric = F)
