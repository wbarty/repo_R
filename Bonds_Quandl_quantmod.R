library(Quandl)
library(quantmod)

p = ...
r = ...
ttm = ...
y = ...

# PV bond valuation
bondval = function(p, r, ttm, y){
    cf = c(rep(p*r, ttm-1), p*(1+r))
    cf = data.frame(cf)
    cf$t = as.numeric(rownames(cf))
    cf$pv_factor = 1/(1+y)^cf$t
    cf$pv = cf$cf * cf$pv_factor
    sum(cf$pv)
}

# Cash flows
cf = c( , , , ,)
bval = function(i, cf, t = seq(along = cf)){
    sum(cf/(1+i)^t)
}
# YTM
ytm = function(cf){
    unitroot(bval, c(0, 1), cf = cf)$root
}

# Duration
px = bond_price
pxu = bond_price with 1% yield increase
pxd = bond_price with 1% yield decrease
yd = change in yield

duration = (pxd - pxu)/(2*p*yd)

# Convexity
C = (pxd + pxu - 2 * p)/(p * (yd)^2)
price_change_from_C = 0.5*C*(yd)^2

# duration dollar change + convexity dollar change = pxz | pxz + p = expected bond price
# an increase in yield causes a price decrease



