library(scenarios)

data(sp500)

series <- sp500["2009/2012"]
returns <- dailyReturn(series)

b <- block.bootstrap(as.vector(returns), 30, 20)

plot(b)
plot(return2price(b, as.numeric(Ad(series[1]))))
