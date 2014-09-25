library(scenarios)

data(sp500)

series <- sp500["2009/2012"]
returns <- dailyReturn(series)
returns <- as.vector(returns)

b <- block.bootstrap(returns, 30, 20)

plot(b)
plot(return2price(b, as.numeric(Ad(series[1]))))
