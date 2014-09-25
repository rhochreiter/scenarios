library(scenarios)

data(sp500)

series <- sp500["2009/2012"]
returns <- as.vector(dailyReturn(series))

sim <- block.bootstrap(returns, 40, 200)

plot(sim)
plot(return2price(sim, as.numeric(Ad(series[1]))))

input <- pick.stage(sim, c(20,40))
plot(input)

tree1 <- fl.b(input, c(50,100))
plot(tree1)
tree2 <- fl.b(input, c(10,100))
plot(tree2)
