# 0. Setup and Data

library(scenarios)

# 1. NST/P

tree1 <- nst.p_inventory(c(5, 3, 2), 4, 0.6, 0.8)
plot(tree1)
tree2 <- nst.p_inventory(c(10, 5, 5), 4, 0.6, 0.8)
plot(tree2)

# 2. NST/B

data(sp500)
ts <- as.numeric(dailyReturn(sp500["2007/2010"]))

tree.structure <- c(5, 3, 2)
tree1 <- nst.b(ts, tree.structure)
plot(tree1)

tree.structure <- c(10, 10, 10)
tree2 <- nst.b(ts, tree.structure)
plot(tree2)

# 3. Facility Location

data(sp500)
series <- sp500["2009/2012"]
history <- as.vector(dailyReturn(series))

future <- block.bootstrap(history, 40, 200)

plot(future)
plot(return2price(future, as.numeric(Ad(series[1]))))

input <- pick.stage(future, c(20,40))
plot(input)

tree1 <- fl.b(input, c(50,100))
plot(tree1)
tree2 <- fl.b(input, c(10,100))
plot(tree2)

# 4. Forward Merging

data(sp500)
series <- sp500["2009/2012"]
returns <- as.vector(dailyReturn(series))

sim <- block.bootstrap(returns, 40, 200)

input <- pick.stage(sim, c(20,40))
plot(input)

tree1 <- sm.f(input, c(10,5))
plot(tree1)

input <- pick.stage(sim, c(20,30,40))
plot(input)

tree2 <- sm.f(input, c(5,3,2))
plot(tree2)

### Comparison between SM-F and FL-B

# Visual comparison

input <- pick.stage(sim, c(20,40))
tree.fl <- fl.b(input, c(10, 50))
tree.sm <- sm.f(input, c(10, 5))
plot(tree.fl)
plot(tree.sm)

# Comparison of first stage probs

prob.fl <- tree.fl$p.stage[2:11]
value.fl <- tree.fl$value[2:11]

prob.sm <- tree.sm$p.node[2:11]
value.sm <- tree.sm$value[2:11]

plot(value.fl, prob.fl, 
     xlim=c(min(value.fl, value.sm), max(value.fl, value.sm)),
     ylim=c(min(prob.fl, prob.sm), max(prob.fl, prob.sm)))
points(value.sm, prob.sm, col="red")

sum(value.fl * prob.fl)
sum(value.sm * prob.sm)

### time-series instead of block bootstrapping

data(sp500)
series <- sp500["2009/2012"]
returns <- as.vector(dailyReturn(series))

future.bb <- block.bootstrap(returns, 2, 1000)

library(rugarch)
spec.garch <- ugarchspec()
fit.garch <- ugarchfit(spec.garch, returns, solver = 'hybrid')
sim.garch <- ugarchsim(fit.garch, n.sim = 2, m.sim = 1000, rseed = 1:25)
sim.garch.values <- t(sim.garch@simulation$seriesSim)

future.garch <- scenario.simulation(history, sim.garch.values)

plot(future.garch)
plot(future.bb)

tree.fl.ts <- fl.b(future.garch, c(10, 50))
tree.sm.ts <- sm.f(future.garch, c(10, 5))
tree.fl.bb <- fl.b(future.bb, c(10, 50))
tree.sm.bb <- sm.f(future.bb, c(10, 5))

par(mfrow=c(2,2))
plot(tree.fl.ts)
plot(tree.sm.ts)
plot(tree.fl.bb)
plot(tree.sm.bb)
par()

