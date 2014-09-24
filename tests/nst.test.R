library(scenarios)

data(sp500)
ts <- as.numeric(dailyReturn(sp500["2007/2010"]))

tree.structure <- c(5, 3, 2)
tree1 <- nst.b(ts, tree.structure)
plot(tree1)

tree.structure <- c(10, 10, 10)
tree2 <- nst.b(ts, tree.structure)
plot(tree2)
