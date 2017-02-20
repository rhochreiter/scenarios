scenario.simulation <- function(original, simulations) {
  simulation <- list()  
  simulation$original <- original
  simulation$simulation <- simulations
  simulation$n <- dim(simulations)[1]
  class(simulation) <- "scenario.simulation"
  return(simulation)
}
