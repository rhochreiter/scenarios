block.bootstrap <- function(series, simulations, n) {
  # choose position of resampled period
  resampling_positions <- round(runif(n, min=1, max=length(returns)-simulations))
  
  # create scenario set
  for(i in 1:n) {
    sims <- series[resampling_positions[i]:(resampling_positions[i]+simulations)]
    if (i==1) { scenario_set <- sims } else {
      scenario_set <- rbind(scenario_set, sims)
    }
  }
  
  # create simulation object
  simulation <- list()  
  simulation$original <- series
  simulation$simulation <- scenario_set
  simulation$n <- n
  class(simulation) <- "scenario.simulation"
  
  # return scenario set  
  return(simulation)
}
