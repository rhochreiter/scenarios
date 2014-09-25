plot.scenario.simulation <- function(simulation, ...) {
  # get simulated values (including root)
  sim_values <- get.matrix(simulation, root=TRUE)
  stages <- c(1:dim(sim_values)[2])
  
  # calculate total minima and maxima
  total_min <- min(min(simulation$original), min(sim_values))
  total_max <- max(max(simulation$original), max(sim_values))
  
  # plot original and simulated values
  plot_which <- c(2:simulation$n) # sample, if n is too large
  
  op <- par(mfrow=c(1, 2))
  plot(simulation$original, type="l", ylim=c(total_min, total_max), ylab="Historical Values")  
  plot(sim_values[1,], col="red", type="l", ylim=c(min(total_min), max(total_max)), ylab="Simulated Values",xaxt="n")
  axis(1, stages, stages-1)
  for(i in plot_which) { lines(sim_values[i, ], col="red") }
  par(op) 
}
