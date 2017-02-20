block.bootstrap <- function(input, n_timesteps=1, n_scenarios=100) {
  
  # choose position of resampled period
  resampling_positions <- round(runif(n_scenarios, min=1, 
                                      max=length(input)-n_timesteps))
  
  # create scenario set
  for(i in 1:n_scenarios) {
    sims <- input[resampling_positions[i]:(resampling_positions[i]+n_timesteps-1)]
    if (i==1) { simulations <- sims } else {
      simulations <- rbind(simulations, sims)
    }
  }
  row.names(simulations) <- NULL
  
  # return scenario simulation object  
  return(scenario.simulation(input, simulations))
}
