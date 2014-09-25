return2price <- function(simulation, start_price=1) {
  simulation$original <- start_price * (cumprod(1+simulation$original))
  end_original <- simulation$original[length(simulation$original)]
  for (s in 1:simulation$n) {
    simulation$simulation[s,] <- end_original * (cumprod(1+simulation$simulation[s,]))
  }
  return(simulation)
}
