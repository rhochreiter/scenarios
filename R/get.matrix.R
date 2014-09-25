get.matrix <- function(scenario, root=TRUE) {
  matrix <- scenario$simulation
  if (root) { matrix <- cbind(rep(scenario$original[length(scenario$original)], scenario$n), matrix)  }
  return(matrix)
}
