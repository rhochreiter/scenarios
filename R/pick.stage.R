pick.stage <- function(scenario, stages) {
  picked <- scenario
  picked$simulation <- scenario$simulation[, c(stages)]
  return(picked)
}
