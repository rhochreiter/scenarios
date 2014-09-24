plot.scenario.tree <- function(scenario.tree) {
  if(is.null(scenario.tree$matrix.index)) { scenario.tree <- add.tree.matrix(scenario.tree) }
  plot(c(1:scenario.tree$stages), scenario.tree$matrix.value[1,], type="l", xlab="Stage", ylab="Value", ylim=c(min(scenario.tree$matrix.value), max(scenario.tree$matrix.value)), xaxt="n")
  axis(1, 1:scenario.tree$stages, labels=0:(scenario.tree$stages-1))
  for(i in 2:dim(scenario.tree$matrix.index)[1]) { lines(c(1:scenario.tree$stages), scenario.tree$matrix.value[i,]) }
}
