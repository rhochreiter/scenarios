scenario.tree <- function(values, predecessors, stages,
                          p.node=NA, p.stage=NA) {

  scenario.tree <- list()
  scenario.tree$value <- values
  scenario.tree$predecessor <- predecessors
  scenario.tree$stage <- stages
  scenario.tree$p.node <- p.node
  scenario.tree$p.stage <- p.stage
  scenario.tree$nodes <- length(values)
  scenario.tree$stages <- max(stages) + 1
  class(scenario.tree) <- "scenario.tree"

  return(scenario.tree)
}