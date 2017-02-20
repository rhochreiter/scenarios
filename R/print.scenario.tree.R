print.scenario.tree <- function(scenario.tree) {
  cat(paste("Scenario Tree with ", scenario.tree$stages, 
            " stages and ", scenario.tree$nodes, 
            " nodes (including root node).", sep=""))
}
