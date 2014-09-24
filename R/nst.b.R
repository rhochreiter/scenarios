nst.b <- function(ts, tree.structure, stage.skip=0) {

  ### preprocessing  
  stages <- length(tree.structure)  
  stages0 <- stages + 1
  nodes.in.stage <- cumprod(tree.structure) 
  nodes <- sum(nodes.in.stage)
  nodes0 <- nodes + 1  
  
  ### values
  tree.value <- ts[sample(length(ts), 1)] # root stage 
  for (t in 1:stages) {
    current_ts <- ts[(1+(t-1)*stage.skip):length(ts)]
    tree.value <- c(tree.value, current_ts[sample(length(current_ts), nodes.in.stage[t])])
  }
  
  ### probabilities
  tree.prob <- c(1, unlist(sapply(1:stages, function(x) { rep(1/tree.structure[x], nodes.in.stage[x]) })))
  tree.stageprob <- c(1, unlist(sapply(1:stages, function(x) { rep(1/nodes.in.stage[x], nodes.in.stage[x]) })))
    
  ### stages
  tree.stages <- c(0, unlist(sapply(1:stages, function(x) { rep(x, nodes.in.stage[x]) })))
  
  ### predecessors
  tree.pred <- c(-1, rep(0, tree.structure[1])) # root and first stage
  current_node <- 1
  for (t in 2:stages) {
    for (n in 1:nodes.in.stage[t-1]) {
      tree.pred <- c(tree.pred, rep(current_node, tree.structure[t]))
      current_node <- current_node + 1
    }
  }
  
  ### postprocessing
  
  ### build tree object
  scenario.tree <- list()
  scenario.tree$value <- tree.value
  scenario.tree$predecessor <- tree.pred
  scenario.tree$stage <- tree.stages
  scenario.tree$nodeprob <- tree.prob
  scenario.tree$stageprob <- tree.stageprob
  scenario.tree$nodes <- nodes0
  scenario.tree$stages <- stages0
  
  class(scenario.tree) <- "scenario.tree"
  
  ### return
  return(scenario.tree)
}
