scm.f <- function(simulation, tree_structure) {
  
  ### preprocessing
  m <- get.matrix(simulation, root=FALSE)
  n_simulations <- dim(m)[1]
  stages_to_generate <- dim(m)[2]
  
  tree_structure_complete <- c(1, tree_structure)
  nodes_at_stage <- c(1, cumprod(tree_structure))
  nodes_without_terminal <- sum(nodes_at_stage[1:(length(nodes_at_stage)-1)])
  
  # root node
  tree.value <- c(simulation$original[length(simulation$original)])
  tree.pred <- c(-1)
  tree.stages <- c(0)
  tree.stageprob <- c(1)
  tree.prob <- c(1)
  
  # tree stages
  current_stage <- 1
  for(nas in nodes_at_stage[2:length(nodes_at_stage)]) {
    tree.stages <- c(tree.stages, rep(current_stage, nas)) 
    current_stage <-   current_stage + 1
  }

  # tree pred
  preds <- c()
  for(current_node in 1:nodes_without_terminal) {
    node_stage <- tree.stages[current_node]
#    for(i in 1:tree_structure_complete[node_stage+1]) {
      preds <- c(preds, rep(current_node-1, tree_structure_complete[node_stage+2]))
#    }
  }
  tree.pred <- c(tree.pred, preds)
  
  #
  positions <- list(c(1:n_simulations))
  for(current_node in 1:nodes_without_terminal) {
    node_stage <- tree.stages[current_node]
    
    current_positions <- positions[[current_node]]
    data <- as.vector(m[current_positions, node_stage+1])
    reduction <- tree_structure_complete[node_stage+2]

    approximation <- node.approx.fixed(data, reduction)
    tree.value <- c(tree.value, approximation$value)
    weights <- c()
    for(p in approximation$position) {
      weights <- c(weights, length(p))
      original.positions <- current_positions[p]
      positions[[length(positions)+1]] <- original.positions
    }
    prob <- weights/sum(weights)
    tree.prob <- c(tree.prob, prob)
  }

  ### postprocessing
  
  # build tree object
  scenario.tree <- list()
  scenario.tree$value <- tree.value
  scenario.tree$predecessor <- tree.pred
  scenario.tree$stage <- tree.stages
  scenario.tree$nodeprob <- tree.prob
  # scenario.tree$stageprob <- tree.stageprob
  scenario.tree$nodes <- length(tree.value)
  scenario.tree$stages <- max(tree.stages) + 1
  class(scenario.tree) <- "scenario.tree"
  
  return(scenario.tree)
}
