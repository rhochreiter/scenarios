sm.f <- function(simulation, tree_structure) {
  
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
  tree.p_stage <- c(1)
  tree.p_node <- c(1)
  
  # predefine vector of stages of all nodes in the tree 
  current_stage <- 1
  for(nas in nodes_at_stage[2:length(nodes_at_stage)]) {
    tree.stages <- c(tree.stages, rep(current_stage, nas)) 
    current_stage <-   current_stage + 1
  }

  # predefine vector of predecessors of all nodes in the tree
  preds <- c()
  for(current_node in 1:nodes_without_terminal) {
    node_stage <- tree.stages[current_node]
    preds <- c(preds, rep(current_node-1, tree_structure_complete[node_stage+2]))
  }
  tree.pred <- c(tree.pred, preds)
  
  # tree generation
  positions <- list(c(1:n_simulations))
  for(current_node in 1:nodes_without_terminal) {
    node_stage <- tree.stages[current_node]
    
    current_positions <- positions[[current_node]]
    data <- as.vector(m[current_positions, node_stage+1])
    reduction <- tree_structure_complete[node_stage+2]

    approximation <- node.approx(data, reduction, fixed=TRUE)
    tree.value <- c(tree.value, approximation$value)
    for(p in approximation$position) {
      original.positions <- current_positions[p]
      positions[[length(positions)+1]] <- original.positions
    }
    tree.p_node <- c(tree.p_node, approximation$prob)
  }

  # return scenario tree 
  return(scenario.tree(tree.value, tree.pred, tree.stages, p.node=tree.p_node))
}
