add.tree.matrix <- function(scenario.tree) {
  stages <- scenario.tree$stage
  terminal.stage <- max(stages)
  
  ancestor <- scenario.tree$predecessor + 1;
  
  terminal_node_id <- which(scenario.tree$stage == terminal.stage)
  terminal_nodes <- length(terminal_node_id)
  
  tree.index <- matrix(rep(0, terminal_nodes * terminal.stage), ncol=terminal.stage)
  
  for (current.node in 1:terminal_nodes) {
    current.node.id <- terminal_node_id[current.node]
    for (current.stage in seq(terminal.stage, 1, by=-1)) {
      tree.index[current.node, current.stage] <- current.node.id
      current.node.id <- ancestor[current.node.id]
    }
  }
  
  tree.value <- matrix(scenario.tree$value[tree.index], ncol=terminal.stage)
  
  # add root node
  tree.index <- cbind(rep(1, dim(tree.index)[1]), tree.index)
  tree.value <- cbind(rep(scenario.tree$value[1], dim(tree.index)[1]), tree.value)
  
  # add
  scenario.tree$matrix.index <- tree.index
  scenario.tree$matrix.value <- tree.value
  return(scenario.tree)
}