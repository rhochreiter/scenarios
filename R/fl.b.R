fl.b <- function(simulation, tree_structure) {
  
  ### preprocessing
  m <- get.matrix(simulation, root=FALSE)
  stages_to_generate <- dim(m)[2]
  
  ### step one - backward clustering
  k <- list()
  k[[1]] <- kmeans(m, tree_structure[stages_to_generate])
  for(i in 2:stages_to_generate) { 
    k[[i]] <- kmeans(k[[i-1]]$center, tree_structure[stages_to_generate-i+1]) 
  }

  ### step two - forward tree buildup
  
  # root node
  tree.value <- c(simulation$original[length(simulation$original)])
  tree.pred <- c(-1)
  tree.stages <- c(0)
  tree.p_stage <- c(1)
  
  # first stage
  values <- as.vector(k[[stages_to_generate]]$center[,1])
  tree.value <- c(tree.value, values)
  tree.pred <- c(tree.pred, rep(0, length(values)))
  tree.stages <- c(tree.stages, rep(1, length(values)))
  tree.p_stage <- c(tree.p_stage, as.vector(prop.table(table(k[[stages_to_generate]]$cluster))))
  
  # from second to termina stage
  for (i in 2:stages_to_generate) {
    values <- as.vector(k[[stages_to_generate-i+1]]$center[,i])
    current <- as.vector(k[[stages_to_generate-i+1]]$cluster)
    ancestor <- as.vector(k[[stages_to_generate-i+2]]$cluster)
    tree.value <- c(tree.value, values)
    tree.pred <- c(tree.pred, ancestor)
    tree.stages <- c(tree.stages, rep(i, length(values)))
    tree.p_stage <- c(tree.p_stage, as.vector(prop.table(table(current))))
  }
  
  # return scenario tree 
  return(scenario.tree(tree.value, tree.pred, tree.stages, p.stage=tree.p_stage))
}