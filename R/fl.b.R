fl.b <- function(simulation, tree_structure) {
  
  ### preprocessing
  m <- get.matrix(simulation, root=FALSE)
  stages_to_generate <- dim(m)[2]
  
  ### backward clustering
  k <- list()
  k[[1]] <- kmeans(m, tree_structure[stages_to_generate])
  for(i in 2:stages_to_generate) { k[[i]] <- kmeans(k[[i-1]]$center, tree_structure[stages_to_generate-i+1]) }
  
  ### forward tree buildup
  
  # root node
  tree.value <- c(simulation$original[length(simulation$original)])
  tree.pred <- c(-1)
  tree.stages <- c(0)
  tree.stageprob <- c(1)
  
  # first stage
  values <- as.vector(k[[stages_to_generate]]$center[,1])
  tree.value <- c(tree.value, values)
  tree.pred <- c(tree.pred, rep(0, length(values)))
  tree.stages <- c(tree.stages, rep(1, length(values)))
  tree.stageprob <- c(tree.stageprob, as.vector(prop.table(table(k[[stages_to_generate]]$cluster))))
  
  # other stages
  for (i in 2:stages_to_generate) {
    values <- as.vector(k[[stages_to_generate-i+1]]$center[,i])
    current <- as.vector(k[[stages_to_generate-i+1]]$cluster)
    ancestor <- as.vector(k[[stages_to_generate-i+2]]$cluster)
    tree.value <- c(tree.value, values)
    tree.pred <- c(tree.pred, ancestor)
    tree.stages <- c(tree.stages, rep(i, length(values)))
    tree.stageprob <- c(tree.stageprob, as.vector(prop.table(table(current))))
  }
  
  ### posteprocessing
  
  ### build tree object
  scenario.tree <- list()
  scenario.tree$value <- tree.value
  scenario.tree$predecessor <- tree.pred
  scenario.tree$stage <- tree.stages
#  scenario.tree$nodeprob <- tree.prob
  scenario.tree$stageprob <- tree.stageprob
  scenario.tree$nodes <- length(tree.value)
  scenario.tree$stages <- max(tree.stages) + 1
  class(scenario.tree) <- "scenario.tree"
  
  return(scenario.tree)  
}