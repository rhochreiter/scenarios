nst.p_inventory <- function(tree.structure, mu0, sigma0, b) {
  
  ### preprocessing
  stages <- length(tree.structure)
  stages0 <- stages + 1
  
  nodes.in.stage <- cumprod(tree.structure)
  nodes.upto.stage <- cumsum(nodes.in.stage)
  
  nodes0.in.stage <- c(1, nodes.in.stage)
  nodes0.upto.stage <- cumsum(nodes0.in.stage)
  
  nodes <- sum(nodes.in.stage)
  nodes0 <- nodes + 1
  node.index <- c(0:nodes)
  
  ### stages
  tree.stages <- c(0)
  current.stage <- 1
  for (current.nodes in nodes.in.stage) {
    tree.stages <- c(tree.stages, rep(current.stage, current.nodes))
    current.stage <- current.stage + 1
  }
  
  ### predecessor
  tree.pred <- c(-1) # stage 0
  tree.pred <- c(tree.pred, rep(0, tree.structure[1])) # stage 1  
  # stage 2
  for (current.node in c(1:nodes.in.stage[1])) { tree.pred <- c(tree.pred, rep(current.node, tree.structure[2])) }
  # stage > 2
  for (current.stage in c(3:stages)) {
    for (current.node in c(1:nodes.in.stage[current.stage-1])) {
      tree.pred <- c(tree.pred, nodes0.in.stage[current.stage-1] + rep(current.node, tree.structure[current.stage]))          
    }
  }
  
  ### probabilities
  tree.prob <- c(1)
  current.stage <- 1
  for (current.nodes in nodes.in.stage) {
    tree.prob <- c(tree.prob, rep(1/tree.structure[current.stage], current.nodes))
    current.stage <- current.stage + 1
  }
  
  tree.stageprob <- c(1)
  current.stage <- 1
  for (current.nodes in nodes.in.stage) {
    tree.stageprob <- c(tree.stageprob, rep(1/current.nodes, current.nodes))
    current.stage <- current.stage + 1
  }
  
  ### values
  mu <- mu0 * (1 - b)
  sigma <- sqrt((sigma0^2)*(1 - b^2))
  
  # stage 0 
  tree.value <- c(mu0)
  eps <- c(0)
  
  # stage 1
  tree.value <- c(tree.value, rnorm(tree.structure[1], mu0, sigma0))  
  eps <- c(eps, rep(0, tree.structure[1]))
  
  tree.value <- c(tree.value, rep(0, nodes0-length(tree.value)))
  
  # stage > 1
  for (current.stage in c(2:stages)) {
    eps <- c(eps, rnorm(nodes.in.stage[current.stage], mu, sigma))
    for (current.node in c((nodes0.upto.stage[current.stage]+1):nodes0.upto.stage[current.stage+1])) {
      tree.value[current.node] <- b * tree.value[tree.pred[current.node] + 1] + eps[current.node]
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
