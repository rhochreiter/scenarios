### Pretty preliminary version

single.stage.reduction <- function(scenario.set, 
                                   reduced.size=0,
                                   probability=NA, 
                                   algorithm="partition",
                                   distance="fortet-mourier-2", 
                                   parameter=NULL) {
  ########## Parameters 
  
  n_scenarios <- nrow(scenario.set)

  if(is.na(probability[1])) { probability <- rep(1, n_scenarios)/n_scenarios }
  
  if(reduced.size <= 0) { reduced.size <- round(0.1 * n_scenarios) } # Default: Reduce to 10%
  if(reduced.size >= n_scenarios) { 
    return(list(status=TRUE, reduction.data=scenario.set, reduction.prob=probability)) 
  }
    
  if(n_scenarios != length(probability)) { }
  
  ########## Scenario Reduction

  status <- FALSE
  scenarios.in <- scenario.set
  scenarios.out <- NULL
  probability.out <- NULL
  
  ##### Algorithm - Kmeans
  
  if(algorithm=="kmeans") { 
    fit <- kmeans(scenarios.in, reduced.size)
    scenarios.out <- fit$centers
    probability.out <- rep(0, reduced.size)
    for(i in 1:reduced.size) { probability.out[i] <- sum(probability[which(fit$cluster == i)]) }
    status <- TRUE
  }

  ##### Algorithm - Partition
  
  if(algorithm=="partition") { 
    metric <- "euclidean"
    if(distance == "wasserstein") { metric <- "manhattan" }
    fit <- clara(scenarios.in, reduced.size, metric = metric)
    scenarios.out <- fit$medoids
    probability.out <- rep(0, reduced.size)
    for(i in 1:reduced.size) { probability.out[i] <- sum(probability[which(fit$clustering == i)]) }
    status <- TRUE
  }
  
  ##### Algorithm - Hierarchical Join
  
  if(algorithm=="hierachical.join") { 
    metric <- "euclidean"
    if(distance == "wasserstein") { metric <- "manhattan" }
    fit <- agnes(scenarios.in, metric = metric)
    clustering <- cutree(fit, k = reduced.size)
    probability.out <- rep(0, reduced.size)
    scenarios.out <- as.numeric(kmeans(scenarios.in[which(clustering==1), ], 1)$centers)
    for(i in 1:reduced.size) { 
      probability.out[i] <- sum(probability[which(clustering == i)]) 
      if(i > 1) {
        scenarios.out <- rbind(scenarios.out, as.numeric(kmeans(scenarios.in[which(clustering==i), ], 1)$centers))
      }
    }
    status <- TRUE
  }
  
  ########## Return Result
  
  rownames(scenarios.out) <- c(1:reduced.size)
  colnames(scenarios.out) <- c(1:ncol(scenarios.out))
  
  return(list(status=status, reduction.data=scenarios.out, reduction.prob=probability.out))
}