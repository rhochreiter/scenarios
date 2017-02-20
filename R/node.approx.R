# Single node approximation using standard kmeans (univariate)
# return at least `reduction` scenarios
# random sample if n(data) < reduction

# value
# position
# prob

node.approx <- node.approx.kmeans <- function(data, reduction, 
                                              probability=NA,
                                              fixed=FALSE,
                                              fixing="random.sampling") {
  original_size <- length(data)
  output <- list()
  
  if(original_size > reduction) {
    result <- kmeans(data, reduction)
    
    output$value <- as.numeric(result$centers)
    position <- list()
    for(i in 1:reduction) {
      pos <- which(result$cluster == i)
      position[[i]] <- pos
    }
    output$position <- position
    output$group <- result$cluster
  } else {
    value <- as.numeric(as.matrix(data))
    if(fixed) { 
      value <- c(value, sample(value, reduction-length(value), replace=TRUE)) 
    }
    output$value <- value

    position <- list()
    group <- c()
    for(i in 1:length(value)) { 
      position[[i]] <- i
      group <- c(group, i)
    }
    output$position <- position
    output$group <- group
  }
  
  # compute probabilities
  weights <- c()
  for(p in position) {
    weights <- c(weights, length(p))
    output$prob <- weights/sum(weights)
  }

  # return output
  return(output)
}
