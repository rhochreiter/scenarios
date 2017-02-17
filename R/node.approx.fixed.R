node.approx.fixed <- function(data, reduction) {
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
  } else {
    orig.value <- as.numeric(as.matrix(data))
    new.value <- orig.value
    while(length(new.value) < reduction) {
      pos <- sample(length(new.value), 1)
      new.value <- c(new.value, new.value[pos])
    }
    output$value <- new.value

    position <- list()
    for(i in 1:reduction) { position[[i]] <- i }
    output$position <- position
  }
  
  return(output)
}
