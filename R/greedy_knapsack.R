greedy_knapsack <- function(x, W) {
  if (!is.data.frame(x) | !all(c("w", "v") %in% colnames(x)) | length(x[x < 0]) != 0 | !is.numeric(W) | length(W) != 1) {
    stop("Check your variables! x must be a data.frame with columns w and v and all positive values, and W must be a scalar value.")
  }
  
  # Sort in decreasing order of value per unit of weight
  x <- dplyr::mutate(x, "vw"=x$v / x$w)
  x <- x[order(x$vw, decreasing=TRUE),]
  
  elements1 <- vector()
  weight <- 0
  
  i <- 1
  elements_p1 <- vector()
  repeat {
    elements_p1 <- append(elements1, rownames(x[i,]))
    weight <- sum(x[elements_p1, "w"])
    i <- i + 1
    
    if (weight > W) break
    else elements1 <- elements_p1
  }
  
  elements2 <- rownames(x[length(elements1) + 1,])
  value1 <- sum(x[elements1, "v"])
  value2 <- sum(x[elements2, "v"])
  
  if (value1 > value2) return(list("value"=value1, "elements"=elements1))
  else return(list("value"=value2, "elements"=elements2))
}
