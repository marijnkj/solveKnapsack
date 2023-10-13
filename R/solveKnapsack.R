library(stats)
library(dplyr)

RNGversion(min(as.character(getRversion()),"3.5.3"))

##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

brute_force_knapsack <- function(x, W) {
  if (!is.data.frame(x) | !all(c("w", "v") %in% colnames(x)) | length(x[x < 0]) != 0 | !is.numeric(W) | length(W) != 1) {
    stop("Check your variables! x must be a data.frame with columns w and v and all positive values, and W must be a scalar value.")
  }
  
  max_value <- 0
  elements <- vector()
  
  for (i in (1:2^nrow(x) - 1)) {
    combination <- intToBits(i)
    x_comb <- x[combination == 1,]
    value <- sum(x_comb$v)
    weight <- sum(x_comb$w)
    
    if (weight <= W & value > max_value) {
      max_value <- value
      elements <- as.numeric(rownames(x_comb))
    }
  }
  
  return(list(value=max_value, elements=elements))
}

dynamic_knapsack <- function(x, W) {
  if (!is.data.frame(x) | !all(c("w", "v") %in% colnames(x)) | length(x[x < 0]) != 0 | !is.numeric(W) | length(W) != 1) {
    stop("Check your variables! x must be a data.frame with columns w and v and all positive values, and W must be a scalar value.")
  }
  
  mat <- matrix(0, nrow=nrow(x), ncol=W)
  for (i in 2:nrow(x)) {
    for (j in 2:W) {
      if (x[i, "w"] > j) {
        mat[i, j] <- mat[i - 1, j]
      }
      else {
        mat[i, j] <- max(mat[i - 1, j], mat[i - 1, j - x[i, "w"]] + x[i, "v"])
      }
    }
  }
  
  return(mat)
}

greedy_knapsack <- function(x, W) {
  if (!is.data.frame(x) | !all(c("w", "v") %in% colnames(x)) | length(x[x < 0]) != 0 | !is.numeric(W) | length(W) != 1) {
    stop("Check your variables! x must be a data.frame with columns w and v and all positive values, and W must be a scalar value.")
  }
  
  # Sort in decreasing order of value per unit of weight
  x <- mutate(x, "vw"=x$v / x$w)
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
