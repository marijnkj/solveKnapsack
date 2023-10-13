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
