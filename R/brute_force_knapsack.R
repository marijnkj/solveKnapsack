#' Find a solution to the knapsack problem with the brute force algorithm
#' 
#' @name brute_force_knapsack
#' @docType methods
#' 
#' @param x A data.frame containing rows for items to be packed with columns 'w' for weight and 'v' for value
#' @param W The maximum weight of the knapsack
#' 
#' @returns A list containing the maximum value of the knapsack and a vector of elements contained in the knapsack
#' @export

brute_force_knapsack <- function(x, W) {
  if (!is.data.frame(x) | !all(c("w", "v") %in% colnames(x)) | length(x[x < 0]) != 0 | !is.numeric(W) | W < 0 | length(W) != 1) {
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
  
  return(list("value"=max_value, "elements"=as.numeric(elements)))
}
