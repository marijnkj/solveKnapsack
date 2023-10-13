dynamic_knapsack <- function(x, W) {
  if (!is.data.frame(x) | !all(c("w", "v") %in% colnames(x)) | length(x[x < 0]) != 0 | !is.numeric(W) | length(W) != 1) {
    stop("Check your variables! x must be a data.frame with columns w and v and all positive values, and W must be a scalar value.")
  }
  
  ### Define functions ###
  get_value_mat <- function (mat, x, i, w) {
    # R doesn't do zero-indexing. Using definitions of using first i items, and weight limit w. 
    # Matrix indexes are adjusted for this.
    if (i == 0 | w <= 0) { # 0 items
      mat[i + 1, w + 1] <- 0
      return(mat)
    }
    
    if (mat[i, w + 1] == -1) { # m[i - 1, w] has not been calculated, must first do this
      mat <- m(mat, x, i - 1, w)
    }
    
    if (x[i, "w"] > w) { # item doesn't fit in the bag
      mat[i + 1, w + 1] <- mat[i, w + 1]
    }
    
    else {
      if (mat[i, w + 1 - x[i, "w"]] == -1) { # m[i - 1, w - x[i, "w"]] has not been calculated, must first do this
        mat <- m(mat, x, i - 1, w - x[i, "w"])
      }
      
      mat[i + 1, w + 1] <- max(mat[i, w + 1], mat[i, w + 1 - x[i, "w"]] + x[i, "v"])
    }
    
    return(mat)
  }
  
  get_elements_knapsack <- function(mat, x, v_knapsack, i, w) {
    if (i == 0) {
      return(v_knapsack)
    }
    if (max(m(mat, x, i, w)) > max(m(mat, x, i - 1, w))) {
      v_knapsack <- append(i, knapsack(mat, x, v_knapsack, i - 1, w - x[i, "w"]))
      return(v_knapsack)
    }
    else {
      return(knapsack(mat, x, v_knapsack, i - 1, w))
    }
  }
  
  ### Get values ###
  mat <- matrix(-1, nrow=nrow(x) + 1, ncol=w + 1) # Initialize nW matrix with -1 values
  max_value <- max(get_value_mat(mat, x, nrow(x), W))
  elements <- get_elements_knapsack(mat, x, vector(), nrow(x), W)
  return(list("value"=max_value, "elements"=elements))
}

### NON RECURSIVE ###
# dynamic_knapsack <- function(x, W) 
#   if (!is.data.frame(x) | !all(c("w", "v") %in% colnames(x)) | length(x[x < 0]) != 0 | !is.numeric(W) | length(W) != 1) {
#     stop("Check your variables! x must be a data.frame with columns w and v and all positive values, and W must be a scalar value.")
#   }
#   
#   # R doesn't do zero-indexing
#   mat <- matrix(nrow=nrow(x) + 1, ncol=W + 1)
#   for (i in 1:(nrow(x) + 1)) mat[i, 1] <- 0
#   for (j in 1:(W + 1)) mat[1, j] <- 0
#   
#   for (i in 2:(nrow(x) + 1)) {
#     for (j in 2:(W + 1)) {
#       if (x[i - 1, "w"] > (j - 1)) {
#         mat[i, j] <- mat[i - 1, j]
#       }
#       else {
#         mat[i, j] <- max(mat[i - 1, j], mat[i - 1, j - x[i - 1, "w"]] + x[i - 1, "v"])
#       }
#     }
#   }
#   
#   return(mat)
# }