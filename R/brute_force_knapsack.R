#' Brute force method
#' 
#' The brute force algorithm will try all possible alternatives of items to include in the knapsack. The algorithm will select the combination of items with the highest value with the maximum possible weight (i.e. less than the Knapsack capacity).
#' 
#' @param x A \code{data.frame} of two non negative numeric columns \code{w} and \code{v}. 
#' 
#' @param W An integer, the knapsack weight limit.
#' 
#' @param parallel TRUE if parallel search. FALSE by default.
#' 
#' @return A list of two elements. The maximum value of the knapsack \code{value} and the elements in the knapsack \code{elements}.
#' 
#' @import parallel
#' 
#' @examples
#' # Example data
#' suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' x <- data.frame(
#' w = sample(1:4000, size = n, replace = TRUE),
#' v = runif(n = n, 0, 10000))
#' 
#' # Run the algorithm for the first eight values from the example data
#' brute_force_knapsack(x=x[1:8,], W=3500)
#' 
#' @references Wikipedia contributors. (2022, May 18). Knapsack problem. In Wikipedia, The Free Encyclopedia. Retrieved 06:38, October 4, 2022, from \url{https://en.wikipedia.org/w/index.php?title=Knapsack_problem&oldid=1088471265}
#' @export


brute_force_knapsack <- function(x, W, parallel=FALSE){
  
  # Check input
  if(!is.data.frame(x)|
     any(!(names(x) %in% c("w", "v")))|
     !any(is.numeric(x$w))|
     !any(is.numeric(x$v))|
     any(x<0)|
     W<0) { stop("False input")}
  
  # Inital values
  ret_value <- 0
  elements <- NA
  value <- NA
  
  # n=number of elements in data
  n <- nrow(x)
  
  # All combinations 
  all_test <- expand.grid(rep(list(0:1), n))==1
  
  if(parallel==TRUE){
    # Find number of cores on computer
    numCores <- 2
    
    # Creates cluster
    cl <- makeCluster(numCores, type = "PSOCK")
    
    # Weights
    w = x$w
    
    # Values
    v = x$v
    
    # Function to calculate weight and values
    ff <-function(y){
      value <- sum(v[y])
      weight <- sum(w[y])
      elements <- (1:n)[y]
      if(weight>W){value<-0}
      
      return(list(value=value, elements=elements))
    }
    # Apply function to all_test (all possible combinations)
    ret_list <- parApply(cl, all_test, 1, ff)
    
    # Turn of cluster
    stopCluster(cl)
    
    # Returns the best value found
    return(ret_list[[which.max(unlist(ret_list)[names(unlist(ret_list))=="value"])]])
    
  } else {
    # Iterating over all combinations
    for (i in 1:nrow(all_test)) {
      
      # Current test
      current_test <- all_test[i,]
      
      # Value of current test
      value <- sum(x$v[current_test])
      
      # Weight of current test
      weight <- sum(x$w[current_test])
      
      # If weight is less than knapsack size (W) and value is less than previous least value
      if(weight<=W & value>ret_value){
        ret_value <- value
        elements <- (1:n)[current_test]
      }
      
    }
    # Return the most optimum of values and elements
    return(list(value=ret_value, elements=elements))
  }
  
}




