brute_force_knapsack <- function(x,W){
  
  # Check input
  if(!is.data.frame(x)|
     !all(names(x)==(c("w", "v")))|
     !any(is.numeric(x$w))|
     !any(is.numeric(x$v))|
     any(x<0)) { stop("False input")}
  
  # Inital values
  ret_value <- 0
  elements <- NA
  value <- NA
  
  # n=number of elements in data
  n <- nrow(x)
  
  # All combinations 
  all_test <- expand.grid(rep(list(0:1), n))==1
  
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