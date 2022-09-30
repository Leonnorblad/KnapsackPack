greedy_knapsack <- function(x,W){
  
  # Value per Weight
  v_per_w <- x$v/x$w
  
  # Ordering x based on the Value per Weight
  order_x <- x[order(v_per_w, decreasing = TRUE),]
  
  # Inital values
  W_temp <- 0
  value <- 0
  elements <- c()
  
  # n=number of elements in data
  n <- nrow(x)
  
  # Putting each item in the knapsack, in order of the Value per Weight
  for(i in 1:n){
    
    # Will stop if the next item cant fit
    if((W_temp+order_x[i,1])>W) {return(list(value=value, elements=elements, weight=W_temp))}
    
    # Cumulative weight of the knapsack
    W_temp <- sum(W_temp, order_x[i,1])
    
    # Cumulative value of the knapsack
    value <- sum(value, order_x[i,2])
    
    # Elements in the knapsack 
    elements <- append(elements, as.numeric(rownames(order_x)[i]),)
  }
  
}