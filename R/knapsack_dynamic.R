knapsack_dynamic <- function(x, W){
  
  # n=number of elements in data
  n <- nrow(x)
  
  # m = matrix of experimented weights
  m <- matrix(ncol=W+1,
              nrow=n+1)
  # First row = 0
  m[1,] <- 0
  
  # First column = 0
  m[,1] <- 0
  
  # Finding the optimum value
  for(i in 1:n){
    for(j in 1:(W+1)){
      if(x$w[i]>j){
        m[i+1,j] <- m[i,j]
      } else {
        m[i+1,j] <- max(m[i,j], m[i,j-x$w[i]] + x$v[i])
      }
    }
  }
  # Function to find elements of the opitmum value
  find_element <- function(i, j){
    if(i==1){
      return()
    }
    if(m[i+1,j]>m[i,j]){
      return(append(i, find_element(i-1, j-x$w[i])))
    } else {
      return(find_element(i-1,j))
    }
  }
  return(list(value=max(m),
              elements=sort(find_element(n,W))))
}