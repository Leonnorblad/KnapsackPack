#' Dynamic method
#' 
#' The dynamic algorithm divides the problem into sub-problems, solves them separately and stores the results in a matrix. It then uses the matrix to solve a wholistic problem by using the stored values when a similar/same problem is encountered.
#' 
#' @param x A \code{data.frame} of two non negative numeric columns \code{w} and \code{v}. 
#' 
#' @param W An integer, the knapsack weight limit.
#' 
#' @return A list of two elements.
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
#' knapsack_dynamic(x=x[1:8,], W=3500)
#' 
#' @references Wikipedia contributors. (2022, May 18). Knapsack problem. In Wikipedia, The Free Encyclopedia. Retrieved 06:38, October 4, 2022, from \url{https://en.wikipedia.org/w/index.php?title=Knapsack_problem&oldid=1088471265}
#' @export

knapsack_dynamic <- function(x, W){
  
  # Check input
  if(!is.data.frame(x)|
     any(!(names(x) %in% c("w", "v")))|
     !any(is.numeric(x$w))|
     !any(is.numeric(x$v))|
     any(x<0)|
     W<0) { stop("False input")}
  
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