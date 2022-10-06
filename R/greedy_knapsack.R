#' Greedy method
#' 
#' The greedy algorithm order the elements based on the value per weight unit. It then fills the knapsack until the weight limit is reached.
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
#' greedy_knapsack(x=x[1:8,], W=3500)
#' 
#' @references Wikipedia contributors. (2022, May 18). Knapsack problem. In Wikipedia, The Free Encyclopedia. Retrieved 06:38, October 4, 2022, from \url{https://en.wikipedia.org/w/index.php?title=Knapsack_problem&oldid=1088471265}
#' @export

greedy_knapsack <- function(x,W){
  
  # Check input
  if(!is.data.frame(x)|
     any(!(names(x) %in% c("w", "v")))|
     !any(is.numeric(x$w))|
     !any(is.numeric(x$v))|
     any(x<0)|
     W<0) { stop("False input")}
  
  # Value per Weight
  v_per_w <- x$v/x$w
  
  # Ordering x based on the Value per Weight
  order_x <- x[order(v_per_w, decreasing = TRUE),]
  
  w_x <- order_x$w
  v_x <- order_x$v
  
  # Inital values
  W_temp <- 0
  value <- 0
  elements <- c()
  
  # n=number of elements in data
  n <- nrow(x)
  
  # Putting each item in the knapsack, in order of the Value per Weight
  for(i in 1:n){
    
    # Will stop if the next item cant fit
    if((W_temp+w_x[i])>W) {return(list(value=value, elements=elements))}
    
    # Cumulative weight of the knapsack
    W_temp <- sum(W_temp, w_x[i])
    
    # Cumulative value of the knapsack
    value <- sum(value, v_x[i])
    
    # Elements in the knapsack 
    elements <- append(elements, as.numeric(rownames(order_x)[i]),)
  }
}
