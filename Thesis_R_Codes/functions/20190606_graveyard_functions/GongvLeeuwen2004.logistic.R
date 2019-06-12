library(dplyr)

GongvLeeuwen2004.logistic <- function(x.input,
                                      connectivity.matrix,
                                      a = 1.7, eps = 0.8,
                                      order = 0){
  
  # eps: coupling strength
  inp.dim <- x.input %>% dim()
  height <- inp.dim[1]
  leng <- inp.dim[2]
  if(!is.null(leng)) x.now <- x.input[,leng]
  if(is.null(height)) height <- x.input %>% length()
  if(is.null(leng)) x.now <- x.input
  
  # Higher orders are not implemented yet
  #if(order) x <- x.input[,len-order:len]
  
  # unit.vector allows to calculate M_i by multiplying it the connectivity matrix
  unit.vector <- matrix(1, height, 1)
  M <- connectivity.matrix %*% unit.vector
  fx <- x.now %>% func.1() %>% as.matrix()
  
  x.next <- (1 - eps)*fx + connectivity.matrix %*% fx*eps / M
  
  x.next %>% return()
  
}

func.1 <- function(x, a = 1.7){1 - a*(x^2)}