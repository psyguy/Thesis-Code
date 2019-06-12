library(dplyr)

Hellrigel2019.logistic <- function(inp.x, connectivity.matrix, a = 1.7, eps = 0.8, order = 0){
  
  inp.dim <- inp.x %>% dim()
  height <- inp.dim[1]
  leng <- inp.dim[2]
  if(!is.null(leng)) x.now <- inp.x[,leng]
  if(is.null(height)) height <- inp.x %>% length()
  if(is.null(leng)) x.now <- inp.x
  
  # Higher orders are not implemented yet
  #if(order) x <- inp.x[,len-order:len]
  
  # unit.vector allows to calculate M_i by multiplying it the connectivity matrix
  unit.vector <- matrix(1, height, 1)
  M <- connectivity.matrix %*% unit.vector
  fx <- x.now %>% func.1(a = a) %>% as.matrix()
  
  x.next <- (1 - eps)*fx + connectivity.matrix %*% fx*eps / M
  
  x.next %>% return()
  
}

func.1 <- function(x, a = 1.7){1 - a*(x^2)}