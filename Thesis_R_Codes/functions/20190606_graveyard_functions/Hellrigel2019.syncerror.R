library(dplyr)

Hellrigel2019.syncerror <- function(x.input, connectivity.matrix){

  inp.dim <- x.input %>% dim()
  height <- inp.dim[1]
  leng <- inp.dim[2]
  if(!is.null(leng)) x.now <- x.input[,leng]
  if(is.null(height)) height <- x.input %>% length()
  if(is.null(leng)) x.now <- x.input
  
  x.duplicated <- x.now
  for (i in 1:(height-1)){x.duplicated <- x.now %>% cbind(x.duplicated)}
  
  x.duplicated <- x.duplicated %>% as.matrix()
  
  d <- (t(x.duplicated) - x.duplicated) %>% abs()
  
  diag(d) <- 0
  
  d %>% return()
  
}
