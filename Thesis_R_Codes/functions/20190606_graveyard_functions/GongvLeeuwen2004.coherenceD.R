library(dplyr)

GongvLeeuwen2004.coherenceD <- function(x.input){
  
  x.duplicated <- rep(1,ncol(x.input)) %*% x.input
  d <- (t(x.duplicated) - x.duplicated) %>% abs()
  diag(d) <- NA
  d %>% return()
  
}
