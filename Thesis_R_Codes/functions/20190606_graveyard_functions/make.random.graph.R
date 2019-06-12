library(dplyr)

make.random.graph <- function(size = 5, num.links = 10, distribution = "binary",
                              parameters = c(0,1), seed = 1){
  
  set.seed(seed)
  
  edges <- rep(0,size*(size-1)/2)
  if(distribution == "binary"){
    ones <- sample.int(length(edges), num.links)
    edges[ones] <- 1
  }
  
  g <- matrix(0, size, size)
  g[base::lower.tri(g, diag=FALSE)] <- edges
  
  (adj.matrix <- g + t(g)) %>% return()
  
}
