swap.edge <- function(connectivity.matrix, from.1, to.1, from.2, to.2){
  
  edge.1 <- connectivity.matrix[from.1,to.1]
  edge.2 <- connectivity.matrix[from.2,to.2]
  
  connectivity.matrix[from.1,to.1] <- edge.2
  connectivity.matrix[to.1,from.1] <- edge.2

  connectivity.matrix[from.2,to.2] <- edge.1
  connectivity.matrix[to.2,from.2] <- edge.1
  
  connectivity.matrix %>% return()
  
}
