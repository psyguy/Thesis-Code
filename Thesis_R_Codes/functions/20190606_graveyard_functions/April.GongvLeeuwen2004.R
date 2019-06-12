library(dplyr)

func.1 <- function(x, a = 1.7){1 - a*(x^2)}

make.random.graph <- function(size = 5,
                              num.links = 10,
                              distribution = "binary",
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

GongvLeeuwen2004.logistic <- function(x.input,
                                      connectivity.matrix,
                                      a = 1.7,
                                      eps = 0.8){
  # eps: coupling strength
  # eps <- 0.8
  # x.input <- x.init
  # connectivity.matrix <- conn
  
  x.now <- x.input <- x.init
  n.nodes <- x.now %>% ncol()

  
  M <- connectivity.matrix %*% rep(1,n.nodes) %>% t()
  fx <- x.now %>% func.1() %>% as.matrix()# %>% t()
  
  neighbors <- fx %*% connectivity.matrix*eps / M
  neighbors[is.nan(neighbors)] <- 0
  
  x.next <- (1 - eps)*fx + neighbors
  
  x.next %>% return()
  
}

func.1 <- function(x, a = 1.7){1 - a*(x^2)}

GongvLeeuwen2004.coherenceD <- function(x.input){
  
  x.duplicated <- rep(1,ncol(x.input)) %*% x.input
  d <- (t(x.duplicated) - x.duplicated) %>% abs()
  diag(d) <- NA
  d %>% return()
  
}

swap.edge <- function(connectivity.matrix, from.1, to.1, from.2, to.2){
  
  edge.1 <- connectivity.matrix[from.1,to.1]
  edge.2 <- connectivity.matrix[from.2,to.2]
  
  connectivity.matrix[from.1,to.1] <- edge.2
  connectivity.matrix[to.1,from.1] <- edge.2
  
  connectivity.matrix[from.2,to.2] <- edge.1
  connectivity.matrix[to.2,from.2] <- edge.1
  
  connectivity.matrix %>% return()
  
}

my.rewire <- function(x.input, conn){
  
  distances <- x.input %>% tail(1) %>% GongvLeeuwen2004.coherenceD()
  
  i_ <- sample.int(N,1)
  d_ <- distances[,i_]
  
  j_1 <- which.min(d_)
  j_2 <- which.max(d_)
  
  # if(!conn[i_,j_1]) conn %>% return()
  conn <- conn %>% swap.edge(from.1 = i_, to.1 = j_1,
                                               from.2 = i_, to.2 = j_2)
  conn %>% return()
  
  }

GongvLeeuwen2004.adaptive.rewire <- function(input){
  
  x.start <- input$x.tot %>% tail(1) 
  conn.start <- input$connectivity.matrix
  
  x.end <- x.start %>% GongvLeeuwen2004.logistic(conn.start)
  conn.end <- my.rewire(x.end, conn.start)
  
  g <- igraph::graph_from_adjacency_matrix(conn.end, mode = "undirected")
  ClCoef <- igraph::transitivity(g)
  
  output <- list(
                x.tot = rbind(input$x.tot, x.end),
                clustering.coefficient = c(input$clustering.coefficient, ClCoef),
                connectivity.matrix = conn.end
                )
  class(output) <- append(class(output),"GongvLeeuwen2004")
  output %>% return()
}
