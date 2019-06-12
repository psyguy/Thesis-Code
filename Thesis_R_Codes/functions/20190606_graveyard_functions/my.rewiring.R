my.rewiring <- function(inp.x, inp.conn, t.transient = 20, a = 1.7, eps = 0.8){
  
  x.temp <- inp.x
  conn <- inp.conn <- conn
  
  for (i in 1:t.transient) x.temp <- x.temp %>% Hellrigel2019.logistic(conn, a = a, eps = eps)
  
  distances <- x.temp %>% Hellrigel2019.syncerror(connectivity.matrix = conn)
  
  num.nodes <- dim(inp.conn)[1]
  
  i_ <- sample.int(num.nodes,1)
  d_ <- distances[,i_]
  k_val <- min(d_[-i_])
  k_ <- which(d_==k_val)
  l_ <- (d_ * conn[i_,]) %>% which.max()
  distances[i_,k_]
  #if(!conn[i_,k_]) 
    conn <- conn %>% swap.edge(i_, k_, i_, l_)

  # Skipped step 5 probabilistic approach
  
  g <- igraph::graph_from_adjacency_matrix(conn, mode = "undirected")
  clustering.coef <- transitivity(g)
  
  output <- list(x = x.temp, connectivity.matrix = conn, clustering.coef = clustering.coef)
  
  output %>% return()

}
