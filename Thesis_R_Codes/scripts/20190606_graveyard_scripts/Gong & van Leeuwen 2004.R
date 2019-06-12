library(dplyr)
rm(list=ls())
source("./My Thesis Functions/April.GongvLeeuwen2004.R")

seed <- 1
# number of links
L_c <- 520
# number of nodes
N <- 3000 #3000
# number of iterations
T_ <- 600 #600 
# tol <- 0.001

set.seed(seed)

conn <- make.random.graph(size = N, num.links = L_c, seed = seed)

x.init <- N %>% runif(-1,1) %>% t()

g <- igraph::graph_from_adjacency_matrix(conn, mode = "undirected")
ClCoef <- igraph::transitivity(g)

GvL.start <- list(
            x.tot = x.init,
            clustering.coefficient = ClCoef,
            connectivity.matrix = conn
            )

GvL.finished <- GvL.start

for (i in 1:10){
  print(i)
  GvL.finished <- GvL.finished %>% GongvLeeuwen2004.adaptive.rewire()
  g <- GvL.finished$connectivity.matrix %>% igraph::graph_from_adjacency_matrix(mode = "undirected")
  (ClCoef <- igraph::transitivity(g)) %>% print()
  }

GvL.finished$x.tot[,6]

(GvL.finished$x.tot[2,] - GvL.finished$x.tot[3,]) %>% sum()

%>% sum()
