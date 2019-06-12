library(dplyr)

seed <- 1
L_c <- 5200
N <- 300
T_ <- 6000 
tol <- 0.001

set.seed(seed)

conn <- make.random.graph(size = N, num.links = L_c, seed = seed)

x.init <- N %>% runif(-1,1)
x.out <- x.init

# for (i in 1:T_) {
#   x.temp <- x.out %>% GongvLeeuwen2004.logistic(conn)
#   x.out <- x.out %>% cbind(x.temp)
# }

save(x.out, file = "x.out_5200.300.6000.Rdata")

distances <- GongvLeeuwen2004.coherenceD(x.out,connectivity.matrix = conn)

i_ <- sample.int(N,1)

d_ <- distances[,i_]
d_ <- d_[i_]

j_1 <- which.min(d_)
j_2 <- which.max(d_)

if(!conn[i_,j_1]) conn <- conn %>% swap.edge(i1 = i_, j1 = j_1, i2 = i_, j2 = j_2)

g <- graph_from_adjacency_matrix(conn, mode = "undirected")

ClCoef <- transitivity(g)


transitivity
