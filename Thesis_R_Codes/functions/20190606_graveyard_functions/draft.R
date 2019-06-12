# library(dplyr)
# library(netdiffuseR)
# 
# x <- c(.3,.5,.7,.11) %>% as.matrix()
# 
# for (i in 1:100) {
#   x.new <- x %>% GongvLeeuwen2004.logistic(conn)
#   x <- x %>% cbind(x.new)
# }
# 
# x_new = (1-eps)*(1-a*x_prev.^2) + (eps./M_i).*connectivity_mat*(1-a*x_prev.^2)
# 
# 
# x <- c(3,5,7,11)
# 
# conn <- matrix(c(0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,0), nrow = 4) %>% as.matrix()
# 
# 
# size <- 1e1
# samples <- 20
# vals <- sample.int(size ^ 2, samples)
# edge.list <- cbind(vals %/% size + 1, vals %% size)
# edge.list.rev <- edge.list[,2] %>% cbind(edge.list[,1])
# 
# edge.list <- edge.list %>% rbind(edge.list.rev)
# adj.matrix <- edgelist_to_adjmat(edge.list, recode.ids=FALSE, keep.isolates = F)
# 
# el  <- cbind(a=1:5, b=5:1) #edgelist (a=origin, b=destination)
# mat <- matrix(0, 10, 10)
# mat[edge.list] <- 1
# 
# 
# 
# graph <- matrix(c(
#   1,2,6,
#   6,6,7
# ), ncol=2)
# 
# # Generates an adjmat of size 4 x 4
# edgelist_to_adjmat(graph)
# 
# # Generates an adjmat of size 7 x 7
# edgelist_to_adjmat(graph, recode.ids=FALSE)
