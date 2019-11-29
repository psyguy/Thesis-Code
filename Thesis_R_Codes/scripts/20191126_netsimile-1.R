# rm(list=ls())
source("./functions/functions_extract.R")
load("./data-pc/last-snapshot-conn-activity_20191010_1926.RData")

#### NetSimile #################

library(gtools) 
library(sna)
library(igraph)
library(e1071)
library(HHG)

featExt <- function(A){
  # A <- adjvec %>% vec2mat()
  G <- graph_from_adjacency_matrix(A, "undirected")
  # A <- as.matrix(get.adjacency(G))
  v <- vcount(G)
  FM <- matrix(0, nrow=v, ncol=7) # Feature Matrix
  for(node in 1:v){
    #1) Degree: No of neighbours
    neighbours <- neighbors(G, node)
    FM[node,1] <- length(neighbours)
    
    #2) Clustering cofficient 
    FM[node,2] <- transitivity(G, type=c("local"), vids=node, isolates=c("zero"))
    
    #3) average number of node i's two hop away distance neighbours
    nsize <- neighborhood.size(G, order=1, nodes=neighbours)
    if(length(nsize) > 0){
      FM[node,3] <- mean(nsize)
    }else{
      FM[node,3] <- 0  # Replacing NaN with 0
    }
    
    # 4 average clustering cofficient of neighbours
    avgNC <- transitivity(G, type=c("local"), vids=neighbours, isolates=c("zero"))
    if(length(avgNC) > 0 ){
      FM[node,4] <- mean(avgNC)
    }else{
      FM[node,4] <- 0  # Replacing NaN with 0
    }
    
    # 5 No of edges in ego(i)
    FM[node,5] <- sapply(ego.extract(A, ego=node),sum)/2
    
    # 6 No of outgoing edges from ego(i)
    egoNetwork <- igraph::neighborhood(G, nodes=node, order=1)[[1]]
    allNeighbours <- unlist(igraph::neighborhood(G, order=1, nodes = egoNetwork))
    FM[node,6] <- length(allNeighbours[!allNeighbours %in% egoNetwork])
    
    # No of neighbours of ego(i)
    FM[node,7] <- sum(sapply(igraph::neighborhood(G, order=1, nodes=egoNetwork), length)) - length(egoNetwork)
  }
  FM
}

## Algo 3 Features Aggregation : Generating Signature Vectors 

featAggr <- function(FM){
  sig <- c()
  sig <- append(sig, apply(FM,2, median))    # Median of each feature
  sig <- append(sig, apply(FM,2, mean))      # Mean of each feature
  sig <- append(sig, apply(FM,2, sd))        # Standard Deviation of each feature
  sig <- append(sig, apply(FM,2, skewness, na.rm=T))  # Skewness of each feature
  sig <- append(sig, apply(FM,2, kurtosis, na.rm=T))  # Kurtosis of each feature
  sig
}

##### Algo 1: NetSimile, computing similarity scores between pair of graphs


s <- l.extracted$`Hyper-coupled minority_Sam Evrard`$m %>% featExt()
s %>% featAggr()
s[4,] %>% densityplot()



calc_signature <- function(s) featAggr(featExt(s))

l.connectivities <- l.extracted %>% map("m")
l.activities <- l.extracted %>% map("a") %>% map(my_coherenceD)

#takes 130 + 392  seconds
signatures.connectivities <- l.connectivities %>% ldply(calc_signature)
signatures.activities <- l.activities %>% ldply(calc_signature)


d <- signatures.activities %>% stats::dist(method = "canberra")

d %>% hclust() %>% plot()




# making feature distributions --------------------------------------------

system.time(features.connectivities <- l.connectivities %>% map(featExt))

# features.activities <- l.activities %>% ldply(featExt)

signatures.connectivities <- features.connectivities %>% ldply(featAggr)

dist.signatures <- signatures.connectivities[-1] %>%
  dist(method = "canberra") %>% 
  as.matrix()

system.time(
  distances.connectivities <- features.connectivities %>% map(~as.matrix(dist(.x, diag = TRUE, upper = TRUE)))
            )



# HHG on features ---------------------------------------------------------


D.1 <- s.1 %>% dist(diag = TRUE, upper = TRUE) %>% as.matrix()
D.21 <- s.21 %>% dist(diag = TRUE, upper = TRUE) %>% as.matrix()

system.time(h <- hhg.test(distances.connectivities[[1]],distances.connectivities[[21]]))



l.hgg <- list()

d <- distances.connectivities
for(i in 1:length(d)){
  for(j in 1:(i)){
    h.tmp <- list(i,j,names(d[i]))
    k <- k+1
  }
}


d3 <- signatures.connectivities[c(21:30),] %>%
  dist(method = "canberra") %>%
  as.vector() %>% 
  densityplot()

di %>% pimage

di <- sigvectors %>%
  dist(method = "canberra") # %>% hclust() %>% plot()
c.d <- cluster::diana(di)
pltree(c.d, cex = 0.6, hang = -1,
       main = "RV")
rect.hclust(c.d, k = 5, border = 2:5)


di %>% hclust(method = "ward.D") %>% plot()


wilcox.test(as.numeric(sigvectors[1,]),as.numeric(sigvectors[4,]))


