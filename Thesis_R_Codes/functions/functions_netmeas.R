# Intro -------------------------------------------------------------------

## This script keeps all functions needed for GongvLeeuwen2004.
## It's standalone and loads functions_my automatically

# rm(list = ls())


# loading the basic functions and packages --------------------------------

if(!exists("path.to.functions_my")) path.to.functions_my <- "functions"
source(paste0(path.to.functions_my,"/functions_my.R"))
source(paste0(path.to.functions_my,"/functions_heartbeat.R"))


# efficiency -------------------------------------------------------------

netmeas_efficiency <- function(m){
  
  d <- m %>% graph_from_adjacency_matrix() %>% shortest.paths()
  dd <- 1/d
  diag(dd) <- 0
  N <- dim(m)[1]
  
  (E <- sum(dd)/(N*(N-1))) %>% return()
  
}


# calculating S, C, and E, and returning a df together with model  --------

netmeas_coefs <- function(b,
                          limit = 10000,
                          freq_snapshot = 200){
  h_ <- b@history
  m_0 <- h_$mat.connectivity[[1]]
  c_0 <- h_$coef.clustering[[1]]
  e_0 <- m_0 %>% netmeas_efficiency()
  g_0 <- m_0 %>% graph_from_adjacency_matrix(mode = "undirected")
  modu_0 <- g_0 %>% cluster_fast_greedy() %>% modularity()
  pl_0 <- g_0 %>% average.path.length(unconnected = TRUE)
  f <- freq_snapshot
  t_ <- seq(f, length(h_$coef.clustering), f)[1:(limit/f)]
  l_ <- t_ %>% length()
  
  c_ <- c()
  e_ <- c()
  modu_ <- c()
  pl_ <- c()
  
  for(t in t_){
    m_i <- h_$mat.connectivity[[t]]
    g_i <- m_i %>% graph_from_adjacency_matrix(mode = "undirected")
    
    c_ <- c_ %>% c(h_$coef.clustering[[t]])
    e_i <- m_i %>% netmeas_efficiency()
    e_ <- e_ %>% c(e_i)
    
    modu_i <- g_i %>% cluster_fast_greedy() %>% modularity()
    modu_ <- modu_ %>% c(modu_i)
    
    pl_i <- g_i %>% average.path.length(unconnected = TRUE)
    pl_ <- pl_ %>% c(pl_i)
    
  }
  
  s_ <- c_*e_/(c_0*e_0)
  
  name <- b@name %>% rep(l_)
  seed <- b@parameters$seed %>% rep(l_)
  eps <- b@parameters$partitions$eps %>% rep(l_)
  a <- b@parameters$partitions$a %>% rep(l_)
  global_minmax <- b@parameters$global_minmax %>% rep(l_)
  blind_swap <- b@parameters$blind_swap %>% rep(l_)

  coef.clustering <- c_/c_0
  coef.efficiency <- e_/e_0
  coef.smallworld <- s_
  coef.modularity <- modu_#/modu_0
  coef.avgpathlength <- pl_#/pl_0
  rewiring <- t_
  
  coefs <- cbind(
    name,
    seed,
    eps,
    a,
    # global_minmax,
    # blind_swap,
    rewiring,
    coef.clustering,
    coef.efficiency,
    coef.smallworld,
    coef.modularity,
    coef.avgpathlength
  ) %>% as.data.frame()
  
  coefs[6:ncol(coefs)] <- lapply(coefs[6:ncol(coefs)], function(x) as.numeric(as.character(x)))
  
  coefs %>% return()
}
