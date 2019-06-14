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
  c_0 <- h_$coef.clustering[[1]]
  e_0 <- h_$mat.connectivity[[1]] %>% netmeas_efficiency()
  f <- freq_snapshot
  
  t_ <- seq(f, length(h_$coef.clustering), f)[1:(limit/f)]
  l_ <- t_ %>% length()
  
  c_ <- c()
  e_ <- c()
  
  for(t in t_){
    c_ <- c_ %>% c(h_$coef.clustering[[t]])
    e_i <- h_$mat.connectivity[[t]] %>% netmeas_efficiency()
    e_ <- e_ %>% c(e_i)
  }
  
  s_ <- c_*e_/(c_0*e_0)
  
  name <- b@name %>% rep(l_)
  seed <- b@parameters$seed %>% rep(l_)
  eps <- b@parameters$eps %>% rep(l_)
  global_minmax <- b@parameters$global_minmax %>% rep(l_)
  blind_swap <- b@parameters$blind_swap %>% rep(l_)

  coef.clustering <- c_/c_0
  coef.efficiency <- e_/e_0
  coef.smallworld <- s_
  rewiring <- t_
  
  coefs <- cbind(
    name,
    seed,
    eps,
    global_minmax,
    blind_swap,
    rewiring,
    coef.clustering,
    coef.efficiency,
    coef.smallworld
  ) %>% as.data.frame()
  
  coefs[6:9] <- lapply(coefs[6:9], function(x) as.numeric(as.character(x)))
  
  coefs %>% return()
}
