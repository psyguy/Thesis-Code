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

netmeas_coefs <- function(initial = NULL,
                          now = NULL,
                          parameters = NULL,
                          name = NULL,
                          t_ = 0,
                          b = NULL,
                          normalize.s = TRUE,
                          concise = FALSE,
                          limit = 10000,
                          freq_snapshot = 200){
  # h_ <- b@history
  # m_0 <- h_$mat.connectivity[[1]]
  # c_0 <- h_$coef.clustering[[1]]
  # g_0 <- m_0 %>% graph_from_adjacency_matrix(mode = "undirected")
  # modu_0 <- g_0 %>% cluster_fast_greedy() %>% modularity()
  # pl_0 <- g_0 %>% average.path.length(unconnected = TRUE)
  # e_0 <- m_0 %>% netmeas_efficiency()
  
  if(!is.null(b)) initial <- b@initial -> now
  
  
  m_0 <- initial
  if(is.list(initial)) m_0 <- initial$mat.connectivity[[1]]
  g_0 <- m_0 %>% graph_from_adjacency_matrix(mode = "undirected")
  
  c_0 <- m_0 %>% my_clustceof()
  e_0 <- m_0 %>% netmeas_efficiency()
  # modu_0 <- g_0 %>% cluster_fast_greedy() %>% modularity()
  # pl_0 <- g_0 %>% average.path.length(unconnected = TRUE)
  
  
  l_ <- t_ %>% length()

  m_ <- now#$mat.connectivity[[1]] #h_$mat.connectivity[[t]]
  if(is.list(now)) m_ <- now$mat.connectivity[[1]]
  g_ <- m_ %>% graph_from_adjacency_matrix(mode = "undirected")
  
  c_ <- m_ %>% my_clustceof()
  e_ <- m_ %>% netmeas_efficiency()
  modu_ <- g_ %>% cluster_fast_greedy() %>% modularity()
  pl_ <- g_ %>% average.path.length(unconnected = TRUE)
    
    
  s_ <- ifelse(normalize.s, c_*e_/(c_0*e_0), c_*e_) 
  
  name <- name #%>% rep(l_)
  seed <- parameters$seed #%>% rep(l_)
  round <- parameters$round
  p_d <- parameters$params.eps_a
  alphabeta.eps <- paste0("(",p_d[1],", ",p_d[2],", ",p_d[3],")")
  alphabeta.a <- paste0("(",p_d[4],", ",p_d[5],", ",p_d[6],")")
  
  # eps <- b@parameters$partitions$eps %>% rep(l_)
  # a <- b@parameters$partitions$a %>% rep(l_)
  # global_minmax <- b@parameters$global_minmax %>% rep(l_)
  # blind_swap <- b@parameters$blind_swap %>% rep(l_)
  
  coef.clustering <- c_#/c_0
  coef.efficiency <- e_#/e_0
  coef.smallworld <- s_
  coef.modularity <- modu_#/modu_0
  coef.avgpathlength <- pl_#/pl_0
  rewiring <- t_
  
  coefs <- cbind(
    name,
    seed,
    round,
    alphabeta.eps,
    alphabeta.a,
    rewiring,
    coef.clustering,
    coef.efficiency,
    coef.smallworld,
    coef.modularity,
    coef.avgpathlength
  ) %>% as.data.frame()
  
  colnames(coefs) <- c("Owner", "Seed",
                       "Round", "Epsilon Proportion",
                       "a Proportion", "Rewiring",
                       "Clustering", "Efficiency",
                       "Small World", "Modularity",
                       "Avg Path Length")
  
  coefs[6:ncol(coefs)] <- lapply(coefs[6:ncol(coefs)], function(x) as.numeric(as.character(x)))
  
  if(concise) coefs <- coefs %>% select("Clustering", "Efficiency",
                                        "Small World", "Modularity",
                                        "Avg Path Length")
  
  coefs %>% return()
}



# a function to calculate coefficients on partitioned graphs --------------

netmeas_wbcoefs <- function(m,
                            parameters = p,
                            name = name,
                            rewiring = 0,
                            size.minority = 50,
                            size.majority =250){
  
  minority <- m[1:50,1:50]
  majority <- m[51:300,51:300]
  # removing the within group connections
  interpartition <- m
  interpartition[1:50,1:50] <- interpartition[51:300,51:300] <- 0
  m.list <- list(whole = m,
                 minority = minority,
                 majority = majority,
                 interpartition = interpartition)
  
  coefs.wb <- m.list %>%
    plyr::ldply(function(x) netmeas_coefs(x, x,
                                          concise =  F,
                                          parameters = p,
                                          name = name,
                                          t_ = rewiring,
                                          normalize.s = F))
  
  coefs.wb %>% return()
}
