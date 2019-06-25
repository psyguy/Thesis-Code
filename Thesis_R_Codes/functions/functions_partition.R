# Notes -------------------------------------------------------------------

#
#   
#   cf. _trial. Added to that, the parameters now include vectors of
#   epsilon and a parameter, allowing partitioning.
#   
#
#
#
#
#


# init --------------------------------------------------------------------

# rm(list = ls())

source("./functions/functions_trial.R")


partition_culture <- function(brain_case = NULL,
                              row_eps = 1,
                              row_a = 1,
                              round = 0,
                              alphabeta = NULL,
                              final.age = 10){
  
  if(!is.null(brain_case)){
    if((brain_case@age$rewires-1) >= final.age*1000) return(brain_case)
    }
  
  name <- NULL
  num_nodes <- 300
  num_edges <- 5200
  seed <- round %>% as.numeric()
  
  # # since the input is the the actual alpha and beta parameters
 
  #if(is.null(alphabeta)){
  #  alpha <- c(0.5, 1, 1, 1, 2, 5)
  #  beta <- c(0.5, 1, 2, 5, 1, 1)
  #  alphabeta <- data.frame(alpha,beta)
  #}
  
  if(FALSE){#ncol(alphabeta)==2){
    alphabeta_eps <- alphabeta[row_eps,]
    #colnames(alphabeta_eps) <- c("eps.alpa", "eps.beta")
    
    alphabeta_a <- alphabeta[row_a,]
    #colnames(alphabeta_eps) <- c("a.alpa", "a.beta")
    range_eps <- c(0.3,0.5)
    range_a <- c(1.4,2)
  }
  
  #if(TRUE){#ncol(alphabeta)==3){
  alphabeta_eps <- alphabeta[row_eps,]
   #colnames(alphabeta_eps) <- c("eps.low", "eps.medium", "eps.high")

   alphabeta_a <- alphabeta[row_a,]
   #colnames(alphabeta_a) <- c("a.low", "a.medium", "a.high")
  #  range_eps <- c(0.3,0.5)
  #  range_a <- c(1.5,1.9)
  #}
  
  
  
  eps <- make_paramdist(alpha_beta = row_eps,#alphabeta_eps,
                        range_param = c(0.3,0.5),#range_eps,
                        l = num_nodes,
                        seed = seed)
  
  a <- make_paramdist(alpha_beta = row_a,#alphabeta_a,
                      range_param = c(1.5,1.9),#range_a,
                      l = num_nodes,
                      seed = seed + 1)
  
  parameters =  list(params.eps_a = cbind(alphabeta_eps,alphabeta_a),
                     round = round,
                     n_nodes = num_nodes,
                     n_edges = num_edges,
                     seed = seed,
                     round = round,
                     lower_bound_starting = 0,
                     brain.code = "",
                     eps = eps,
                     a = a)
  
  repeat{
    brain_case <- trial_grow(parameters =  parameters,
                             n_rewires = 1000,
                             n_updates = 20,
                             freq_snapshot = 200,
                             name = name,
                             brain_younger = brain_case,
                             quiet = FALSE)
    if((brain_case@age$rewires-1) == final.age*1000) break
  }
  
  brain_case %>% return()
  
}

# 
# save_vars(list.of.vars = "brain_case",
#           prefix = paste(brain_case@parameters$brain.code, brain_case@name, sep = "_"))


