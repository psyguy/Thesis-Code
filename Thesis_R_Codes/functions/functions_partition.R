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
                              final.age = 10){
  
  if(!is.null(brain_case)){
    if((brain_case@age$rewires-1) >= final.age*1000) return(brain_case)
    }
  
  alpha <- c(0.5, 1, 1, 1, 2, 5)
  beta <- c(0.5, 1, 2, 5, 1, 1)
  alphbet <- data.frame(alpha,beta)
  
  name <- NULL
  num_nodes <- 300
  num_edges <- 5200
  seed <- round

  params.eps_a <- alphbet[row_eps,] %>% cbind(alphbet[row_a,]) %>%
    format(nsmall=1)
  colnames(params.eps_a) <- c("eps", "eps", "a", "a") %>%
    paste(colnames(params.eps_a), sep = ".")
  
  
  eps <- make_paramdist(alpha_beta = params.eps_a[1:2],
                            range_param = c(0.3,0.5),
                            n = num_nodes,
                            seed = seed)
  
  a <- make_paramdist(alpha_beta = params.eps_a[3:4],
                          range_param = c(1.4,2),
                          n = num_nodes,
                          seed = seed + 1)
  
  parameters =  list(params.eps_a = params.eps_a,
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
                             quiet = TRUE)
    if((brain_case@age$rewires-1) == final.age*1000) break
  }
  
  brain_case %>% return()
  
}

# 
# save_vars(list.of.vars = "brain_case",
#           prefix = paste(brain_case@parameters$brain.code, brain_case@name, sep = "_"))


