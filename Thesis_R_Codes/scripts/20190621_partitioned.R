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

rm(list = ls())

source("./functions/functions_trial.R")

alpha <- c(0.5, 1, 1, 1, 2, 5)
beta <- c(0.5, 1, 2, 5, 1, 1)
l_ <- data.frame(alpha,beta)

vat_name <- NULL
vat_num_nodes <- 300
vat_num_edges <- 5200
round <- 1
                                                                                     
vat_seed <- 2001

row_eps <- 1
row_a <- 2

params.dist <- l_[row_eps,] %>% cbind(l_[row_a,]) %>%
  format(nsmall=1)
colnames(params.dist) <- c("eps", "eps", "a", "a") %>%
  paste(colnames(params.dist), sep = ".")


vat_eps <- make_paramdist(alpha_beta = params.dist[1:2],
                          range_param = c(0.3,0.5),
                          n = vat_num_nodes,
                          seed = vat_seed)

vat_a <- make_paramdist(alpha_beta = params.dist[3:4],
                        range_param = c(1.4,2),
                        n = vat_num_nodes,
                        seed = vat_seed + 1)

parameters =  list(n_nodes = vat_num_nodes,
                   n_edges = vat_num_edges,
                   params.dist = params.dist,
                   eps = vat_eps,
                   a = vat_a,
                   seed = vat_seed,
                   lower_bound_starting = 0,
                   global_minmax = FALSE,
                   blind_swap = FALSE)

brain_case <- NULL
for(days in 1:5){
  brain_case <- trial_grow(parameters =  parameters,
                           n_rewires = 1000,
                           n_updates = 20,
                           freq_snapshot = 200,
                           name = vat_name,
                           brain_younger = brain_case,
                           quiet = FALSE)
  
  brain_case@history$coef.clustering %>%
    plot(main = paste(brain_case@name,
                      "at",
                      brain_case@age$rewires %/% 1000,
                      "days."))
  
}

save_vars(list.of.vars = "brain_case",
          prefix = paste0("brain_",
                          brain_case@name,
                          "_",
                          brain_case@parameters$n_edges,
                          "edges"))

