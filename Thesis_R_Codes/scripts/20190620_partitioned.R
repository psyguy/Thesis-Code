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

vat_name <- NULL
vat_num_nodes <- 300
vat_num_edges <- 5200
vat_seed <- 2018
partitions <- list(l_eps = c(0.3, 0.4, 0.5),
                   r_eps = c(1, 0, 0),
                   l_a = c(1.4, 1.7, 2),
                   r_a = c(0, 0, 1)
                   )

vat_eps <- make_paramvect(l_ = partitions$l_eps,
                          r_ = partitions$r_eps,
                          n = vat_num_nodes,
                          seed = vat_seed)

vat_a <- make_paramvect(l_ = partitions$l_a,
                        r_ = partitions$r_a,
                        n = vat_num_nodes,
                        seed = vat_seed)

parameters =  list(n_nodes = vat_num_nodes,
                   n_edges = vat_num_edges,
                   partitions = partitions,
                   eps = vat_eps,
                   a = vat_a,
                   seed = vat_seed,
                   lower_bound_starting = 0,
                   global_minmax = FALSE,
                   blind_swap = FALSE)

brain_case <- NULL
for(days in 1:10){
  brain_case <- trial_grow(parameters =  parameters,
                           n_rewires = 1000,
                           n_updates = 20,
                           freq_snapshot = 200,
                           name = vat_name,
                           brain_younger = brain_case,
                           quiet = FALSE)
  
  brain_case@history$coef.clustering %>% plot(main = paste(brain_case@name,
                                                           "at",
                                                           brain_case@age$rewires %/% 1000,
                                                           "days."))
  
  # 
  # tmp_seq <- seq(1, brain_case@age$rewires, 10)
  # 
  # sbs_h <- (brain_case@history$activities[tmp_seq,])
  # 
  # sbs_h[,12] %>% plot()
  # 
  # sbs_h %>% gplots::heatmap.2(dendrogram = 'none',
  #                             Rowv = FALSE,
  #                             Colv = FALSE,
  #                             margins = c(1, 1),
  #                             col = colorRampPalette(c("white","yellow","orange","red"))(n = 299),#brewer.pal(name = "RdBu"),
  #                             # key = FALSE,
  #                             # density.info = "none",
  #                             trace = 'none',
  #                             xlab = "nodes",
  #                             ylab = "rewirings",
  #                             main = paste(brain_case@name,
  #                                          "\n with",
  #                                          brain_case@parameters$n_edges,
  #                                          "edges \n after",
  #                                          brain_case@age$rewires-1,
  #                                          "rewirings")
  #                             )
  
  
}

save_vars(list.of.vars = "brain_case",
          prefix = paste0("brain_",
                          brain_case@name,
                          "_",
                          brain_case@parameters$n_edges,
                          "edges"))

