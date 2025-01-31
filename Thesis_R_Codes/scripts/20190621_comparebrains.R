rm(list = ls())

# source("./functions/functions_trial.R")
source("./functions/functions_netmeas.R")
source("./functions/functions_partition.R")

# for loop over all brain cases -------------------------------------------

sampled.path <- "./data/20190625_hpc_data/"
sampled.names <- list.files(path = sampled.path, pattern = "*.RData")
coefs_all <- NULL
# rewires <- seq(1, 10001, 5)
t <- Sys.time()
for(sampled in sampled.names){
  rm(brain_case)
  load(paste(sampled.path, sampled, sep = ""))
  brain_case@name %>% print()
  coefs_all <- coefs_all %>%
    rbind(brain_case@history$coefficients)
}
Sys.time()-t


# plotting coefficients over time -----------------------------------------

coefficient.name <- (coefs_all %>% colnames())[6:10]

for(i in coefficient.name){
  this.coefficient <- coefs_all %>% select(i, rewiring, alphabeta.a, alphabeta.eps)
  colnames(this.coefficient)[1] <- "value"
  # this.coefficient$value <- this.coefficient$value/co$value[1]
  coef.plot <- this.coefficient %>% ggplot(aes(x = rewiring,
                                               y = value,
                                               colour = alphabeta.a,
                                               linetype = alphabeta.eps)) +
    geom_line(size = .75, alpha = 0.8) + 
    # ggplot2::ylim(0,NA) + 
    ggtitle(i)
  coef.plot %>% print()
  paste0(i, ".png") %>% ggsave()
}
