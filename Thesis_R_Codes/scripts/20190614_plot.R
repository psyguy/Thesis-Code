rm(list = ls())

source("./functions/functions_trial.R")
source("./functions/functions_netmeas.R")

# time series of nodes ----------------------------------------------------

library(ggplot2)
library(reshape)


# data <- data.frame(time = seq(0, 23), noob = rnorm(24), plus = runif(24), extra = rpois(24, lambda = 1))
# Molten <- melt(data, id.vars = "time")
# ggplot(Molten, aes(x = time, y = value, colour = variable)) + geom_line()








# for loop over all brain cases -------------------------------------------


sampled.path <- "./data/"
sampled.names <- list.files(path = sampled.path, pattern = "*.RData")

coefs_all <- NULL
rewires <- seq(1, 10001, 5)
t <- Sys.time()
for(sampled in sampled.names){
  load(paste(sampled.path, sampled, sep = ""))
  brain_case@name %>% print()
  coefs_all <- coefs_all %>% rbind(brain_case %>% netmeas_coefs())
  
  plot.title<- paste("Node activities of", brain_case@name)

  sbs_h <- (brain_case@history$activities[rewires,1:300]) %>%
    as.data.frame()

  # sbs_h[,12] %>% plot()

  data <- sbs_h %>% cbind(rewires)

  # Molten$variable %>% str()

  # data <- data.frame(time = seq(0, 23), noob = rnorm(24), plus = runif(24), extra = rpois(24, lambda = 1))
  Molten <- melt(data, id.vars = "rewires")

  ggplot(Molten,
         aes(x = rewires,
             y = value,
             colour = variable)) +
    geom_line(alpha = 0.5) + theme(legend.position="none") + ylim(-1, 1) + ggtitle(plot.title)
  ggsave(paste0("activities.",brain_case@name,".png"), dpi = 500)
}
Sys.time()-t


# plotting coefficients over time -----------------------------------------


coefs_all %>% ggplot(aes(x = rewiring,
                         y = coef.clustering,
                         colour = name)) +
              geom_line(size = 2, alpha = 0.8) + 
              ggtitle("Clustering Coefficient")
ggsave("coef.clustering.png")

coefs_all %>% ggplot(aes(x = rewiring,
                         y = coef.efficiency,
                         colour = name)) +
                       geom_line(size = 2, alpha = 0.8) + 
                       ggtitle("Global Efficiency")
ggsave("coef.efficiency.png")
                     
coefs_all %>% ggplot(aes(x = rewiring,
                         y = coef.smallworld,
                         colour = name)) +
                       geom_line(size = 2, alpha = 0.8) + 
                       ggtitle("Small World index")
ggsave("coef.smallworld.png")


# saving the parameters ---------------------------------------------------

parameters_df <- coefs_all %>% filter(rewiring == 200) %>% arrange(name) %>% select(1:5)


# --- Graph 1 : If you want ONLY the table in your image :
# First I create an empty graph with absolutely nothing :
qplot(1:10, 1:10, geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
  # Then I add my table :
  annotation_custom(grob = tableGrob(parameters_df))
ggsave("brain_parameters.png")



save_vars(list.of.vars = c("parameters_df", "coefs_all"), prefix = "9brains")
