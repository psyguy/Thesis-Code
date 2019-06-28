load("I:/Thesis_Codes/Thesis_R_Codes/data/coefs.wb.b_20190628_1754.RData")

partition.list <- c("minority", "majority",
                    "interpartition", "whole")

max.cl <- coefs.wb.b$Clustering %>% max()
max.sw <- coefs.wb.b$`Small World` %>% max()
max.mo <- coefs.wb.b$Modularity %>% max()
max.de <- coefs.wb.b$Degree %>% max()
max.pl <- coefs.wb.b$`Avg Path Length` %>% max()
max.ef <- coefs.wb.b$Efficiency %>% max()

names <- coefs.wb.b$Owner %>% as.character() %>% unique()
t3 <- Sys.time()
for(name in names){
  print(name)
  coefs.this.round <- coefs.wb.b %>% filter(Owner == name)
  owner.name <- coefs.this.round$Owner[1] %>% as.character()
  title <- paste0("Coefficients for ",
                  name,
                  "eps ",
                  coefs.this.round$`Epsilon Proportion`,
                  ", a ",
                  coefs.this.round$`a Proportion`)

  p1 <- ggplot(data = coefs.this.round,
               aes(x = Rewiring,
                   y = Clustering,
                   colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) +
    ggplot2::ylim(0,max.cl)

  p2 <- ggplot(data = coefs.this.round,
               aes(x = Rewiring,
                   y = `Small World`,
                   colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) +
    ggplot2::ylim(0,max.sw)
  
  p3 <- ggplot(data = coefs.this.round,
               aes(x = Rewiring,
                   y = Modularity,
                   colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) + 
    ggplot2::ylim(0,max.mo)
  
  p4 <- ggplot(data = coefs.this.round,
               aes(x = Rewiring,
                   y = Degree,
                   colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) + 
    ggplot2::ylim(0,max.de)
  
  p5 <- ggplot(data = coefs.this.round,
               aes(x = Rewiring,
                   y = `Avg Path Length`,
                   colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) + 
    ggplot2::ylim(0,max.pl)
  
  p6 <- ggplot(data = coefs.this.round,
               aes(x = Rewiring,
                   y = Efficiency,
                   colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) + 
    ggplot2::ylim(0,max.ef)
  

  figure <- ggarrange(p1, p2, p3, p4, p5, p6,
                      ncol=1, #nrow=2,
                      common.legend = TRUE,
                      legend="bottom"
  )
  
  annotate_figure(figure, top = title)# %>% print()
  paste0("wb_", name, ".png") %>% ggsave(width = 7,
                                   height = 42,
                                   dpi = "retina")
}
Sys.time() - t3






# plots graphs with colored edges -----------------------------------------


g <- r3.red@now$mat.connectivity %>% graph_from_adjacency_matrix(mode="undirected")

V(g)$partition <- c(rep("minority", 50),
                    rep("majority", 250))




# select edges and set color 
E(g)[V(g)[name == "minority"] %--% V(g)[name == "minority"]]$color <- "blue2" # within.mino
E(g)[V(g)[name == "minority"] %--% V(g)[name == "majority"]]$color <- "darkorchid1" # between.minomajo
E(g)[V(g)[name == "majority"] %--% V(g)[name == "majority"]]$color <- "brown3" # within.majo
# plot
set.seed(1)
g %>% plot(vertex.size = 0,
           vertex.color = c("skyblue", "red")[1 + (V(g)$partition == "majority")],
           vertex.label = NA,
           edge.width = 1,
           edge.curved= 0.5,
           main = "red")
