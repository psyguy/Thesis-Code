# rm(list=ls())
library(ggpubr)
library(grid)

# reading the aggregated harvested data
load("./data/coefs.wb.all_20190630_1327.RData")

# making the verbal description of the parameter sets
coefs.all <- coefs.wb.all %>% mutate(`Verbal Description` = 
                                       paste(`Epsilon Proportion`, `a Proportion`, sep="_"))
coefs.all$`Verbal Description` <- coefs.all$`Verbal Description` %>% as.factor()

levels(coefs.all$`Verbal Description`) <- c("Hyper-coupled Minority",
                                            "Hyper-chaotic Minority",
                                            "Homogeneous Society",
                                            "Hypo-chaotic Minority",
                                            "Hypo-coupled Minority")


color.bg <- "white"
color.mino <- "deepskyblue3" #"blue2"
color.majo <- "orangered" #"brown3"
color.inter <- "olivedrab2" #"green4"
color.whole <- "azure4"

max.cl <- coefs.all$Clustering %>% max()
max.sw <- coefs.all$`Small World` %>% max()
max.mo <- coefs.all$Modularity %>% max()
max.de <- coefs.all$`Edge Density` %>% max()
max.pl <- coefs.all$`Avg Path Length` %>% max()
max.ef <- coefs.all$Efficiency %>% max()

names.list <- coefs.all$Owner %>% unique()
t3 <- Sys.time()
for(name in names.list[1:1]){
  print(name)
  coefs.this.round <- coefs.all %>% filter(Owner == name)
  
  title <- paste0(name,
                  " (",
                  coefs.this.round$`Verbal Description`[1],
                  ")")
  
  p.cl <- ggplot(data = coefs.this.round,
                 aes(x = Rewiring,
                     y = Clustering,
                     colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) +
    scale_colour_manual(values = c(color.inter, color.majo,
                                   color.mino, color.whole)) +
    ggplot2::ylim(0,max.cl) + 
    theme(legend.title = element_blank(),#element_text(size=12, color = "thistle4"),
          legend.justification=c(0,1), 
          legend.key.width = unit(1.5, "line"),
          legend.position= c(0, 1),#c(0.05, 0.95),
          legend.background = element_blank(),
          legend.text = element_text(size=8, color = "black", face="bold"),
          legend.key = element_blank())
  # + labs(subtitle="Legend: Top-Left Inside the Plot")
  
  p.sw <- ggplot(data = coefs.this.round,
                 aes(x = Rewiring,
                     y = `Small World`,
                     colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) +
    scale_colour_manual(values = c(color.inter, color.majo,
                                   color.mino, color.whole)) +
    ggplot2::ylim(0,max.sw) + 
    theme(legend.title = element_blank(),#element_text(size=12, color = "thistle4"),
          legend.justification=c(0,1), 
          legend.key.width = unit(1.5, "line"),
          legend.position= c(0, 1),#c(0.05, 0.95),
          legend.background = element_blank(),
          legend.text = element_text(size=8, color = "black", face="bold"),
          legend.key = element_blank())
  
  
  p.mo <- ggplot(data = coefs.this.round,
                 aes(x = Rewiring,
                     y = Modularity,
                     colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) + 
    scale_colour_manual(values = c(color.inter, color.majo,
                                   color.mino, color.whole)) +
    ggplot2::ylim(0,max.mo) + 
    theme(legend.title = element_blank(),#element_text(size=12, color = "thistle4"),
          legend.justification=c(0,1), 
          legend.key.width = unit(1.5, "line"),
          legend.position= c(0, 1),#c(0.05, 0.95),
          legend.background = element_blank(),
          legend.text = element_text(size=8, color = "black", face="bold"),
          legend.key = element_blank())
  
  
  p.ed <- ggplot(data = coefs.this.round,
                 aes(x = Rewiring,
                     y = `Edge Density`,
                     colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) + 
    scale_colour_manual(values = c(color.inter, color.majo,
                                   color.mino, color.whole)) +
    ggplot2::ylim(0,max.de) + 
    theme(legend.title = element_blank(),#element_text(size=12, color = "thistle4"),
          legend.justification=c(0,1), 
          legend.key.width = unit(1.5, "line"),
          legend.position= c(0, 1),#c(0.05, 0.95),
          legend.background = element_blank(),
          legend.text = element_text(size=8, color = "black", face="bold"),
          legend.key = element_blank())
  
  
  p.pl <- ggplot(data = coefs.this.round,
                 aes(x = Rewiring,
                     y = `Avg Path Length`,
                     colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) + 
    scale_colour_manual(values = c(color.inter, color.majo,
                                   color.mino, color.whole)) +
    ggplot2::ylim(0,max.pl) + 
    theme(legend.title = element_blank(),#element_text(size=12, color = "thistle4"),
          legend.justification=c(0,1), 
          legend.key.width = unit(1.5, "line"),
          legend.position= c(0, 1),#c(0.05, 0.95),
          legend.background = element_blank(),
          legend.text = element_text(size=8, color = "black", face="bold"),
          legend.key = element_blank())
  
  
  p.ef <- ggplot(data = coefs.this.round,
                 aes(x = Rewiring,
                     y = Efficiency,
                     colour = Partition)) +
    geom_line(size = .75, alpha = 0.7) + 
    scale_colour_manual(values = c(color.inter, color.majo,
                                   color.mino, color.whole)) +
    ggplot2::ylim(0,max.ef) + 
    theme(legend.title = element_blank(),#element_text(size=12, color = "thistle4"),
          legend.justification=c(0,1), 
          legend.key.width = unit(1.5, "line"),
          legend.position= c(0, 1),#c(0.05, 0.95),
          legend.background = element_blank(),
          legend.text = element_text(size=8, color = "black", face="bold"),
          legend.key = element_blank())
  
  
  figure <- ggarrange(p.cl, p.ed, p.sw, p.pl, p.mo, p.ef,
                      ncol=2, nrow=3,
                      common.legend = F
                      # legend="bottom"
  )
  
  top.text <- textGrob(paste("Evolution of coefficients for", title, "\n"),
                       gp=gpar(fontsize=25,font=8))
  annotate_figure(figure, top = top.text)

  paste0("wb_", title, ".png") %>% ggsave(width = 14,
                                         height = 21,
                                         dpi = "retina")
}
Sys.time() - t3

