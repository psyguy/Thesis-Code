

m <- snp$adj.mat.vect[1600-0] %>% vec2mat()
g_ <- m %>% graph_from_adjacency_matrix(mode = "undirected")

k <- c(1:75)

rc <- sapply(k, function(x) brainGraph::rich_club_coeff(g_, k=x)$phi %>% as.numeric())

plot(k, rc, type = "l", main = "interpartition")


# PLOTTING BETWEENNESS ----------------------------------------------------

last.snapshot <- snp %>% filter(Rewiring == 1e+5)

colors = list(
  bg = "white",
  mino = "deepskyblue3", #"blue2"
  majo = "orangered", #"brown3"
  inter = "olivedrab2", #"green4"
  whole = "azure4")

bt <- function(v, edge_betweenness = FALSE){
  g <- v %>%
  vec2mat() %>% 
  graph_from_adjacency_matrix()
  
  if(edge_betweenness) o <- g %>% 
  edge_betweenness(directed = FALSE)
  else o <- g %>% 
      betweenness(directed = FALSE, normalized = FALSE)
  
  o %>% return()
  
  # oo <- o
  # # if(length(o) < 44850) oo <- o %>% 
  # #   c(rep(NA, 44850 - length(o)))
  # # tibble(oo) %>%
  #   return()
  }

rc <- function(v, k = c(1:150)){
  g_ <- v %>% 
  vec2mat() %>% 
  graph_from_adjacency_matrix()
  
  o <- 
    sapply(k, function(x) brainGraph::rich_club_coeff(g_,
                                                    k=x)$phi %>% as.numeric())
  
  o %>% return()
  
  # oo <- o
  # # if(length(o) < 44850) oo <- o %>% 
  # #   c(rep(NA, 44850 - length(o)))
  # # tibble(oo) %>%
  #   return()
  }

# cd <- function(v, k = c(1:150)){
#   g_ <- v %>% 
#     vec2mat() %>% 
#     graph_from_adjacency_matrix()
#   
#   o <- centralization.degree(g_)$res   
#   o %>% return
# }

name.this.owner <- "Sam Evrard"

system.time(
tmp <- snp %>%
  filter(Owner == name.this.owner) %>% 
  filter(Rewiring == 1e+5) %>% 
  mutate(`Vertex Betweenness` = map(adj.mat.vect, bt,
                   edge_betweenness = FALSE)) %>%
  mutate(`Edge Betweenness` = map(adj.mat.vect, bt,
                   edge_betweenness = TRUE)) %>%
  mutate(`Centrality` = map(adj.mat.vect, rc)) %>%
  mutate(`Rich Club` = map(adj.mat.vect, rc))
)





Rich.Club.150 <- tmp %>%
  make.df("Rich Club") %>%
  ggplot(aes(x = `Club Size`,
             y = `Rich Club`,
             colour = Partition)) +
  geom_line(size = 1.5, alpha = 0.8) +
  scale_colour_manual(values = c(colors$inter, colors$majo,
                               colors$mino, colors$whole)) +
  # scale_colour_manual(values = c(colors$inter, colors$majo,
  #                                colors$mino, colors$whole)) +
  theme(legend.position = "none") +
  ggplot2::xlim(0, 150) +
  ggplot2::ylim(0, 1)

`Vertex Betweenness` <- tmp %>%
  make.df("Vertex Betweenness") %>%
  ggplot(aes(`Vertex Betweenness`)) +
  geom_density(aes(fill = Partition), alpha = 0.6) +
  scale_fill_manual(values = c(colors$inter, colors$majo,
                               colors$mino, colors$whole)) +
  theme(legend.position = "none") +
  ggplot2::xlim(0, 1000)


`Edge Betweenness` <- tmp %>%
  make.df("Edge Betweenness") %>%
  ggplot(aes(`Edge Betweenness`)) +
  geom_density(aes(fill = Partition), alpha = 0.6) +
  scale_fill_manual(values = c(colors$inter, colors$majo,
                               colors$mino, colors$whole)) +
  theme(legend.position = "none") +
  ggplot2::xlim(0, 150)


figure <- ggarrange(Rich.Club.150,
                    `Vertex Betweenness`,
                    `Edge Betweenness`,
                    ncol = 1, nrow = 3,
                    common.legend = TRUE,
                    legend = "bottom"#ifelse(is.null(name.this.owner),
                                    # "bottom", "bottom")#"none", "bottom")
)

vd <- snp %>%
  filter(Owner == name.this.owner) %>%
  pull(Verbal.Description) %>% 
  as.character()

pf <- path.fig <- "."
if(substr(pf, nchar(pf), nchar(pf))!="/") path.fig <- paste0(path.fig, "/")
file.name <- paste0(path.fig, title)

annotate_figure(figure,
                top = text_grob(label =  paste0("",
                                                "Final Rich Club and Betweenness values of",
                                                "\n",
                                                name.this.owner,
                                                " (",
                                                tolower(vd[1]),
                                                ")"
                                              ),
                                size = 25,
                                family = "Times"
                                )
                ) %>%
  graph2pdf(height = 15, width = 15,
            file = paste0(file.name,".pdf")
  )


# Marginal density plot of y (right panel) 
ydensity <- ggplot(df2, aes(y)) + geom_density(aes(fill=group), alpha=.8) + scale_fill_manual(values = my3cols) + theme(legend.position = "none") + coord_flip()

blankPlot <- ggplot()+geom_blank()+ theme_void()

require("gridExtra") 
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
