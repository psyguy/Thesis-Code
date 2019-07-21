

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

bt <- function(v){
  o <- v %>%
  vec2mat() %>% 
  graph_from_adjacency_matrix() %>% 
  edge_betweenness(directed = FALSE)
  
  oo <- o
  # if(length(o) < 44850) oo <- o %>% 
  #   c(rep(NA, 44850 - length(o)))
  tibble(oo) %>%
    return()
  }

rc <- function(v, k = c(1:150)){
  g_ <- v %>% 
  vec2mat() %>% 
  graph_from_adjacency_matrix()
  
  o <- 
    sapply(k, function(x) brainGraph::rich_club_coeff(g_,
                                                    k=x)$phi %>% as.numeric())
  oo <- o
  # if(length(o) < 44850) oo <- o %>% 
  #   c(rep(NA, 44850 - length(o)))
  tibble(oo) %>%
    return()
  }

tmp <- snp %>%
  filter(Rewiring == 1e+5) %>% 
  # mutate(bet = map(adj.mat.vect, bt)) %>% 
  mutate(rc = map(adj.mat.vect, rc))


tmp.unnested <- tmp %>% unnest()


my3cols <- c("#E7B800", "#2E9FDF", "#FC4E07")
set.seed(1234)
x <- c(rnorm(350, mean = -1), rnorm(350, mean = 1.5), rnorm(350, mean = 4))
y <- c(rnorm(350, mean = -0.5), rnorm(350, mean = 1.7), rnorm(350, mean = 2.5))
group <- as.factor(rep(c(1, 2, 3), each = 350))
df2 <- data.frame(x, y, group) 
head(df2) 
# Scatter plot of x and y variables and color by groups 
scatterPlot <-
  ggplot(df2,
         aes(x, y)) + geom_point(aes(color = group)) + scale_color_manual(values = my3cols) + theme(legend.position=c(0,1), legend.justification=c(0,1))
# Marginal density plot of x (top panel) 
xdensity <- ggplot(df2, aes(x)) + geom_density(aes(fill = group), alpha=.8) + scale_fill_manual(values = my3cols) + theme(legend.position = "none")
# Marginal density plot of y (right panel) 
ydensity <- ggplot(df2, aes(y)) + geom_density(aes(fill=group), alpha=.8) + scale_fill_manual(values = my3cols) + theme(legend.position = "none") + coord_flip()

blankPlot <- ggplot()+geom_blank()+ theme_void()

require("gridExtra") 
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
