# rm(list = ls())

source("./functions/functions_partition.R")


# Plotting the connectivity matricies and networks at snapshots -----------

extract_partitions <- function(m){
  minority <- m[1:50,1:50]
  majority <- m[51:300,51:300]
  # removing the within group connections
  interpartition <- m
  interpartition[1:50,1:50] <- interpartition[51:300,51:300] <- 0
  m.list <- list(whole = m,
                 minority = minority,
                 majority = majority,
                 interpartition = interpartition)
  m.list %>% return()
}



extract_identifiers <- function(b){
  
  ## debug
  # b <- brain_case
  
  Owner <- b@name #%>% rep(l_)
  Seed <- b@parameters$seed #%>% rep(l_)
  Round <- b@parameters$round
  p_d <- b@parameters$params.eps_a
  Epsilon.Proportion <- paste0("(",p_d[1],", ",p_d[2],", ",p_d[3],")")
  a.Proportion <- paste0("(",p_d[4],", ",p_d[5],", ",p_d[6],")")
  
  vd <- "Homogeneous Society"
  if(Epsilon.Proportion == "(0, 5, 1)") vd <- "Hyper-coupled minority"
  if(Epsilon.Proportion == "(1, 5, 0)") vd <- "Hypo-coupled minority"
  if(a.Proportion == "(0, 5, 1)") vd <- "Hyper-chaotic minority"
  if(a.Proportion == "(1, 5, 0)") vd <- "Hypo-chaotic minority"
  Verbal.Description <- vd
  
  identifiers <- data.frame(Owner,
                            Seed,
                            Round,
                            Rewiring = 0,
                            Verbal.Description,
                            Epsilon.Proportion,
                            a.Proportion)
  identifiers <- lapply(1:4, function(x) identifiers)
  names(identifiers) <- c("whole",
                          "minority",
                          "majority",
                          "interpartition")
  identifiers  %>% return()
}



extract_coefs <- function(m,
                          identifiers = NULL){
  
  g_ <- m %>% graph_from_adjacency_matrix(mode = "undirected")
  
  Clustering <- m %>% my_clustceof()
  Efficiency <- m %>% netmeas_efficiency()
  Small.World <- Clustering*Efficiency
  Modularity <- g_ %>% cluster_fast_greedy() %>% modularity()
  Assortativity <- g_ %>% assortativity.degree() %>% as.numeric()
  Rich.Club <- brainGraph::rich_club_coeff(g_)$phi %>% as.numeric()
  Average.Path.Length <- g_ %>% average.path.length(unconnected = TRUE)
  Edge.Density <- m %>% sum() %>% sum()
  
  coefs <- data.frame(Clustering, 
                      Small.World,
                      Modularity,
                      Assortativity,
                      Rich.Club,
                      Average.Path.Length,
                      Edge.Density,
                      Efficiency
  )
  
  if(is.data.frame(identifiers)) coefs <- cbind(identifiers[1,],
                                                coefs)
  
  coefs %>% return()
}



extract_id.n.mats <- function(m_raw, identifiers){
  
  m_partitions <- m_raw %>%
    extract_partitions()
  coefs_partitioned <- m_partitions %>%
    map(extract_coefs)
  
  output <- identifiers %>%
    map2(coefs_partitioned, cbind) %>% 
    bind_rows(.id = "Partition") %>% 
    mutate(adj.mat.vect = map(m_partitions, mat2vec))
  
  # normalizing the edge densities
  denom <- c(300, 50, 250)
  denom <- (denom*(denom-1)/2) %>% c(50*250)
  output$Edge.Density <- output$Edge.Density / rep(denom, nrow(output)/4)
  output %>% return()
}


extract_brains <- function(b_loc,
                           snapshots = c(50e3, 100e3)){
  ## debug
  # b_loc <- this.brain_location
  # snapshots <- 100e3
  
  library(purrr)
  
  load(b_loc)
  age_ <- brain_case@age$rewires - 1
  
  brain_case@name %>% paste("being extracted at age", age_) %>% print()
  
  l.m <- brain_case@history$mat.connectivity
  l.m[1] <- NULL
  l.m.compact <- l.m %>% compact()
  
  identifiers <- brain_case %>% extract_identifiers()
  
  output <- l.m.compact[snapshots/200] %>%
    ldply(extract_id.n.mats, identifiers)
  
  output$Rewiring <- rep((age_ - 100e3 + snapshots), each = 4)
  
  output %>% return()
}


extract_plotnet <- function(m,
                            title = "Network",
                            colors = list(
                              bg = "white",
                              mino = "deepskyblue3", #"blue2"
                              majo = "orangered", #"brown3"
                              inter = "olivedrab2", #"green4"
                              whole = "azure4"),
                            save = TRUE,
                            curve = 0,
                            path.fig = "figures"
                            ){
  if(is.vector(m)) m <- m %>% vec2mat()
  pf <- path.fig
  if(substr(pf, nchar(pf), nchar(pf))!="/") path.fig <- paste0(path.fig, "/")
  
  g <- m %>% graph_from_adjacency_matrix(mode="undirected")
  
  V(g)$partition <- c(rep("minority", 50),
                      rep("majority", 250))
  
  
  #Set the margin size (small margins)
  par(mar = rep(0.05, 4))
  vertex.color <- c("skyblue", "salmon")[1 + (V(g)$partition == "majority")]
  vertex.size <- 2
  ## select edges and set color and plot one by one
  # First remove them, then add majority, plot majority 
  E(g)$color <- NA
  E(g)[V(g)[partition == "majority"] %--% V(g)[partition == "majority"]]$color <- colors$majo
  set.seed(1)
  g %>% plot(vertex.size = vertex.size,
             vertex.color = vertex.color,
             add=FALSE,
             vertex.label = NA,
             edge.width = 1,
             edge.curved= curve)
  
  E(g)$color <- NA
  E(g)[V(g)[partition == "minority"] %--% V(g)[partition == "majority"]]$color <- colors$inter
  set.seed(1)
  g %>% plot(vertex.size = vertex.size,
             vertex.color = vertex.color,
             add=TRUE,
             vertex.label = NA,
             edge.width = 1,
             edge.curved= curve)
  
  
  E(g)$color <- NA
  E(g)[V(g)[partition == "minority"] %--% V(g)[partition == "minority"]]$color <- colors$mino
  set.seed(1)
  g %>% plot(vertex.size = vertex.size,
             vertex.color = vertex.color,
             add=TRUE,
             vertex.label = NA,
             edge.width = 1,
             edge.curved= curve)
  
  if(save) graph2pdf(height = 10, width = 10,
                     file = paste0(path.fig, title, "_network"))
}


extract_plotcon <- function(m,
                            title = "Connectivity",
                            colors = list(
                              bg = "white",
                              mino = "deepskyblue3", #"blue2"
                              majo = "orangered", #"brown3"
                              inter = "olivedrab2", #"green4"
                              whole = "dimgray"),
                            save = TRUE,
                            path.fig = "figures"){
  if(is.vector(m)) m <- m %>% vec2mat
  pf <- path.fig
  if(substr(pf, nchar(pf), nchar(pf))!="/") path.fig <- paste0(path.fig, "/")
  file.name <- paste0(path.fig, title)
  
  s <- m %>% seriate()
  m[1:50,1:50] <- m[1:50,1:50]*3
  m[1:50,51:300] <- m[1:50,51:300]*2
  m[51:300,1:50] <- m[51:300,1:50]*2
  
  col.pimage <- c(colors$bg, colors$majo,
                  colors$inter, colors$mino)

  pimage(m,
         col = col.pimage,
         key = FALSE)
  
  if(save) graph2pdf(height = 5, width = 5,
                     file = paste0(path.fig, title, "_unserialized"))
  
  pimage(m,
         s,
         col = col.pimage,
         key = FALSE)
  
  if(save) graph2pdf(height = 5, width = 5,
                     file = paste0(path.fig, title, "_serialized"))
  
}


extract_plotglue <- function(title = "Someone",
                             path.fig = "figures"){
  pf <- path.fig
  if(substr(pf, nchar(pf), nchar(pf))!="/") path.fig <- paste0(path.fig, "/")
  file.name <- paste0(path.fig, title)
  
  panel.upper.l <- file.name %>% paste0("_unserialized.pdf") %>% image_read_pdf()
  panel.upper.r <- file.name %>% paste0("_serialized.pdf") %>% image_read_pdf()
  panel.lower <- file.name %>% paste0("_network.pdf") %>% image_read_pdf()
  
  whole <- c(panel.upper.l, panel.upper.r) %>% 
    image_append() %>% 
    c(panel.lower) %>% 
    image_append(stack = TRUE)
  
  image_write(whole,"a.pdf")
  
  graph2pdf(whole,
            height = 15, width = 10,
            file = paste0("Profile of", title))
  
}


extract_plotcoefs.single <- function(chosen.coef,
                                     name.this.owner,
                                     snp,
                                     colors = list(
                                       bg = "white",
                                       mino = "deepskyblue3",
                                       majo = "orangered",
                                       inter = "olivedrab2",
                                       whole = "dimgray")){
  
  p <- ggplot(data = snp %>% filter(Owner == name.this.owner),
              aes(x = Rewiring,
                  y = !!ensym(chosen.coef),
                  colour = Partition)) +
    geom_line(size = 1.5, alpha = 0.8) +
    scale_colour_manual(values = c(colors$inter, colors$majo,
                                   colors$mino, colors$whole)) +
    ggplot2::ylim(min(0, min(snp[chosen.coef])),
                  max(snp[chosen.coef]))
  p %>% return()
}


extract_plotcoefs.glued <- function(name.this.owner,
                                    snp,
                                    path.fig = "figures"){
  coef.names <- names(snp)[9:16] %>% as.list()
  list.of.plots <- coef.names %>% 
    llply(extract_plotcoefs.single,
          name.this.owner = name.this.owner, snp = snp)
  
  figure <- ggarrange(plotlist = list.of.plots,
                      ncol = 2, nrow = 4,
                      common.legend = TRUE,
                      legend = "bottom")
  
  title <- paste0("Network statistics of ",
                  name.this.owner,
                  " (", tolower(snp$Verbal.Description[1]), ")")
  
  pf <- path.fig
  if(substr(pf, nchar(pf), nchar(pf))!="/") path.fig <- paste0(path.fig, "/")
  file.name <- paste0(path.fig, title)
  
  
  annotate_figure(figure,
                  top = text_grob(label =  paste0("",
                                                  "Network statistics of ",
                                                  name.this.owner,
                                                  "\n (",
                                                  tolower(snp$Verbal.Description[1]),
                                                  ")"),
                                  size = 25, family = "Times"
                                  )
                  ) %>%
    graph2pdf(height = 15, width = 15,
              file = file.name
              )
  
}




