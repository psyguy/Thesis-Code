






# plots graphs with colored edges -----------------------------------------


g <- r3.red@now$mat.connectivity %>% graph_from_adjacency_matrix(mode="undirected")

V(g)$partition <- c(rep("minority", 50),
                    rep("majority", 250))




# select edges and set color 
E(g)[V(g)[partition == "minority"] %--% V(g)[partition == "minority"]]$color <- "blue2" # within.mino
E(g)[V(g)[partition == "minority"] %--% V(g)[partition == "majority"]]$color <- "darkorchid1" # between.minomajo
E(g)[V(g)[partition == "majority"] %--% V(g)[partition == "majority"]]$color <- "brown3" # within.majo
# plot
set.seed(1)
g %>% plot(vertex.size = 0,
           vertex.color = c("skyblue", "red")[1 + (V(g)$partition == "majority")],
           vertex.label = NA,
           edge.width = 1,
           edge.curved= 0.5,
           main = "red")
