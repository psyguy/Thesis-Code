source("./scripts/20191202_family-comparison-heatmaps.R")

library(viridis)

hm <- function(m, title = ""){
  f <- families
  if(nrow(m)==5) f <- families %>% unique()
  m <- m %>% as.matrix() %>% unname()
  Heatmap(m,
          col = inferno(10,1,0,1,-1),
          #colorRamp2(c(0, 1),c("white", "azure4")),
          # col = (rainbow(100, start = 0, end = 1)),
          column_title = title,
          name = "Dissimilarity",
          na_col = "white",
          cluster_rows = FALSE,
          cluster_columns = FALSE,
          # column_names_rot = -45, column_names_side = "top", right_annotation = ha,
          row_split = factor(f, levels = unique(f)),
          column_split = factor(f, levels = unique(f)),
          row_gap = unit(0.5, "mm"), column_gap = unit(0.5, "mm"), border = TRUE)

}


l.family.halved[[1]] %>% hm("ss")

l <- l.avg[[1]]
rownames(l) <- colnames(l) <- unique(families)


(l) %>% corrplot(method = "shade",
                 type = "lower",
                 # title = "\n Connectivity HHG test \n",
                 is.corr = T,
                 col = rep(inferno(10,1,0,1,-1),2),
                 # addgrid.col = NA,
                 # addCoefasPercent = T,
                 addCoef.col = "black",
                 # tl.pos = "r",
                 tl.col = "black",
                 number.cex = .6) %>% png()
