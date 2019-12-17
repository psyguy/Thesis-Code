# What has been so far used (dis)similarity with sloppiness. Here I am correcting the
# definitions and cleaning the code. Moreover, I will plot resemblences/differentiations
# as networks
rm(list=ls())
load("./data/signature-and-HHG_20191202_1704.RData")
load("./data/HHG-unscaled_20191216_1510.RData")
source("./scripts/20191202_family-comparison-heatmaps.R")

library(plyr)
library(viridis)
col.pallett <- inferno(10,1,0,1,-1)


# functions used for plotting heatmaps ------------------------------------

sim.hm <- function(m, title = "", col = col.pallett){
  f <- families
  m <- m %>% as.matrix() %>% unname()
  Heatmap(m,
          col = col,
          column_title = title,
          name = "Similarities",
          na_col = "black",
          cluster_rows = FALSE,
          cluster_columns = FALSE,
          row_split = factor(f, levels = unique(f)),
          column_split = factor(f, levels = unique(f)),
          row_gap = unit(0.5, "mm"), column_gap = unit(0.5, "mm"), border = TRUE)
  }

f.corrplot <- function(m,
                       type = "lower",
                       col = col.pallett) corrplot(m,
                                                   method = "shade",
                                                   type = type,
                                                   is.corr = T,
                                                   col = rep(col,2),
                                                   addCoef.col = col.pallett[1],
                                                   diag = TRUE,
                                                   cl.pos = "n",
                                                   tl.pos = "n",
                                                   number.cex = .6)

# making Sim/Diff matrices ------------------------------------------------

hhg.act <- df.hhg.act %>% make.hhg.mat()
hhg.con <- df.hhg.con %>% make.hhg.mat()
l.Similarity <- list(`Anatomical HHG similarity` = 1 - hhg.con,
                     `Anatomical NetSimile similarity` = 1 - s.con,
                     `Functional HHG similarity` = 1 - hhg.act,
                     `Functional NetSimile similarity` = 1 - s.act
                     )

l.Similarities.halved <- list(`Anatomical` = make.hhg.s(l.Similarity$`Anatomical HHG similarity`,
                                                          l.Similarity$`Anatomical NetSimile similarity`),
                              `Functional` = make.hhg.s(l.Similarity$`Functional HHG similarity`,
                                                      l.Similarity$`Functional NetSimile similarity`)
                              )
l.Resemblance <- l.Similarity %>%
  map(~make.block.mean(make.halved.mats(.,.,TRUE), TRUE))

for(l in l.Resemblance) rownames(l) <- colnames(l) <- unique(families)

Differentiation <- l.Resemblance %>% 
  ldply(function(l){
    sm <- NULL
    for(i in 1:5){
      # sm <- 
      x <- l[i,]
      sm[i] <- 1 - x[i]/mean(x[-i])
    }
    names(sm) <- families %>% unique()
    sm %>% return()
  }
  )

g.d <- Differentiation %>% gather("Family", "Differentiation", -.id)

g.d$Family <- factor(g.d$Family, levels = unique(g.d$Family))

colnames(g.d)[1] <- "Resemblance measure"


# Plotting Similarity/Resemblance/Differentiation -------------------------

g.d %>% ggplot(aes(x = `Resemblance measure`,
                   y = Differentiation,
                   fill = Family),
               title = "avg") +
  ylim(-.5,.5) +
  geom_bar(position = "dodge", size = 1, width = 0.5, stat="identity")


# Plotting graphs ---------------------------------------------------------

differ <- Differentiation %>%
  filter(.id == "Anatomical HHG similarity") %>% 
  select(-.id)
resemb <- l.Resemblance[[1]]
