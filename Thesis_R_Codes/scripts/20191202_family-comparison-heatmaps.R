rm(list=ls())

library(tidyverse)
library(ComplexHeatmap)
library(seriation)


load("./data/signature-and-HHG_20191202_1704.RData")

families <<- df.hhg.act %>%
  mutate(index = as.numeric(index.1)) %>% 
  arrange(index) %>%
  pull(name.1) %>%
  unique() %>% 
  gsub(" .*", "", .)

make.hhg.mat <- function(df, hhg.value = "perm.pval.hhg.ml"){
  df.h <- df %>% mutate_at(c("index.1","index.2"),as.numeric)
  h.mat <- matrix(nrow = 50, ncol = 50)
  for(i in 1:nrow(df.h)){
    h <- df.h[i,]
    h.mat[h$index.1, h$index.2] <- h.mat[h$index.2, h$index.1] <- h %>%
      select(hhg.value) %>% as.numeric()
  }
  h.mat %>% return()
}

make.halved.mats <- function(m.lower, m.upper, standardize = FALSE){
  m.lower[upper.tri(m.lower)] <- 0
  m.upper[lower.tri(m.upper)] <- 0
  o <- m.lower + m.upper
  if(standardize) o <- m.lower/max(m.lower, na.rm = TRUE) + m.upper/max(m.upper, na.rm = TRUE)
  diag(o) <- NA
  o %>% return()
}

make.block.mean <- function(m){
  o <- matrix(nrow = 50, ncol = 50) 
  for(i in 1:5){
    for(j in 1:5){
      o[(10*i-9):(i*10),(10*j-9):(j*10)] <- mean(m[(10*i-9):(i*10),(10*j-9):(j*10)], na.rm = TRUE)
    }
  }
  o %>% return()
}

make.together <- function(m){
  m <- make.halved.mats(m, m, TRUE)
  m %>% make.block.mean() %>% make.halved.mats(m,.) %>% return()
}

hhg.act <- df.hhg.act %>% make.hhg.mat()
hhg.con <- df.hhg.con %>% make.hhg.mat()

l.family.sim <- list(`Connectivity HHG test` = hhg.con,
                     `Connectivity NetSimile distances` = s.con,
                     `Connectivity HHG test` = hhg.act,
                     `Connectivity NetSimile distances` = s.act
                     ) %>% map(make.together)

library(circlize)
hm <- function(m){
  Heatmap(m,
        # col = colorRamp2(c(0, 0.05, 1),c("orangered", "azure4", "white")),
        col = c("white", "deepskyblue3"),
        name = deparse(quote(m)),
        na_col = "black",
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        row_split = factor(families, levels = unique(families)),
        column_split = factor(families, levels = unique(families)),
        row_gap = unit(0.5, "mm"), column_gap = unit(0.5, "mm"), border = TRUE)
}

l.family.sim[1] %>% l_ply(hm)
## trial here, do not run
## .euc. distances yield less significant distinctions, hence use canberra
# 
# hhg.act %>% make.together()
# s.euc.act %>% make.together()
# s.act %>% make.together()
# 
# hhg.con %>% make.together() 
# s.euc.con %>% make.together()
# s.con %>% make.together()
# 
# 
# s.euc.act %>% make.halved.mats(s.act) %>%  pimage
# s.euc.con %>% make.halved.mats(s.con) %>%  pimage
# s.euc.act %>% make.block.mean() %>% make.halved.mats(make.block.mean(s.act)) %>% pimage
# s.euc.con %>% make.block.mean() %>% make.halved.mats(make.block.mean(s.con)) %>% pimage
# hhg.con %>% make.block.mean() %>% make.halved.mats((hhg.act)) %>% pimage





