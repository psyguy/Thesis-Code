# rm(list=ls())

library(tidyverse)

hhg.files <- list.files(path = "./data/HHG/")
cn <- c(
  "index.1",
  "index.2",
  "name.1",
  "name.2",
  "sum.chisq",
  "sum.lr",
  "max.chisq",
  "max.lr",
  "perm.pval.hhg.sc",
  "perm.pval.hhg.sl",
  "perm.pval.hhg.mc",
  "perm.pval.hhg.ml",
  "stat.type",
  "n")
df.hhg.con <- df.hhg.act <- data.frame(matrix(ncol = length(cn), nrow = 0))
for(i in hhg.files){
  load(paste0("./data/HHG/",i))
  df.hhg.con <- hhg.con %>% unlist() %>% as.character() %>% rbind(df.hhg.con,.) %>% mutate_all(as.character)
  df.hhg.act <- hhg.act %>% unlist() %>% as.character() %>% rbind(df.hhg.act,.) %>% mutate_all(as.character)
}

colnames(df.hhg.con) <- colnames(df.hhg.act) <- cn

df.h <- df.hhg.con %>% select(index.1:index.2,sum.chisq:perm.pval.hhg.ml) %>% mutate_all(as.numeric)
h.mat <- matrix(nrow = 50, ncol = 50)
for(i in 1:nrow(df.h)){
  h <- df.h[i,]
  h.mat[h$index.1, h$index.2] <- df.hhg.act[i,]$perm.pval.hhg.ml %>% as.numeric() #1 - log(h$sum.chisq)/max(log(df.h$sum.chisq))
  h.mat[h$index.2, h$index.1] <- h$perm.pval.hhg.sl#%>% log()
  }
# diag(h.mat) <- NaN
(1-h.mat) %>% pimage

