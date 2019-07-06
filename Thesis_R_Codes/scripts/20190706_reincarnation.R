#!/usr/bin/env Rscript
# rm(list = ls())

Args <- commandArgs(TRUE)
rounds <- as.numeric(Args[1])


rounds <- 34
source("./functions/functions_reports.R")
options(bitmapType='cairo')

r_ <- (rounds %% 10) + 1

ed_ <- (rounds) %/% 50 + 1

pat.tmp <- NULL

pat.tmp <- "_g-0.3k-5.2k"

pattern <- paste0("r-", r_, pat.tmp)


sampled.path <- "./data/gen-1_5200-edges/"
sampled.names <- list.files(path = sampled.path, pattern = "*.RData")

names <- sampled.names %>%
  my_gsub(paste0(".*",
                 pat.tmp,
                 "_"), "") %>% 
  my_gsub("\\_.*", "") %>% 
  sort()

this.owner.oldest <- sampled.names[grepl(names[r_], sampled.names)] %>% 
  sort() %>% 
  tail(1)

current.life <- this.owner.oldest %>% substring(6,7) %>% as.numeric()

  
t <- Sys.time()
# for(sampled in r.this){
  
  life.prefix <- sprintf("life-%02d", (current.life+1))
  
  load(paste(sampled.path, this.owner.oldest, sep = ""))

  ## rebirth
  brain_case@initial <- brain_case@now
  # making initial$mat.con a list (standard form)
  brain_case@initial$mat.connectivity <- brain_case@now$mat.connectivity %>% list()
  # removing the long history of previous life
  brain_case@history <- brain_case@initial
  
  brain_case <- partition_culture(brain_case = brain_case,
                                  final.age = 102)
  
  
  reports_netviz(brain_case)
  
  
  save_vars(list.of.vars = "brain_case",
            prefix = paste(life.prefix,
                           brain_case@parameters$brain.code,
                           brain_case@name,
                           sep = "_"),
            path = sampled.path
            )
  
# }

paste("Netviz of round", r_,
      "of", pat.tmp,
      "index", rounds,
      "took", (Sys.time()-t)) %>% print()
