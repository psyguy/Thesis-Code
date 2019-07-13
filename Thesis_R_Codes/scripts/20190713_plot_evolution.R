#!/usr/bin/env Rscript

# Args <- commandArgs(TRUE)
# rounds <- as.numeric(Args[1])
rm(list = ls())

source("./functions/functions_extract.R")

load("./data/snp.lean_all_5k_20190713_1356.RData")
snp <- snp.lean
rm(snp.lean)

all.owners <- snp$Owner %>% 
  unique() %>% 
  sort() %>% 
  as.character() %>%
  as.list()


system.time(all.owners %>%
              map(extract_plotcoefs.glued,
                  snp = snp)
            )

