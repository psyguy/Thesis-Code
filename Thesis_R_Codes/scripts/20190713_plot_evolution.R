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

all.partitions <- snp$Partition %>% 
  unique() %>% 
  sort() %>% 
  as.character() %>%
  as.list()

all.vd <- snp$Verbal.Description %>% 
  unique() %>% 
  sort() %>% 
  as.character() %>%
  as.list()



system.time(all.owners %>%
              map(extract_plotcoefs.glued,
                  snp = snp)
)

system.time(all.partitions %>%
              map(extract_plotcoefs.glued,
                  snp = snp)
)



path.to.pdfs <- "./figures/netstats-plots_25k"
coef.files <- list.files(path = path.to.pdfs, pattern = "*.pdf")

p <- coef.files[grepl("homo", coef.files)] %>% 
  c(coef.files[grepl("hypo-cha", coef.files)]) %>% 
  c(coef.files[grepl("hyper-cha", coef.files)]) %>% 
  c(coef.files[grepl("hypo-cou", coef.files)]) %>% 
  c(coef.files[grepl("hyper-cou", coef.files)])



staple_pdf(input_files = paste(path.to.pdfs, p, sep = "/"),
           output_filepath = NULL)

?savePlot


extract_plotcoefs.glued(this.Verbal.Description = all.vd[[1]],
                        this.Partition = all.partitions[[1]], 
                        snp=snp)
