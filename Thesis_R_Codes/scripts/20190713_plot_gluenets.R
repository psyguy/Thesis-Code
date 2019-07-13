#!/usr/bin/env Rscript

# Args <- commandArgs(TRUE)
# rounds <- as.numeric(Args[1])
rm(list = ls())

source("./functions/functions_extract.R")

path.to.pdfs <- "./figures/pdfs_1-2/"

con.files <- list.files(path = path.to.pdfs, pattern = "*.pdf")

i <- 1

this.con.files <- con.files[(3*(i-1)+1):(3*i)]



load("./data/snp.lean_all_5k_20190713_1356.RData")
snp <- snp.lean
rm(snp.lean)

all.owners <- snp$Owner %>% 
  unique() %>% 
  sort() %>% 
  as.character() %>%
  as.list()

l <- paste0(path.to.pdfs, this.con.files) %>% 
  as.list() %>% 
  map(~image_read_pdf(.x))

whole <- c(l[[3]], l[[2]]) %>% 
    image_append() %>% 
    c(l[[1]]) %>% 
    image_append(stack = TRUE)

graph2pdf()

library(pdftools)
pdftools::pdf_combine()

library(grImport)
grImport

staple_pdf("./figures/Network statistics by 25k - 5200 edges")
