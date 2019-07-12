rm(list = ls())

r_ <- 1

setwd("I:/Thesis_Codes/Thesis_R_Codes/")
source("./functions/functions_extract.R")

path.to.brains <- "./data/5200-edges"
path.to.snp <- "./data/5200-edges-snapshots"

pat.tmp <- NULL

pat.tmp <- "_g-0.3k-5.2k"

pattern <- paste0("r-", r_, pat.tmp)
pattern <- pat.tmp

paste("Now reading round", r_, "of", pat.tmp) %>% print()


brain_files <- list.files(path = path.to.brains,
                          pattern = "*.RData")

r.this <- brain_files[grepl(pattern, brain_files)]
r.this <- r.this[grepl("_g-", r.this)]
r.this <- r.this[grepl(paste0("_r-", r_), r.this)]
r.this <- r.this[!grepl("coefs.wb.", r.this)]
brain_locations <- path.to.brains %>% 
  paste(r.this, sep = "/")

this.brain_location <- brain_locations#[1]

Sys.time() %>% print()
system.time(snp <- this.brain_location %>%
              ldply(extract_brains,
                    snapshots =  seq(5e3, 100e3, 5e3)
              )
) %>% print()
save_vars(
  list.of.vars = "snp",
  prefix = paste("snp_round-", r_),
  path = path.to.snp
)
