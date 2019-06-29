#!/usr/bin/env Rscript

Args <- commandArgs(TRUE)
rounds <- as.numeric(Args[1])

source("./functions/functions_reports.R")


r_ <- (rounds %% 10) + 1

ed_ <- (rounds - 1) %/% 10

pat.tmp <- NULL

if(ed_ == 0) pat.tmp <- "_g-0.3k-5.2k"
if(ed_ == 1) pat.tmp <- "_g-0.3k-4.68k"
if(ed_ == 2) pat.tmp <- "_g-0.3k-5.72k"
if(ed_ == 3) pat.tmp <- "_g-0.3k-6.24k"

pattern <- paste0("r-", r_, pat.tmp)

paste("Now reading round", r_, "of", pat.tmp, "index", rounds) %>% print()

reports_coefs.extract(pattern = pattern)