list.of.packages <- c("tidyverse", "dplyr", "plyr",
                      "Hmisc", "igraph")
function.path <- "./functions/"
library(plyr)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)}
l_ply(list.of.packages, require, character.only = TRUE)

my.scripts <- list.files(path = function.path, pattern = "*.R")
my.scripts <- my.scripts[my.scripts!="my.library.loader.R"]
l_ply(paste0(function.path,my.scripts), source)

rm(function.path,list.of.packages,my.scripts, new.packages)
