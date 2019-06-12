# Intro -------------------------------------------------------------------

## This script keeps all functions needed for GongvLeeuwen2004.
## It's standalone, doesn't need to call any other script

# loading packages --------------------------------------------------------

list.of.packages <- c("tidyverse",
                      "dplyr",
                      "plyr",
                      "corrplot",
                      "Hmisc",
                      "seriation",
                      "igraph")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  install.packages(new.packages)
}
tmp <- lapply(list.of.packages, require, character.only = TRUE)

rm(list.of.packages, new.packages, tmp)


# mini functions ----------------------------------------------------------

mini_logistic <- function(x, a = 1.7) {
  1 - a * (x ^ 2)
}

# saves a backup of variables ---------------------------------------------

save_vars <- function(list.of.vars = NULL,
                      prefix = "StatusQuo",
                      path = "data") {
  mine_gsub <- function(x, pat, rep)
    gsub(pat, rep, x)
  
  # if(is.null(list.of.vars)) list.of.vars <- ls() # setdiff(ls(), lsf.str())
  
  date_time <- Sys.time() %>%
    mine_gsub(" ", "_") %>%
    mine_gsub("-", "") %>%
    mine_gsub(":", "") %>%
    strtrim(13) # getting rid of timezone, etc.
  
  if (!is.null(path))
    path <- paste0(path, "/")
  file_name <- paste0(path, prefix, "_", date_time, ".RData")
  
  save(list = list.of.vars, file = file_name)
}


# to make random graphs of certain size and #links -------------------------

make_random_graph <- function(size = 5,
                              num.links = 10,
                              distribution = "binary",
                              parameters = c(0, 1),
                              seed = rnorm(1)) {
  set.seed(seed)
  edges <- rep(0, size * (size - 1) / 2)
  
  if (distribution == "binary") {
    ones <- sample.int(length(edges), num.links)
    edges[ones] <- 1
  }
  
  g <- matrix(0, size, size)
  g[base::lower.tri(g, diag = FALSE)] <- edges
  
  (adj.matrix <- g + t(g)) %>% return()
}


# swapping the desired edges ----------------------------------------------

swap_edges <- function(connectivity.matrix,
                       from.1,
                       to.1,
                       from.2,
                       to.2) {
  edge.1 <- connectivity.matrix[from.1, to.1]
  edge.2 <- connectivity.matrix[from.2, to.2]
  
  connectivity.matrix[from.1, to.1] <- edge.2
  connectivity.matrix[to.1, from.1] <- edge.2
  
  connectivity.matrix[from.2, to.2] <- edge.1
  connectivity.matrix[to.2, from.2] <- edge.1
  
  connectivity.matrix %>% return()
  
}

# my small rewiring function ----------------------------------------------

my_rewire <- function(x.input, conn) {
  distances <- x.input %>% tail(1) %>% my_coherenceD()
  
  i_ <- sample.int(ncol(conn), 1)
  d_ <- distances[, i_]
  
  j_1 <- which.min(d_)
  j_2 <- which.max(d_)
  
  # if(!conn[i_,j_1]) conn %>% return()
  conn <- conn %>% swap_edges(
    from.1 = i_,
    to.1 = j_1,
    from.2 = i_,
    to.2 = j_2
  )
  conn %>% return()
  
}


# coherence D -------------------------------------------------------------

my_coherenceD <- function(x.input) {
  x.duplicated <- rep(1, ncol(x.input)) %*% x.input
  d <- (t(x.duplicated) - x.duplicated) %>% abs()
  diag(d) <- NA
  d %>% return()
  
}


# clustering coeeficient (transitivity) -----------------------------------

my_clustceof <- function(m) {
  tr <- function(m)
    sum(diag(m), na.rm = TRUE)
  
  m <- m %>% as.matrix()
  if (dim(m)[1] != dim(m)[2])
    stop("m must be a square matrix")
  
  m2 <- m %*% m
  m3 <- m2 %*% m
  
  tr(m3) / (sum(m2) - tr(m2))
}
