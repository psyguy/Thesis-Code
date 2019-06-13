# Intro -------------------------------------------------------------------

## This script keeps all functions needed for GongvLeeuwen2004.
## It's standalone and loads functions_my automatically

# rm(list = ls())


# loading the basic functions and packages --------------------------------

if(!exists("path.to.functions_my")) path.to.functions_my <- "functions"
source(paste0(path.to.functions_my,"/functions_my.R"))
source(paste0(path.to.functions_my,"/functions_heartbeat.R"))


# a unified function to grow (update+rewire) ------------------------------

trial_grow <- function(
                       parameters =  list(n_nodes = 100,
                                          n_edges = 0,
                                          eps = 0.2,
                                          seed = -99),
                       n_updates = 1, # number of heartupdates per rewireupdates of notes
                       n_rewires = 1, # number of rewirings of the network
                       freq_snapshot = 200, # frequency of saving connectivity matrices in the brain
                       brain_younger = NULL,
                       save_brain = FALSE,
                       name = NULL,
                       quiet = FALSE){
  
  
  brain_growing <- brain_younger

  
# making a brain if there is no younger brain to continue growing ---------

  if (is.null(brain_growing)) {
    
    if(!parameters$n_edges){
      parameters$n_edges <- round(1.5 * 2 * log(parameters$n_nodes) * (parameters$n_nodes - 1))
    }
    if(parameters$seed == 0 | parameters$seed == -99){
      if(parameters$seed == -99) set.seed(-99)
      parameters$seed <- 1000 * round(rnorm(1),3) %>% abs()
    }
    
    m <- make_random_graph(size = parameters$n_nodes,
                           num.links = parameters$n_edges,
                           seed = parameters$seed)
    a <- parameters$n_nodes %>% runif(0, 1) %>% t()
    
    cl <-  m %>% my_clustceof()
    
    l <- list(
      activities = a,
      mat.connectivity = list(m),
      coef.clustering = cl
    )
    
    # giving the brain a name if not already specified
    if(is.null(name)) name <- give_name(num = 1, seed = parameters$seed)
    
    brain_growing <- new(
      "brain",
      name = name,
      birthday = as.character(Sys.time()),
      age = list(updates = 1, rewires = 1),
      starting_values = l,
      parameters = parameters,
      history = l,
      now = l
    )
  }
  
# writing the update and rewire again together here -----------------------


  # extracting values of now
  m.l <- brain_growing@now$mat.connectivity -> now.m
  if(is.list(m.l)) now.m <- m.l[[length(m.l)]]
  now.a <- brain_growing@now$activities
  now.c <- brain_growing@now$coef.clustering
  eps <- brain_growing@parameters$eps

  now.updates <- brain_growing@age$updates
  now.rewires <- brain_growing@age$rewires
  
    
  # extracting historical values
  his.a <- brain_growing@history$activities
  his.c <- brain_growing@history$coef.clustering
  his.m <- brain_growing@history$mat.connectivity

  
  time_start <- Sys.time()
  new.a <- now.a
  new.m <- now.m
  new.updates <- now.updates
  new.rewires <- now.rewires
  
  for(r in 1:n_rewires) {
    # updating nodes for n_update times
    for(u in 1:n_updates) {
      new.a <- trial_logistic(new.a, new.m, eps)
    }
    
    # incrementing age
    new.updates <- new.updates + n_updates
    new.rewires <- new.rewires + 1
    
    # rewiring and new clustering coefficient
    new.m <- my_rewire(new.a, new.m)
    new.c <- new.m %>% my_clustceof()
    
    # adding activities and clustering coefficient to the history
    his.a <- his.a %>% rbind(new.a)
    his.c <- his.c %>% c(new.c)
    
    # saving the snapshot
    if(!(new.rewires %% freq_snapshot)) his.m[[new.rewires]] <- new.m
    
    if(!quiet & !(new.rewires %% 100)) print(paste(brain_growing@name, "is now",
                           # new.updates, "updates and",
                           new.rewires, "rewires old."))
  }
  
  # updating the brain
  ## age
  brain_growing@age$updates <- new.updates
  brain_growing@age$rewires <- new.rewires
  
  ## history
  brain_growing@history$activities <- his.a
  brain_growing@history$mat.connectivity <- his.m
  brain_growing@history$coef.clustering <- his.c
  
  ## now
  brain_growing@now$activities <- new.a
  brain_growing@now$mat.connectivity <- new.m
  brain_growing@now$coef.clustering <- new.c
  
  (time_taken <- Sys.time() - time_start) %>% paste("for", brain_growing@name) %>%
    print()

  if(save_brain) save_vars("brain_growing", prefix = paste0("vat_", brain_growing@name))
  
  brain_growing %>% return()
  
}


# summary of aged brains --------------------------------------------------

trial_summary <- function(aged_brain){
  
  activities <- aged_brain@history$activities
  cl.c <- aged_brain@history$coef.clustering
  # cl.c %>% mean()
  # cl.c %>% plot()#main = paste("Clustering Coefficient of", b@name))
  
  cl.c_range <- cl.c %>% range()
  coef.clustering_normalized <- (cl.c_range[2]-cl.c_range[1])/mean(cl.c)
  
  # variance of activity of nodes at each update
  (variance_between_node <- apply(activities, 1, var)) # %>% plot(main = "between")
  
  # variance of each node at it's life time
  (variance_within_node <- apply(activities, 2, var)) # %>% plot(main = "within")
  
  # trial_summary <- list(name = aged_brain@name,
  #                       age = aged_brain@age,
  #                       parameters = aged_brain@parameters,
  #                       mean_variance = variance_between_node %>% mean(),
  #                       coef.clustering_normalized = coef.clustering_normalized
  #                       )
  
  output <- c(aged_brain@name,
              aged_brain@age$update,
              aged_brain@age$rewire,
              aged_brain@parameters$n_nodes,
              aged_brain@parameters$n_edges,
              aged_brain@parameters$eps,
              aged_brain@parameters$seed,
              variance_between_node %>% mean(),
              coef.clustering_normalized
  ) %>% t() %>% as.data.frame()
  
  colnames(output) <- c("name", "age_update", "age_rewire",
                        "n_nodes", "n_edges", "eps",
                        "seed", "mean_variance", "coef.clustering_normalized")
  
  return(output)
  
}


# logistic update for trial -----------------------------------------------

trial_logistic <- function(a, m, eps) {
  # unit.vector allows to calculate M_i by multiplying it the connectivity matrix
  unit.vector <- matrix(1, length(a), 1)
  M <- m %*% unit.vector
  fx <- a %>% mini_logistic() %>% as.matrix() %>% t()
  (a_next <- (1 - eps)*fx + m %*% fx*eps / M) %>% t() %>% return()
  
}

# the vat where brains grow 20190611 -----------------------------------------------
# 
# trial_vat_20190611 <- function(
#   parameters =  list(n_nodes = 100,
#                      n_edges = 0,
#                      eps = 0.2,
#                      freq_snapshot = 20,
#                      seed = -99),
#   n_hr = 1, # number of heartupdates per rewire
#   n_rewires = 1, # number of rewirings/updates
#   young_brain = NULL,
#   save_brain = FALSE,
#   name = NULL,
#   quiet = FALSE
# ){
#   
#   toy_brain <- young_brain
#   
#   if (is.null(toy_brain)) {
#     
#     if(!parameters$num_edges){
#       parameters$num_edges <- round(1.5 * 2 * log(parameters$num_nodes) * (parameters$num_nodes - 1))
#     }
#     if(parameters$seed == 0 | parameters$seed == -99){
#       if(parameters$seed == -99) set.seed(-99)
#       parameters$seed <- 1000 * round(rnorm(1),3) %>% abs()
#     }
#     
#     m <- make_random_graph(size = parameters$num_nodes,
#                            num.links = parameters$num_edges,
#                            seed = parameters$seed)
#     a <- parameters$num_nodes %>% runif(-1, 1) %>% t()
#     
#     cl <-  m %>% my_clustceof()
#     
#     l <- list(
#       activities = a,
#       mat.connectivity = list(m),
#       coef.clustering = cl
#     )
#     
#     
#     if(is.null(name)) name <- give_name(num = 1, seed = parameters$seed)
#     toy_brain <- new(
#       "brain",
#       name = name,
#       birthday = as.character(Sys.time()),
#       age = list(beat = 1, minute = 1),
#       starting_values = l,
#       parameters = parameters,
#       history = l,
#       now = l
#     )
#   }
#   
#   starting_beat <- toy_brain@age$beat
#   owner_firstname <- strsplit(x = toy_brain@name, split = " ")[[1]][1]
#   
#   time_start <- Sys.time()
#   for(m in 1: num_minutes) {
#     for(h in starting_beat:(starting_beat+num_hr)){
#       toy_brain <- toy_brain %>% heartbeat_update()
#     }
#     toy_brain <- toy_brain %>%
#       heartbeat_rewire()#freq_snapshot = toy_brain@parameters$freq_snapshot)
#     if(!quiet) print(paste(owner_firstname, "is", m, "minutes old now."))
#   }
#   
#   (time_taken <- Sys.time() - time_start) %>% paste("for",owner_firstname) %>%
#     print()
#   
#   if(save_brain) save_vars(prefix = paste0("vat_", toy_brain@name))
#   
#   toy_brain %>% return()
#   
#   }


# a unified function to grow (update+rewire) ------------------------------
# 
# trial_grow_20190612 <- function(
#   parameters =  list(n_nodes = 100,
#                      n_edges = 0,
#                      eps = 0.2,
#                      seed = -99),
#   n_updates = 1, # number of heartupdates per rewireupdates of notes
#   n_rewires = 1, # number of rewirings of the network
#   freq_snapshot = 200, # frequency of saving connectivity matrices in the brain
#   brain_younger = NULL,
#   save_brain = FALSE,
#   name = NULL,
#   quiet = FALSE){
#   
#   
#   brain_growing <- brain_younger
#   
#   
#   # making a brain if there is no younger brain to continue growing ---------
#   
#   if (is.null(brain_growing)) {
#     
#     if(!parameters$n_edges){
#       parameters$n_edges <- round(1.5 * 2 * log(parameters$n_nodes) * (parameters$n_nodes - 1))
#     }
#     if(parameters$seed == 0 | parameters$seed == -99){
#       if(parameters$seed == -99) set.seed(-99)
#       parameters$seed <- 1000 * round(rnorm(1),3) %>% abs()
#     }
#     
#     m <- make_random_graph(size = parameters$n_nodes,
#                            num.links = parameters$n_edges,
#                            seed = parameters$seed)
#     a <- parameters$n_nodes %>% runif(-1, 1) %>% t()
#     
#     cl <-  m %>% my_clustceof()
#     
#     l <- list(
#       activities = a,
#       mat.connectivity = list(m),
#       coef.clustering = cl
#     )
#     
#     # giving the brain a name if not already specified
#     if(is.null(name)) name <- give_name(num = 1, seed = parameters$seed)
#     
#     brain_growing <- new(
#       "brain",
#       name = name,
#       birthday = as.character(Sys.time()),
#       age = list(updates = 1, rewires = 1),
#       starting_values = l,
#       parameters = parameters,
#       history = l,
#       now = l
#     )
#   }
#   
#   # writing the update and rewire again together here -----------------------
#   
#   
#   # extracting values of now
#   m.l <- brain_growing@now$mat.connectivity -> now.m
#   if(is.list(m.l)) now.m <- m.l[[length(m.l)]]
#   now.a <- brain_growing@now$activities
#   now.c <- brain_growing@now$coef.clustering
#   eps <- brain_growing@parameters$eps
#   
#   now.updates <- brain_growing@age$updates
#   now.rewires <- brain_growing@age$rewires
#   
#   
#   # extracting historical values
#   his.a <- brain_growing@history$activities
#   his.c <- brain_growing@history$coef.clustering
#   his.m <- brain_growing@history$mat.connectivity
#   
#   
#   time_start <- Sys.time()
#   new.a <- now.a
#   new.m <- now.m
#   new.updates <- now.updates
#   new.rewires <- now.rewires
#   
#   for(r in 1:n_rewires) {
#     # updating nodes for n_update times
#     for(u in 1:n_updates) {
#       new.a <- trial_logistic(new.a, new.m, eps)
#       his.a <- his.a %>% rbind(new.a)
#     }
#     
#     # incrementing age
#     new.updates <- new.updates + n_updates
#     new.rewires <- new.rewires + 1
#     
#     # rewiring and new clustering coefficient
#     new.m <- my_rewire(new.a, new.m)
#     new.c <- new.m %>% my_clustceof()
#     
#     # adding clustering coefficient to the history
#     his.c <- his.c %>% c(new.c)
#     
#     # saving the snapshot
#     if(!(new.rewires %% freq_snapshot)) his.m[[new.rewires]] <- new.m
#     
#     if(!quiet) print(paste(brain_growing@name, "is now",
#                            new.updates, "updates and",
#                            new.rewires, "rewires old."))
#   }
#   
#   # updating the brain
#   ## age
#   brain_growing@age$updates <- new.updates
#   brain_growing@age$rewires <- new.rewires
#   
#   ## history
#   brain_growing@history$activities <- his.a
#   brain_growing@history$mat.connectivity <- his.m
#   brain_growing@history$coef.clustering <- his.c
#   
#   ## now
#   brain_growing@now$activities <- new.a
#   brain_growing@now$mat.connectivity <- new.m
#   brain_growing@now$coef.clustering <- new.c
#   
#   (time_taken <- Sys.time() - time_start) %>% paste("for", brain_growing@name) %>%
#     print()
#   
#   if(save_brain) save_vars("brain_growing", prefix = paste0("vat_", brain_growing@name))
#   
#   brain_growing %>% return()
#   
# }
# 

