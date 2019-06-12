# Intro -------------------------------------------------------------------

## This script keeps all functions needed for GongvLeeuwen2004.
## It's standalone and loads functions_my automatically

# rm(list = ls())


# loading the basic functions and packages --------------------------------

if(!exists("path.to.functions_my")) path.to.functions_my <- "functions"
source(paste0(path.to.functions_my,"/functions_my.R"))
source(paste0(path.to.functions_my,"/functions_heartbeat.R"))



# the vat where brains grow -----------------------------------------------

trial_vat <- function(
  parameters =  list(num_nodes = 100,
                     num_edges = 0,
                     eps = 0.2,
                     freq_snapshot = 20,
                     seed = -99),
  num_hr = 1, # number of heartbeats per minute
  num_minutes = 1, # number of rewirings/updates
  young_brain = NULL,
  save_brain = FALSE,
  name = NULL,
  quiet = FALSE
){
  
  toy_brain <- young_brain
  
  if (is.null(toy_brain)) {
    
    if(!parameters$num_edges){
      parameters$num_edges <- round(1.5 * 2 * log(parameters$num_nodes) * (parameters$num_nodes - 1))
    }
    if(parameters$seed == 0 | parameters$seed == -99){
      if(parameters$seed == -99) set.seed(-99)
      parameters$seed <- 1000 * round(rnorm(1),3) %>% abs()
    }
    
    m <- make_random_graph(size = parameters$num_nodes,
                           num.links = parameters$num_edges,
                           seed = parameters$seed)
    a <- parameters$num_nodes %>% runif(-1, 1) %>% t()
    
    cl <-  m %>% my_clustceof()
    
    l <- list(
      activities = a,
      mat.connectivity = list(m),
      coef.clustering = cl
    )
    
    
    if(is.null(name)) name <- give_name(num = 1, seed = parameters$seed)
    toy_brain <- new(
      "brain",
      name = name,
      birthday = as.character(Sys.time()),
      age = list(beat = 1, minute = 1),
      starting_values = l,
      parameters = parameters,
      history = l,
      now = l
    )
  }
  
  starting_beat <- toy_brain@age$beat
  owner_firstname <- strsplit(x = toy_brain@name, split = " ")[[1]][1]
  
  time_start <- Sys.time()
  for(m in 1: num_minutes) {
    for(h in starting_beat:(starting_beat+num_hr)){
      toy_brain <- toy_brain %>% heartbeat_update()
    }
    toy_brain <- toy_brain %>%
      heartbeat_rewire()#freq_snapshot = toy_brain@parameters$freq_snapshot)
    if(!quiet) print(paste(owner_firstname, "is", m, "minutes old now."))
  }
  
  (time_taken <- Sys.time() - time_start) %>% paste("for",owner_firstname) %>%
    print()
  
  if(save_brain) save_vars(prefix = paste0("vat_", toy_brain@name))
  
  toy_brain %>% return()
  
}


# brain culture and summary -----------------------------------------------

trial_summary <- function(aged_brain){
  
  aged_brain <- toy_brain
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
              aged_brain@age$beat,
              aged_brain@age$minute,
              aged_brain@parameters$num_nodes,
              aged_brain@parameters$num_edges,
              aged_brain@parameters$eps,
              aged_brain@parameters$seed,
              variance_between_node %>% mean(),
              coef.clustering_normalized
  ) %>% t() %>% as.data.frame()
  
  colnames(output) <- c("name", "age_beat", "age_minute",
                        "num_nodes", "num_edges", "eps",
                        "seed", "mean_variance", "coef.clustering_normalized")
  
  return(output)
  
}
