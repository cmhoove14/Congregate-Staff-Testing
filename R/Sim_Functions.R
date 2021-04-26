sim_work_transmission <- function(Lambda, alpha, R, workers, sim_t, dt, verbose = FALSE){
  
  exp_cases <- numeric(sim_t)
  tests_adm <- numeric(sim_t)
  
  for(t in 1:sim_t){
    # Advance infections ----------
    states       <- unlist(lapply(workers, function(w) w$state))
    infecteds    <- which(states %in% c("E", "I", "Q"))
    
    for(i in infecteds){
      # Advance time infected
      workers[[i]]$t_infect <- workers[[i]]$t_infect+1
      
      # Worker becomes infectious if past latent period
      if(workers[[i]]$t_infect*dt > workers[[i]]$t_latent){
        workers[[i]]$state <- "I"
      }
      
      # Worker is recovered if past end of infectious period
      if(workers[[i]]$t_infect > length(workers[[i]]$infectiousness)){
        workers[[i]]$state <- "R"
      }
    }
    
    # Testing and isolation  -----------
    tested <- which(unlist(lapply(workers, function(w) w$test_schedule[t])) == 1)
    tests_adm[t] <- length(tested)
    
    #Isolate if tested and infectious. Assumes test positive if infectious
    for(i in tested){
      if(workers[[i]]$state == "I"){
        workers[[i]]$state <- "Q"
      }
    }
    
    # New infections ------------
    # Determine who's working
    working <- which(unlist(lapply(workers, function(w) w$work_schedule[t])) == 1)
    
    # FOIs
    Lambda_it          <- rep(Lambda*dt/2*(1-alpha), length(workers)) # Community infectivity. divide by 2 for two time periods spent in community vs only one in workplace
    Lambda_it[working] <- Lambda*dt*alpha                           # Workplace infectivity for those working
    
    # Bernoulli trials determine who is exposed, become infected if exposed and susceptible
    bernoullis <- rbinom(length(workers),1,Lambda_it)
    
    new_Is <- which(bernoullis == 1 & states == "S")
    
    # Assign characteristics for newly exposed
    if(length(new_Is) > 0){
      for(i in new_Is){
        workers[[i]]$state <- "E"
        workers[[i]]$t_infect <- 0
        workers[[i]]$infectiousness <- infectious_profile(t_latent     = workers[[i]]$t_latent, 
                                                          t_peak       = workers[[i]]$t_incubation, 
                                                          t_infectious = workers[[i]]$t_infectious, 
                                                          dt           = dt)
      }
    }
    
    # Infectious work days -------
    states_advanced <- unlist(lapply(workers, function(w) w$state))
    
    infectors_t  <- which(states_advanced == "I")
    inf_work_t   <- unlist(lapply(infectors_t, function(i) workers[[i]]$work_schedule[t]))
    infectious_t <- unlist(lapply(infectors_t, function(i) workers[[i]]$infectiousness[workers[[i]]$t_infect]))
    
    if(verbose){
      cat("S -", sum(states_advanced=="S"), "  ",
          "E -", sum(states_advanced=="E"), "  ",
          "I -", sum(states_advanced=="I"), "  ",
          "Q -", sum(states_advanced=="Q"), "  ",
          "R -", sum(states_advanced=="R"), "\n")
    }
    
    exp_cases[t] <- sum(inf_work_t*infectious_t*R)
  }
  
  out <- tibble(exp_cases,tests_adm) %>% 
    mutate(time = 1:sim_t)
  
  return(out)
} 
