# Generate vector or matrix of infection trajectory parameters  
sim_inf_pars <- function(n){
  t_inc <- rlnorm(n, 1.63, 0.5)
  t_lnt <- t_inc + runif(n, -2, 0)
  t_inf <- runif(n, 6.5, 9.5)
  
  return(cbind(t_inc, t_lnt, t_inf))
}


# Simulate transmission given parameters, time characteristics, and workers
sim_work_transmission <- function(Lambda, alpha, R, delay, test_sens, workers, sim_t, dt, verbose = FALSE){
  
  exp_cases <- numeric(sim_t)
  tests_adm <- numeric(sim_t)
  
  for(t in 2:sim_t){
    # Advance infections ----------
    states       <- unlist(lapply(workers, function(w) w$state[t-1]))
    infecteds    <- which(states %in% c("E", "I", "T", "Q"))
    
    for(i in infecteds){
      # Advance time infected
      workers[[i]]$t_infect <- workers[[i]]$t_infect+1
      
      # Worker becomes infectious if past latent period and not yet tested/quarantined
      if(workers[[i]]$t_infect*dt > workers[[i]]$t_latent & 
         workers[[i]]$state[t-1] %in% c("E", "I")){
        workers[[i]]$state[t] <- "I"
      }
      
      # Worker is recovered if past end of infectious period
      if(workers[[i]]$t_infect > length(workers[[i]]$infectiousness)){
        workers[[i]]$state[t] <- "R"
      }
    }
    
    # Testing and isolation  -----------
    tested <- which(unlist(lapply(workers, function(w) w$test_schedule[t])) == 1 & 
                    states != "Q") 
    tests_adm[t] <- length(tested)
    
    if(verbose){
      cat(tests_adm[t], "tests administered\n")
    }
    
    
    # Testing conducted. If worker infectious above test sens level, flagged as tested and sets clock for delay until test result returned and quarantine
    for(i in tested){
      # If worker actively infectious
      if(workers[[i]]$state[t-1] == "I" &
         workers[[i]]$state[t] != "R"){
        # If worker's infectiousness is greater than test sensitivity
        if(workers[[i]]$infectiousness[workers[[i]]$t_infect] > test_sens){
          workers[[i]]$state[t] <- "T"
          workers[[i]]$delay <- delay
          # If no delay, instantly quarantine
          if(delay <= 0 & workers[[i]]$state[t] == "T"){
            workers[[i]]$state[t] <- "Q"
          }
        }
      }
    }
    
    # Test delay and quarantine on notification of positive if delay > 0
    for(i in which(states == "T")){
      workers[[i]]$delay <- workers[[i]]$delay - 1
      if(workers[[i]]$delay <= 0){
        workers[[i]]$state[t] <- "Q"
      }
    }
    
    # New infections ------------
    # Determine who's working
    working <- which(unlist(lapply(workers, function(w) w$work_schedule[t])) == 1)
    
    # FOIs
    Lambda_it          <- rep(Lambda*dt, length(workers)) # Community infectivity
    Lambda_it[working] <- Lambda*dt*alpha                 # Workplace infectivity for those working
    
    # Bernoulli trials determine who is exposed, become infected if exposed and susceptible
    bernoullis <- rbinom(length(workers),1,Lambda_it)
    
    new_Is <- which(bernoullis == 1 & states == "S")
    
    # Assign characteristics for newly exposed
    if(length(new_Is) > 0){
      for(i in new_Is){
        workers[[i]]$state[t] <- "E"
        workers[[i]]$t_infect <- 0
        workers[[i]]$infectiousness <- infectious_profile(t_latent     = workers[[i]]$t_latent, 
                                                          t_peak       = workers[[i]]$t_incubation, 
                                                          t_infectious = workers[[i]]$t_infectious, 
                                                          dt           = dt)
      }
    }
    
    # Finalize advancement of states
    states_advanced <- unlist(lapply(workers, function(w) w$state[t]))
    unchanged <- which(states_advanced == "")
    for(i in unchanged){
      workers[[i]]$state[t] <- workers[[i]]$state[t-1]
    }
    
    
    # Infectious work days -------
    infectors_t  <- which(states_advanced %in% c("I", "T")) # Workers infectious or waiting on test will transmit
    inf_work_t   <- unlist(lapply(infectors_t, function(i) workers[[i]]$work_schedule[t]))
    infectious_t <- unlist(lapply(infectors_t, function(i) workers[[i]]$infectiousness[workers[[i]]$t_infect]))
    
    if(verbose){
      cat("S -", sum(states_advanced=="S"), "  ",
          "E -", sum(states_advanced=="E"), "  ",
          "I -", sum(states_advanced=="I"), "  ",
          "T -", sum(states_advanced=="T"), "  ",
          "Q -", sum(states_advanced=="Q"), "  ",
          "R -", sum(states_advanced=="R"), "\n")
    }
    
    exp_cases[t] <- sum(inf_work_t*infectious_t*R)
  }
  
  out_cases_tests <- tibble("exp_cases" = exp_cases,
                            "tests_adm" = tests_adm,
                            "time"      = 1:sim_t)
  
  out_list <- list()
  out_list$workers   <- workers
  out_list$cases_tests <- out_cases_tests

  return(out_list)
} 
