library(tidyverse)

load("data/sim_workers.RData")
source(here::here("R/Utils.R"))
source(here::here("R/Sim_Functions.R"))
source("Analysis/0-Sim_Setup.R")

sim_work_transmission2 <- function(Lambda, R_work, R, delay, test_thresh, workers, sim_t, dt, verbose = FALSE){
  
  inf_days  <- numeric(sim_t)
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
    tested <- which(unlist(lapply(workers, function(w) w$test_schedule[t])) == 1) 
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
        if(workers[[i]]$infectiousness[workers[[i]]$t_infect] > test_thresh){
          workers[[i]]$state[t] <- "T"
          workers[[i]]$delay <- delay
          # If no delay, instantly quarantine
        }
      }
    }
    
    states_updated <- unlist(lapply(workers, function(w) w$state[t]))
    
    # Test delay and quarantine on notification of positive if delay > 0
    for(i in which(states_updated == "T")){
      workers[[i]]$delay <- workers[[i]]$delay - 1
      if(workers[[i]]$delay <= 0){
        workers[[i]]$state[t] <- "Q"
        # Workers who are isolated after testing positive not tested for 90 days per guidance
        workers[[i]]$test_schedule[t:max(c(t+90*(1/dt), sim_t))] <- 0 
      }
    }
    
    # New infections ------------
    # Determine who's working and infectious
    working <- which(unlist(lapply(workers, function(w) w$work_schedule[t])) == 1)
    infectors_t  <- which(states_updated %in% c("I", "T")) # Workers infectious or waiting on test will transmit
    inf_work_t   <- unlist(lapply(infectors_t, function(i) workers[[i]]$work_schedule[t]))
    infectious_t <- unlist(lapply(infectors_t, function(i) workers[[i]]$infectiousness[workers[[i]]$t_infect]))
    
    # FOIs
    Lambda_it          <- rep(Lambda*dt, length(workers))                     # Community infectivity
    Lambda_it[working] <- sum(inf_work_t*infectious_t*R_work)/length(working) # Workplace infectivity for those working. 
    # beta*I/N except beta*I is weighted by infectiousness and determined by workplace transmission, R_work
    
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
    inf_days[t]  <- sum(inf_work_t*infectious_t*R > 0)
  }
  
  out_cases_tests <- tibble("exp_cases" = exp_cases,
                            "inf_days"  = inf_days,
                            "tests_adm" = tests_adm,
                            "time"      = 1:sim_t)
  
  out_list <- list()
  out_list$workers   <- workers
  out_list$cases_tests <- out_cases_tests
  
  return(out_list)
} 


n_sims <- 30
n_reps <- 30

test_sims  <- matrix(NA, nrow = n_sims, ncol = 2)
test_sims2 <- matrix(NA, nrow = n_sims, ncol = 2)

for(i in 1:n_sims){
  rep_holder1 <- matrix(NA, nrow = n_reps, ncol = 2)
  rep_holder2 <- matrix(NA, nrow = n_reps, ncol = 2)
  
  for(j in 1:n_reps){
    sim <- sim_work_transmission(0.01*dt, R_work = 1, R = 1, delay = 0, 
                                 test_thresh = 0, test_sens = 1, test_spec = 1,
                                 workers = workers_testday_r2, sim_t = sim_t, dt = dt)
    sim2 <- sim_work_transmission2(0.01*dt, R_work = 1, R = 1, delay = 0, test_thresh = 0, 
                                   workers = workers_testday_r2, sim_t = sim_t, dt = dt)
    
    rep_holder1[j,] <- c(sum(sim$cases_tests$exp_cases), sum(sim$cases_tests$tests_adm))
    rep_holder2[j,] <- c(sum(sim2$cases_tests$exp_cases), sum(sim2$cases_tests$tests_adm))
  }
  
  test_sims[i,] <- c(mean(rep_holder1[,1]), mean(rep_holder1[,2]))
  test_sims2[i,] <- c(mean(rep_holder1[,1]), mean(rep_holder1[,2]))
  print(i)
}

boxplot(cbind(test_sims[,1], test_sims2[,1]))
boxplot(cbind(test_sims[,2], test_sims2[,2]))









sim_work_transmission2 <- function(Lambda, R_work, R, delay, test_thresh, workers, sim_t, dt, verbose = FALSE){
  
  inf_days  <- numeric(sim_t)
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
    tested <- which(unlist(lapply(workers, function(w) w$test_schedule[t])) == 1) 
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
        if(workers[[i]]$infectiousness[workers[[i]]$t_infect] > test_thresh){
          workers[[i]]$state[t] <- "T"
          workers[[i]]$delay <- delay
          # If no delay, instantly quarantine
        }
      }
    }
    
    states_updated <- unlist(lapply(workers, function(w) w$state[t]))
    
    # Test delay and quarantine on notification of positive if delay > 0
    for(i in which(states_updated == "T")){
      workers[[i]]$delay <- workers[[i]]$delay - 1
      if(workers[[i]]$delay <= 0){
        workers[[i]]$state[t] <- "Q"
        # Workers who are isolated after testing positive not tested for 90 days per guidance
        workers[[i]]$test_schedule[t:max(c(t+90*(1/dt), sim_t))] <- 0 
      }
    }
    
    # New infections ------------
    # Determine who's working and infectious
    working <- which(unlist(lapply(workers, function(w) w$work_schedule[t])) == 1)
    infectors_t  <- which(states_updated %in% c("I", "T")) # Workers infectious or waiting on test will transmit
    inf_work_t   <- unlist(lapply(infectors_t, function(i) workers[[i]]$work_schedule[t]))
    infectious_t <- unlist(lapply(infectors_t, function(i) workers[[i]]$infectiousness[workers[[i]]$t_infect]))
    
    # FOIs
    Lambda_it          <- rep(Lambda*dt, length(workers))                     # Community infectivity
    Lambda_it[working] <- sum(inf_work_t*infectious_t*R_work)/length(working) # Workplace infectivity for those working. 
    # beta*I/N except beta*I is weighted by infectiousness and determined by workplace transmission, R_work
    
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
    inf_days[t]  <- sum(inf_work_t*infectious_t*R > 0)
  }
  
  out_cases_tests <- tibble("exp_cases" = exp_cases,
                            "inf_days"  = inf_days,
                            "tests_adm" = tests_adm,
                            "time"      = 1:sim_t)
  
  out_list <- list()
  out_list$workers   <- workers
  out_list$cases_tests <- out_cases_tests
  
  return(out_list)
} 

