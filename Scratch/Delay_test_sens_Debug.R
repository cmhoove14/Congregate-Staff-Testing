load("data/sim_workers.RData")
source("R/Utils.R")
source("R/Sim_Functions.R")
source("Analysis/0-Sim_Setup.R")

set.seed(430)
test_sim_d0 <- sim_work_transmission(Lambda    = 0.01*dt,
                                  R_work    = 1.5,
                                  R         = 1.5,
                                  delay     = 0,
                                  test_thresh = 0,
                                  test_spec = 1,
                                  test_sens = 1, 
                                  workers   = workers_leaky_testday13,
                                  sim_t     = sim_t,
                                  dt        = dt,
                                  verbose   = T)

sum(test_sim_d0$cases_tests$exp_cases)

set.seed(430)
test_sim_d1 <- sim_work_transmission(Lambda    = 0.01*dt,
                                     R_work    = 1.5,
                                     R         = 1.5,
                                     delay     = 1,
                                     test_thresh = 0,
                                     test_spec = 1,
                                     test_sens = 1, 
                                     workers   = workers_leaky_testday13,
                                     sim_t     = sim_t,
                                     dt        = dt,
                                     verbose   = T)

sum(test_sim_d1$cases_tests$exp_cases)


set.seed(430)
test_sim_d2 <- sim_work_transmission(Lambda    = 0.01*dt,
                                     R_work    = 1.5,
                                     R         = 1.5,
                                     delay     = 2,
                                     test_thresh = 0,
                                     test_spec = 1,
                                     test_sens = 1, 
                                     workers   = workers_leaky_testday13,
                                     sim_t     = sim_t,
                                     dt        = dt,
                                     verbose   = T)

sum(test_sim_d2$cases_tests$exp_cases)







set.seed(430)
test_sim_s1 <- sim_work_transmission(Lambda    = 0.01*dt,
                                     R_work    = 1.5,
                                     R         = 1.5,
                                     delay     = 0,
                                     test_thresh = 0,
                                     test_spec = 1,
                                     test_sens = 1, 
                                     workers   = workers_leaky_testday13,
                                     sim_t     = sim_t,
                                     dt        = dt,
                                     verbose   = T)

sum(test_sim_s1$cases_tests$exp_cases)


set.seed(430)
test_sim_s08 <- sim_work_transmission(Lambda    = 0.01*dt,
                                     R_work    = 1.5,
                                     R         = 1.5,
                                     delay     = 0,
                                     test_thresh = 0,
                                     test_spec = 1,
                                     test_sens = 0.8, 
                                     workers   = workers_leaky_testday13,
                                     sim_t     = sim_t,
                                     dt        = dt,
                                     verbose   = T)

sum(test_sim_s08$cases_tests$exp_cases)



set.seed(430)
test_sim_s09 <- sim_work_transmission(Lambda    = 0.01*dt,
                                      R_work    = 1.5,
                                      R         = 1.5,
                                      delay     = 0,
                                      test_thresh = 0,
                                      test_spec = 1,
                                      test_sens = 0.9, 
                                      workers   = workers_leaky_testday13,
                                      sim_t     = sim_t,
                                      dt        = dt,
                                      verbose   = T)

sum(test_sim_s09$cases_tests$exp_cases)



set.seed(430)
test_sim_s099 <- sim_work_transmission(Lambda    = 0.01*dt,
                                      R_work    = 1.5,
                                      R         = 1.5,
                                      delay     = 0,
                                      test_thresh = 0,
                                      test_spec = 1,
                                      test_sens = 0.99, 
                                      workers   = workers_leaky_testday13,
                                      sim_t     = sim_t,
                                      dt        = dt,
                                      verbose   = T)

sum(test_sim_s099$cases_tests$exp_cases)

# ---------------
Lambda    = 0.01*dt
R_work    = 1.5
R         = 1.5
delay     = 0
test_thresh = 0
test_spec = 1
test_sens = 0.95
workers   = workers_leaky_testday13
sim_t     = sim_t
dt        = dt
verbose   = T
workers = workers_leaky_testday13
symps = F

inf_days  <- numeric(sim_t)
exp_cases <- numeric(sim_t)
tests_adm <- numeric(sim_t)

set.seed(430)

for(t in 2:300){
  # Advance infections ----------
  states       <- unlist(lapply(workers, function(w) w$state[t-1]))
  infecteds    <- which(states %in% c("E", "I", "T", "O"))
  
  for(i in infecteds){
    # Advance time infected
    workers[[i]]$t_infect <- workers[[i]]$t_infect+1
    
    # Update state to default to current state for infecteds. Will get overwritten below if state changes. This helps advance tested but not yet isolated workers progress to being isolated when there is a delay
    workers[[i]]$state[t] <- workers[[i]]$state[t-1]
    
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
  
  # Testing conducted. See Utils.R `test_workers` function for details
  if(length(tested) > 0){
    workers <- test_workers(test_indices = tested, workers = workers, timestep = t, 
                            test_thresh = test_thresh, test_sens = test_sens, test_spec = test_spec, delay = delay*1/dt)
  }
  
  states_updated <- unlist(lapply(workers, function(w) w$state[t]))
  
  # Test delay and quarantine on notification of positive if delay > 0. If delay=0, this will send positive workers just tested straight to quarantine
  for(i in which(states_updated == "T")){
    workers[[i]]$delay <- workers[[i]]$delay - 1
    if(workers[[i]]$delay <= 0){
      workers[[i]]$state[t] <- "O"
      # Workers who are isolated after testing positive not tested for 90 days per guidance
      workers[[i]]$test_schedule[t:max(c(t+90*(1/dt), sim_t))] <- 0 
    }
  }
  
  # Self-isolation due to symptoms if modeling symptoms
  if(symps){
    for(i in which(states_updated == "I")){
      # Assume that symptoms start at time of peak infectiousness and that workers who self isolate do so upon symptom onset
      if(workers[[i]]$selfiso == 1 & workers[[i]]$t_infect >= which.max(workers[[i]]$infectiousness)){
        workers[[i]]$state[t] <- "O"
      } 
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
      if(symps){
        # All we really care about is if someone ends up self-isolating due to symptoms, so only record selfiso variable
        symp                 <- rbinom(1,1,p_symp)
        workers[[i]]$selfiso <- ifelse(symp == 1, rbinom(1,1,p_selfiso), 0)
      }
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
    states_fin <- unlist(lapply(workers, function(w) w$state[t]))
    
    cat("S -", sum(states_fin=="S"), "  ",
        "E -", sum(states_fin=="E"), "  ",
        "I -", sum(states_fin=="I"), "  ",
        "T -", sum(states_fin=="T"), "  ",
        "O -", sum(states_fin=="O"), "  ",
        "R -", sum(states_fin=="R"), "\n")
  }
  
  exp_cases[t] <- sum(inf_work_t*infectious_t*R)
  inf_days[t]  <- sum(inf_work_t*infectious_t*R > 0)
}


t = 301
