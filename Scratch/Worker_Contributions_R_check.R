library(parallel)

load("data/sim_workers.RData")
source("R/Utils.R")
source("R/Sim_Functions.R")
source("Analysis/0-Sim_Setup.R")

set.seed(430)

test_sim <- sim_work_transmission(Lambda    = 0.01*dt,
                                  Reff      = 1,
                                  delay     = 0,
                                  test_thresh = 0,
                                  test_spec = 1,
                                  test_sens = 1,
                                  workers   = workers_leaky_testday1,
                                  sim_t     = sim_t,
                                  dt        = dt,
                                  verbose   = F)

test_sim_workers <- test_sim$workers

worker_contributions <- lapply(test_sim_workers, function(w){
  if(is.finite(w$t_infect)){
    infected_start <- min(which(w$state == "E"))+1
    infected_end   <- min(which(w$state == "R"))-1
    
    infected_end <- ifelse(is.infinite(infected_end), length(w$state), infected_end)
    
    #cat(infected_start, " ", infected_end, "\n")
    
    working        <- w$work_schedule[infected_start:infected_end]
    infectiousness <- w$infectiousness
    
    w_exp_cases    <- sum(working*infectiousness)
    
  } else {
    w_exp_cases <- 0
  }
  
  return(w_exp_cases)
})
