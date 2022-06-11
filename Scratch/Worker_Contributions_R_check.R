library(parallel)

load("data/sim_workers.RData")
source("R/Utils.R")
source("R/Sim_Functions.R")
source("Analysis/0-Sim_Setup.R")

set.seed(430)

# Function to extract worker contributions from workers list post-sim
extract_contributions <- function(workers_list){
  unlist(lapply(workers_list, function(w){
    if(is.finite(w$t_infect)){
      infected_start <- min(which(w$state == "E"))+1
      infected_end   <- min(which(!w$state %in% c("S", "E", "I")))-1
      
      infected_end <- ifelse(is.infinite(infected_end), length(w$work_schedule), infected_end)
      
      #cat(infected_start, " ", infected_end, "\n")
      
      working        <- w$work_schedule[infected_start:infected_end]
      infectiousness <- w$infectiousness[1:(infected_end-infected_start+1)]
      
      w_exp_cases    <- sum(working*infectiousness)
      
    } else {
      w_exp_cases <- 0
    }
    
    return(w_exp_cases)
  }))
}

# Sim with base workers weekly testing
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

worker_contributions <- extract_contributions(test_sim_workers)

work_cont1 <- tibble("rt" = worker_contributions, variant = "alpha", "delay" = 0)

sum(worker_contributions)

base_workers_infectiousness <- bind_rows(lapply(test_sim_workers, function(w){
  if(length(w$infectiousness)>0){
    tibble("t" = 1:length(w$infectiousness)/3,
           "beta_t" = w$infectiousness)
  } else {
    NULL
  }
})) %>% 
  group_by(t) %>% 
  summarise(mean_beta = mean(beta_t))

plot(base_workers_infectiousness, type = "l")

# Compare to when delay of one day 
test_sim_d1 <- sim_work_transmission(Lambda    = 0.01*dt,
                                  Reff      = 1,
                                  delay     = 1,
                                  test_thresh = 0,
                                  test_spec = 1,
                                  test_sens = 1,
                                  workers   = workers_leaky_testday1,
                                  sim_t     = sim_t,
                                  dt        = dt,
                                  verbose   = F)

test_sim_workers_d1 <- test_sim_d1$workers

worker_contributions_d1 <- extract_contributions(test_sim_workers_d1)


work_cont2 <- tibble("rt" = worker_contributions_d1, variant = "alpha", "delay" = 1)

sum(worker_contributions_d1)

# Delta variant worker contributions  

load("data/sim_workers_delta.RData")


test_sim_delta <- sim_work_transmission(Lambda    = 0.01*dt,
                                        Reff      = 1,
                                        delay     = 0,
                                        test_thresh = 0,
                                        test_spec = 1,
                                        test_sens = 1,
                                        workers   = workers_leaky_testday1_delta,
                                        sim_t     = sim_t,
                                        dt        = dt,
                                        verbose   = F)

test_sim_workers_delta <- test_sim_delta$workers

worker_contributions_delta <- extract_contributions(test_sim_workers_delta)


work_cont3 <- tibble("rt" = worker_contributions_delta, variant = "delta", "delay" = 0)

sum(worker_contributions_delta)

delta_workers_infectiousness <- bind_rows(lapply(test_sim_workers_delta, function(w){
  if(length(w$infectiousness)>0){
    tibble("t" = 1:length(w$infectiousness)/3,
           "beta_t" = w$infectiousness)
  } else {
    NULL
  }
})) %>% 
  group_by(t) %>% 
  summarise(mean_beta = mean(beta_t))

lines(delta_workers_infectiousness, col = 2)

# DElta with delay

test_sim_delta_d1 <- sim_work_transmission(Lambda    = 0.01*dt,
                                        Reff      = 1,
                                        delay     = 1,
                                        test_thresh = 0,
                                        test_spec = 1,
                                        test_sens = 1,
                                        workers   = workers_leaky_testday1_delta,
                                        sim_t     = sim_t,
                                        dt        = dt,
                                        verbose   = F)

test_sim_workers_delta_d1 <- test_sim_delta_d1$workers

worker_contributions_delta_d1 <- extract_contributions(test_sim_workers_delta_d1)


work_cont4 <- tibble("rt" = worker_contributions_delta_d1, variant = "delta", "delay" = 0)

sum(worker_contributions_delta_d1)
