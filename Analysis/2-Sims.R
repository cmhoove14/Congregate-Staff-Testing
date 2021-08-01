# -------------------------
# Simulations across testing strategies and frequencies assuming worker-worker transmission and leaky work schedules  
# Chris Hoover June 2021  
# -------------------------

library(parallel)

source("R/Utils.R")
source("R/Sim_Functions.R")
source("Analysis/0-Sim_Setup.R")
load("data/sim_workers.RData")

set.seed(430)

sim_grid <- expand.grid(test_freq  = c(0,0.5,1,2,4),
                        test_sys   = c("systematic", "random"),
                        work_sched = c("leaky", "cohort"),
                        delay      = c(0,1,2),
                        comm_prev  = c(lambda1,lambda2,lambda3),
                        R          = c(R1, R2, R3))

# Remove redundant 
sim_grid <- sim_grid[-which(sim_grid$test_freq == 0 & sim_grid$test_sys == "random"),]

sim_grid_expand <- as.data.frame(sim_grid) %>% 
  slice(rep(1:n(), each = n_sims))

# Check to ensure sim function runs properly
# test_sim <- sim_work_transmission(Lambda    = 0.01*dt,
#                                   R_work    = 1,
#                                   R         = 1,
#                                   delay     = 0,
#                                   test_sens = 0,
#                                   workers   = workers_leaky_testday1,
#                                   sim_t     = sim_t,
#                                   dt        = dt,
#                                   verbose   = F)

# Check to make sure all the right worker lists are being called
# for(i in 1:nrow(sim_grid)){
#   test_freq  = sim_grid[i,1]
#   test_sys   = sim_grid[i,2]
#   work_sched = sim_grid[i,3]
#   comm_prev  = sim_grid[i,4]
#   R          = sim_grid[i,5]
#   
#   if(work_sched == "leaky"){
#     workers_use_char <- ifelse(test_sys == "systematic",
#                                ifelse(test_freq == 0, "workers_leaky",
#                                       ifelse(test_freq == 0.5, "workers_leaky_testday1_biweekly",
#                                              ifelse(test_freq == 1, "workers_leaky_testday1",
#                                                     ifelse(test_freq == 2, "workers_leaky_testday13", "workers_leaky_testday1234")))),
#                                ifelse(test_freq == 0.5, "workers_leaky_testday_r1_biweekly",
#                                       ifelse(test_freq == 1, "workers_leaky_testday_r1", 
#                                              ifelse(test_freq == 2, "workers_leaky_testday_r2", "workers_leaky_testday_r4"))))
#   } else if(work_sched == "cohort"){
#     workers_use_char <- ifelse(test_sys == "systematic",
#                                ifelse(test_freq == 0, "workers",
#                                       ifelse(test_freq == 0.5, "workers_testday1_biweekly",
#                                              ifelse(test_freq == 1, "workers_testday1",
#                                                     ifelse(test_freq == 2, "workers_testday13", "workers_testday1234")))),
#                                ifelse(test_freq == 0.5, "workers_testday_r1_biweekly",
#                                       ifelse(test_freq == 1, "workers_testday_r1", 
#                                              ifelse(test_freq == 2, "workers_testday_r2", "workers_testday_r4"))))
#     
#   } else {
#     # Will throw error below
#     workers_use_char <- NULL
#   }
#   #   
#      print(workers_use_char)
# }

clooster <- makeCluster(detectCores()-1)

clusterExport(clooster, varlist = ls())

clusterEvalQ(clooster, library(tidyverse))
clusterEvalQ(clooster, library(triangle))

all_sims <- bind_rows(parLapply(cl = clooster,
                                X = 1:nrow(sim_grid_expand),
                                fun = function(i){
                                  test_freq  = sim_grid_expand[i,1]
                                  test_sys   = sim_grid_expand[i,2]
                                  work_sched = sim_grid_expand[i,3]
                                  d          = sim_grid_expand[i,4]
                                  comm_prev  = sim_grid_expand[i,5]
                                  R          = sim_grid_expand[i,6]
                                  
                                  if(work_sched == "leaky"){
                                    workers_use_char <- ifelse(test_sys == "systematic",
                                                               ifelse(test_freq == 0, "workers_leaky",
                                                                      ifelse(test_freq == 0.5, "workers_leaky_testday1_biweekly",
                                                                             ifelse(test_freq == 1, "workers_leaky_testday1",
                                                                                    ifelse(test_freq == 2, "workers_leaky_testday13", "workers_leaky_testday1234")))),
                                                               ifelse(test_freq == 0.5, "workers_leaky_testday_r1_biweekly",
                                                                      ifelse(test_freq == 1, "workers_leaky_testday_r1", 
                                                                             ifelse(test_freq == 2, "workers_leaky_testday_r2", "workers_leaky_testday_r4"))))
                                  } else if(work_sched == "cohort"){
                                    workers_use_char <- ifelse(test_sys == "systematic",
                                                               ifelse(test_freq == 0, "workers_base",
                                                                      ifelse(test_freq == 0.5, "workers_testday1_biweekly",
                                                                             ifelse(test_freq == 1, "workers_testday1",
                                                                                    ifelse(test_freq == 2, "workers_testday13", "workers_testday1234")))),
                                                               ifelse(test_freq == 0.5, "workers_testday_r1_biweekly",
                                                                      ifelse(test_freq == 1, "workers_testday_r1", 
                                                                             ifelse(test_freq == 2, "workers_testday_r2", "workers_testday_r4"))))
                                    
                                  } else {
                                    # Will throw error below
                                    workers_use_char <- NULL
                                  }
                                  
                                  workers_use <- get(workers_use_char)
                                  
                                  sim_work_transmission(Lambda    = comm_prev*dt,
                                                        R_work    = R,
                                                        R         = R,
                                                        delay     = d,
                                                        test_sens = 0,
                                                        workers   = workers_use,
                                                        sim_t     = sim_t,
                                                        dt        = dt,
                                                        verbose   = F)$cases_tests %>% 
                                    mutate(sim = i,
                                           lambda = comm_prev, 
                                           R = R,
                                           work_sched = work_sched,
                                           delay = d,
                                           testsys = test_sys,
                                           testfreq = test_freq)
                                  
                                }))  

stopCluster(clooster)

# Summarise sims to totals at the end of the sim 
sims_end <- all_sims %>% 
  group_by(sim, lambda, R, work_sched, delay, testsys, testfreq) %>% 
  summarise(totcases = sum(exp_cases),
            totdays  = sum(inf_days),
            tottests = sum(tests_adm)) %>% 
  ungroup()

# Make sure there's the right number of summaries 
nrow(sims_end) == nrow(sim_grid_expand)

saveRDS(sims_end, "data/sims_end_totals.RDS")


