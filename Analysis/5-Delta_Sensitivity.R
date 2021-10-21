# -------------------------
# Sensitivity analyses investigating shortened timing in infectious period for delta variant
# Chris Hoover Sept 2021  
# -------------------------

library(parallel)

load("data/sim_workers_delta.RData")
source("R/Utils.R")
source("R/Sim_Functions.R")
source("Analysis/0-Sim_Setup.R")

# Sim setup ---------------
set.seed(430)

sim_grid_delta <- expand.grid(test_freq  = c(0,0.5,1,2,4),
                              test_sys   = c("systematic", "random"),
                              work_sched = c("leaky", "cohort"),
                              delay      = c(0,1,2),
                              comm_prev  = c(lambda1,lambda2,lambda3))

# Remove redundant 
sim_grid_delta <- sim_grid_delta[-which(sim_grid_delta$test_freq == 0 & sim_grid_delta$test_sys == "random"),]

sim_grid_delta_expand <- as.data.frame(sim_grid_delta) %>% 
  slice(rep(1:n(), each = n_sims))

clooster <- makeCluster(detectCores()-1)

clusterExport(clooster, varlist = ls())

clusterEvalQ(clooster, library(tidyverse))
clusterEvalQ(clooster, library(triangle))

all_sims <- bind_rows(parLapply(cl = clooster,
                                X = 1:nrow(sim_grid_delta_expand),
                                fun = function(i){
                                  test_freq  = sim_grid_delta_expand[i,1]
                                  test_sys   = sim_grid_delta_expand[i,2]
                                  work_sched = sim_grid_delta_expand[i,3]
                                  d          = sim_grid_delta_expand[i,4]
                                  comm_prev  = sim_grid_delta_expand[i,5]
                                  
                                    workers_use_char <- ifelse(test_sys == "systematic",
                                                               ifelse(test_freq == 0, "workers_leaky_delta",
                                                                      ifelse(test_freq == 0.5, "workers_leaky_testday1_biweekly_delta",
                                                                             ifelse(test_freq == 1, "workers_leaky_testday1_delta",
                                                                                    ifelse(test_freq == 2, "workers_leaky_testday13_delta", "workers_leaky_testday1234_delta")))),
                                                               ifelse(test_freq == 0.5, "workers_leaky_testday_r1_biweekly_delta",
                                                                      ifelse(test_freq == 1, "workers_leaky_testday_r1_delta", 
                                                                             ifelse(test_freq == 2, "workers_leaky_testday_r2_delta", "workers_leaky_testday_r4_delta"))))
                                  
                                  workers_use <- get(workers_use_char)
                                  
                                  sim_work_transmission(Lambda    = comm_prev*dt,
                                                        R_work    = R3,
                                                        R         = R3,
                                                        delay     = d,
                                                        test_thresh = 0,
                                                        test_spec = 1,
                                                        test_sens = 1, 
                                                        workers   = workers_use,
                                                        sim_t     = sim_t,
                                                        dt        = dt,
                                                        verbose   = F)$cases_tests %>% 
                                    mutate(sim = i,
                                           lambda = comm_prev, 
                                           R = R3,
                                           work_sched = "leaky",
                                           delay = d,
                                           testsys = test_sys,
                                           testfreq = test_freq,
                                           delta = 1)
                                  
                                }))  

stopCluster(clooster)

# Summarise sims to totals at the end of the sim 
sims_end <- all_sims %>% 
  group_by(sim, lambda, R, work_sched, delay, testsys, testfreq, delta) %>% 
  summarise(totcases = sum(exp_cases),
            adjcases = sum(adj_cases),
            totdays  = sum(inf_days),
            tottests = sum(tests_adm)) %>% 
  ungroup()

# Make sure there's the right number of summaries 
nrow(sims_end) == nrow(sim_grid_delta_expand)

saveRDS(sims_end, "data/delta_sims_end_totals.RDS")
