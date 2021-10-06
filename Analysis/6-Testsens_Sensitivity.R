# -------------------------
# Simulations assessing imperfect test sensitivity effects across testing frequencies assuming worker-worker transmission and leaky work schedules  
# Chris Hoover June 2021  
# -------------------------

library(parallel)

load("data/sim_workers.RData")
source("R/Utils.R")
source("R/Sim_Functions.R")
source("Analysis/0-Sim_Setup.R")

set.seed(430)

sim_grid_sens <- expand.grid(test_freq  = c(0,0.5,1,2,4),
                             delay      = c(0,1,2),
                             testsens   = c(1,0.95,0.9,0.85,0.8))

sim_grid_sens_expand <- as.data.frame(sim_grid_sens) %>% 
  slice(rep(1:n(), each = n_sims))

clooster <- makeCluster(detectCores()-1)

clusterExport(clooster, varlist = ls())

clusterEvalQ(clooster, library(tidyverse))
clusterEvalQ(clooster, library(triangle))

all_sims <- bind_rows(parLapply(cl = clooster,
                                X = 1:nrow(sim_grid_sens_expand),
                                fun = function(i){
                                  test_freq  = sim_grid_sens_expand[i,1]
                                  d          = sim_grid_sens_expand[i,2]
                                  testsens   = sim_grid_sens_expand[i,3]
                                  
                                  
                                  workers_use_char <- ifelse(test_freq == 0, "workers_leaky",
                                                                    ifelse(test_freq == 0.5, "workers_leaky_testday1_biweekly",
                                                                           ifelse(test_freq == 1, "workers_leaky_testday1",
                                                                                  ifelse(test_freq == 2, "workers_leaky_testday13", "workers_leaky_testday1234"))))
                                  
                                  workers_use <- get(workers_use_char)
                                  
                                  sim_work_transmission(Lambda    = lambda3*dt,
                                                        R_work    = R3,
                                                        R         = R3,
                                                        delay     = d,
                                                        test_thresh = 0,
                                                        test_spec = 1,
                                                        test_sens = testsens, 
                                                        workers   = workers_use,
                                                        sim_t     = sim_t,
                                                        dt        = dt,
                                                        verbose   = F)$cases_tests %>% 
                                    mutate(sim = i,
                                           lambda = lambda3, 
                                           R = R3,
                                           delay = d,
                                           testfreq = test_freq,
                                           testsens = testsens)
                                  
                                }))  

stopCluster(clooster)

# Summarise sims to totals at the end of the sim 
sims_end_sens <- all_sims %>% 
  group_by(sim, lambda, R, delay, testfreq, testsens) %>% 
  summarise(totcases = sum(exp_cases),
            totdays  = sum(inf_days),
            tottests = sum(tests_adm)) %>% 
  ungroup()

# Make sure there's the right number of summaries 
nrow(sims_end_sens) == nrow(sim_grid_sens_expand)

saveRDS(sims_end_sens, "data/testsens_sims_end_totals.RDS")