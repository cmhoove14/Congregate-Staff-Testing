# -------------------------
# Simulations assessing impact of self-isolation on performance of systematic testing strategie
# Chris Hoover June 2021  
# -------------------------

library(parallel)

load("data/sim_workers.RData")
source("R/Utils.R")
source("R/Sim_Functions.R")
source("Analysis/0-Sim_Setup.R")

set.seed(430)

p_iso_sens <- expand.grid(p_selfiso  = seq(0,1,0.1),
                          testsys    = c("systematic", "random"))

sim_grid_selfiso_expand <- as.data.frame(p_iso_sens) %>% 
  slice(rep(1:n(), each = n_sims))

clooster <- makeCluster(detectCores()-1)

clusterExport(clooster, varlist = ls())

clusterEvalQ(clooster, library(tidyverse))
clusterEvalQ(clooster, library(triangle))

all_sims <- bind_rows(parLapply(cl = clooster,
                                X = 1:nrow(sim_grid_selfiso_expand),
                                fun = function(i){
                                  p_iso    = sim_grid_selfiso_expand[i,1]
                                  testsys  = sim_grid_selfiso_expand[i,2]
                                  
                                  workers_use_char <- ifelse(testsys == "systematic",
                                                             "workers_leaky_testday1",
                                                             "workers_leaky_testday_r1")
                                  
                                  workers_use <- get(workers_use_char)

                                  sim_work_transmission(Lambda      = lambda2*dt,
                                                        Reff        = R2,
                                                        delay       = 0,
                                                        test_thresh = 0,
                                                        test_spec   = 1,
                                                        test_sens   = 1,
                                                        workers     = workers_use,
                                                        sim_t       = sim_t,
                                                        dt          = dt,
                                                        symps       = T,
                                                        p_symp      = 0.8,
                                                        p_selfiso   = p_iso,
                                                        verbose     = F)$cases_tests %>%
                                    mutate(sim = i,
                                           lambda = lambda2, 
                                           R = R2,
                                           work_sched = "leaky",
                                           delay = 0,
                                           testsys = testsys,
                                           testfreq = 1,
                                           delay = 0,
                                           p_selfiso = p_iso)
                                  
                                }))  

stopCluster(clooster)

# Summarise sims to totals at the end of the sim 
sims_end_iso <- all_sims %>% 
  group_by(sim, lambda, R, work_sched, testsys, testfreq, delay, p_selfiso) %>% 
  summarise(totcases = sum(exp_cases),
            adjcases = sum(adj_cases),
            totdays  = sum(inf_days),
            tottests = sum(tests_adm)) %>% 
  ungroup()

# Make sure there's the right number of summaries 
nrow(sims_end_iso) == length(p_iso_sens)

saveRDS(sims_end_iso, "data/selfiso_sims_end_totals.RDS")