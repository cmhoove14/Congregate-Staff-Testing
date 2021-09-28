# -------------------------
# Simulations across systematic day of week  
# Chris Hoover June 2021  
# -------------------------

library(parallel)

source("R/Utils.R")
source("R/Sim_Functions.R")
source("Analysis/0-Sim_Setup.R")
load("data/sim_workers.RData")

set.seed(430)

# Systematic day of week sims  --------------- 
sims_dow <- expand.grid(test_freq  = c(0,1),
                        test_day   = c(0:4),
                        work_sched = c("leaky", "cohort"),
                        comm_prev  = c(lambda1,lambda2,lambda3),
                        R          = c(R1, R2, R3))

# Remove redundant 
sims_dow <- sims_dow[-which(sims_dow$test_freq == 0 & sims_dow$test_day > 0),]
sims_dow <- sims_dow[-which(sims_dow$test_freq > 0 & sims_dow$test_day == 0),]

sims_dow_expand <- as.data.frame(sims_dow) %>% 
  slice(rep(1:n(), each = n_sims))

clooster2 <- makeCluster(detectCores()-1)

clusterExport(clooster2, varlist = ls())

clusterEvalQ(clooster2, library(tidyverse))
clusterEvalQ(clooster2, library(triangle))

dow_sims <- bind_rows(parLapply(cl = clooster2,
                                X = 1:nrow(sims_dow_expand),
                                fun = function(i){
                                  test_freq  = sims_dow_expand[i,1]
                                  test_day   = sims_dow_expand[i,2]
                                  work_sched = sims_dow_expand[i,3]
                                  comm_prev  = sims_dow_expand[i,4]
                                  R          = sims_dow_expand[i,5]
                                  
                                  if(work_sched == "leaky"){
                                    workers_use_char <- case_when(test_day == 0 & test_freq == 0 ~ "workers_leaky",
                                                                  test_day == 1 & test_freq == 1 ~ "workers_leaky_testday1",
                                                                  test_day == 2 & test_freq == 1 ~ "workers_leaky_testday2",
                                                                  test_day == 3 & test_freq == 1 ~ "workers_leaky_testday3",
                                                                  test_day == 4 & test_freq == 1 ~ "workers_leaky_testday4")
                                  } else if(work_sched == "cohort"){
                                    workers_use_char <- case_when(test_day == 0 & test_freq == 0 ~ "workers_base",
                                                                  test_day == 1 & test_freq == 1 ~ "workers_testday1",
                                                                  test_day == 2 & test_freq == 1 ~ "workers_testday2",
                                                                  test_day == 3 & test_freq == 1 ~ "workers_testday3",
                                                                  test_day == 4 & test_freq == 1 ~ "workers_testday4")
                                    
                                  } else {
                                    # Will throw error below
                                    workers_use_char <- NULL
                                  }
                                  
                                  workers_use <- get(workers_use_char)
                                  
                                  sim_work_transmission(Lambda    = comm_prev*dt,
                                                        R_work    = R,
                                                        R         = R,
                                                        delay     = 0,
                                                        test_thresh = 0,
                                                        test_sens = 1,
                                                        test_spec = 1,
                                                        workers   = workers_use,
                                                        sim_t     = sim_t,
                                                        dt        = dt,
                                                        verbose   = F)$cases_tests %>% 
                                    mutate(sim = i,
                                           lambda = comm_prev, 
                                           R = R,
                                           work_sched = work_sched,
                                           testsys = "systematic",
                                           testday = test_day,
                                           testfreq = test_freq)
                                  
                                }))  

stopCluster(clooster2)


# Summarise sims to totals at the end of the sim 
dows_end <- dow_sims %>% 
  group_by(sim, lambda, R, work_sched, testsys, testday, testfreq) %>% 
  summarise(totcases = sum(exp_cases),
            totdays  = sum(inf_days),
            tottests = sum(tests_adm)) %>% 
  ungroup()

# Make sure there's the right number of summaries 
nrow(dows_end) == nrow(sims_dow_expand)

saveRDS(dows_end, "data/dow_sims_totals.RDS")
