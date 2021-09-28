# -------------------------
# Generate workers for microsimulation 
# Chris Hoover June 2021  
# -------------------------

source("R/Utils.R")
source("R/Sim_Functions.R")
source("Analysis/0-Sim_Setup.R")

set.seed(430)

# Base workers: no testing, leaky schedules ------
workers_leaky <- generate_workers_leaky(n_workers       = n_staff,
                                        sim_t           = sim_t,
                                        inf_pars_fx     = sim_inf_pars,  
                                        schedule_days   = schedule_days,
                                        schedule_shifts = schedule_shifts,
                                        testdays        = 0,
                                        test_freq       = 0)

# Workers with systematic testing, leaky schedules ---------
workers_leaky_testday1_biweekly <- generate_workers_leaky(n_workers       = n_staff,
                                                          sim_t           = sim_t,
                                                          inf_pars_fx     = sim_inf_pars,  
                                                          schedule_days   = schedule_days,
                                                          schedule_shifts = schedule_shifts,
                                                          testdays        = 1,
                                                          test_freq       = 0.5)

workers_leaky_testday1 <- generate_workers_leaky(n_workers       = n_staff,
                                                 sim_t           = sim_t,
                                                 inf_pars_fx     = sim_inf_pars,  
                                                 schedule_days   = schedule_days,
                                                 schedule_shifts = schedule_shifts,
                                                 testdays        = 1,
                                                 test_freq       = 1)

workers_leaky_testday2 <- generate_workers_leaky(n_workers       = n_staff,
                                                 sim_t           = sim_t,
                                                 inf_pars_fx     = sim_inf_pars,  
                                                 schedule_days   = schedule_days,
                                                 schedule_shifts = schedule_shifts,
                                                 testdays        = 2,
                                                 test_freq       = 1)

workers_leaky_testday3 <- generate_workers_leaky(n_workers       = n_staff,
                                                 sim_t           = sim_t,
                                                 inf_pars_fx     = sim_inf_pars,  
                                                 schedule_days   = schedule_days,
                                                 schedule_shifts = schedule_shifts,
                                                 testdays        = 3,
                                                 test_freq       = 1)

workers_leaky_testday4 <- generate_workers_leaky(n_workers       = n_staff,
                                                 sim_t           = sim_t,
                                                 inf_pars_fx     = sim_inf_pars,  
                                                 schedule_days   = schedule_days,
                                                 schedule_shifts = schedule_shifts,
                                                 testdays        = 4,
                                                 test_freq       = 1)

workers_leaky_testday5 <- generate_workers_leaky(n_workers       = n_staff,
                                                 sim_t           = sim_t,
                                                 inf_pars_fx     = sim_inf_pars,  
                                                 schedule_days   = schedule_days,
                                                 schedule_shifts = schedule_shifts,
                                                 testdays        = 5,
                                                 test_freq       = 1)

workers_leaky_testday13 <- generate_workers_leaky(n_workers       = n_staff,
                                                  sim_t           = sim_t,
                                                  inf_pars_fx     = sim_inf_pars,  
                                                  schedule_days   = schedule_days,
                                                  schedule_shifts = schedule_shifts,
                                                  testdays        = c(1,3),
                                                  test_freq       = 2)

workers_leaky_testday24 <- generate_workers_leaky(n_workers       = n_staff,
                                                  sim_t           = sim_t,
                                                  inf_pars_fx     = sim_inf_pars,  
                                                  schedule_days   = schedule_days,
                                                  schedule_shifts = schedule_shifts,
                                                  testdays        = c(2,4),
                                                  test_freq       = 2)

workers_leaky_testday1234 <- generate_workers_leaky(n_workers       = n_staff,
                                                    sim_t           = sim_t,
                                                    inf_pars_fx     = sim_inf_pars,  
                                                    schedule_days   = schedule_days,
                                                    schedule_shifts = schedule_shifts,
                                                    testdays        = c(1:4),
                                                    test_freq       = 4)

# Workers with random testing, leaky schedules ---------
# Single random every other week
workers_leaky_testday_r1_biweekly <- generate_workers_leaky_random(n_workers        = n_staff,
                                                                   sim_t            = sim_t,
                                                                   inf_pars_fx     = sim_inf_pars,  
                                                                   schedule_days    = schedule_days,
                                                                   schedule_shifts  = schedule_shifts,
                                                                   master_schedule  = master_schedule,
                                                                   test_freq        = 0.5)

# Single random day during the work week
workers_leaky_testday_r1 <- generate_workers_leaky_random(n_workers        = n_staff,
                                                          sim_t            = sim_t,
                                                          inf_pars_fx     = sim_inf_pars,  
                                                          schedule_days    = schedule_days,
                                                          schedule_shifts  = schedule_shifts,
                                                          master_schedule  = master_schedule,
                                                          test_freq        = 1)


# Two random days during the work week
workers_leaky_testday_r2 <- generate_workers_leaky_random(n_workers        = n_staff,
                                                          sim_t            = sim_t,
                                                          inf_pars_fx     = sim_inf_pars,  
                                                          schedule_days    = schedule_days,
                                                          schedule_shifts  = schedule_shifts,
                                                          master_schedule  = master_schedule,
                                                          test_freq        = 2)

# Four random days during the work week
workers_leaky_testday_r4 <- generate_workers_leaky_random(n_workers        = n_staff,
                                                          sim_t            = sim_t,
                                                          inf_pars_fx     = sim_inf_pars,  
                                                          schedule_days    = schedule_days,
                                                          schedule_shifts  = schedule_shifts,
                                                          master_schedule  = master_schedule,
                                                          test_freq        = 4)

# Base regular workers: no testing, regular schedules ------
workers_base <- generate_workers(n_workers       = n_staff,
                                 sim_t           = sim_t,
                                 inf_pars_fx     = sim_inf_pars,  
                                 schedule_days   = schedule_days,
                                 schedule_shifts = schedule_shifts,
                                 testdays        = 0,
                                 test_freq       = 0)

# Workers with systematic testing, reguylar schedules ---------
workers_testday1_biweekly <- generate_workers(n_workers       = n_staff,
                                              sim_t           = sim_t,
                                              inf_pars_fx     = sim_inf_pars,  
                                              schedule_days   = schedule_days,
                                              schedule_shifts = schedule_shifts,
                                              testdays        = 1,
                                              test_freq       = 0.5)

workers_testday1 <- generate_workers(n_workers       = n_staff,
                                     sim_t           = sim_t,
                                     inf_pars_fx     = sim_inf_pars,  
                                     schedule_days   = schedule_days,
                                     schedule_shifts = schedule_shifts,
                                     testdays        = 1,
                                     test_freq       = 1)

workers_testday2 <- generate_workers(n_workers       = n_staff,
                                     sim_t           = sim_t,
                                     inf_pars_fx     = sim_inf_pars,  
                                     schedule_days   = schedule_days,
                                     schedule_shifts = schedule_shifts,
                                     testdays        = 2,
                                     test_freq       = 1)

workers_testday3 <- generate_workers(n_workers       = n_staff,
                                     sim_t           = sim_t,
                                     inf_pars_fx     = sim_inf_pars,  
                                     schedule_days   = schedule_days,
                                     schedule_shifts = schedule_shifts,
                                     testdays        = 3,
                                     test_freq       = 1)

workers_testday4 <- generate_workers(n_workers       = n_staff,
                                     sim_t           = sim_t,
                                     inf_pars_fx     = sim_inf_pars,  
                                     schedule_days   = schedule_days,
                                     schedule_shifts = schedule_shifts,
                                     testdays        = 4,
                                     test_freq       = 1)

workers_testday5 <- generate_workers(n_workers       = n_staff,
                                     sim_t           = sim_t,
                                     inf_pars_fx     = sim_inf_pars,  
                                     schedule_days   = schedule_days,
                                     schedule_shifts = schedule_shifts,
                                     testdays        = 5,
                                     test_freq       = 1)

workers_testday13 <- generate_workers(n_workers       = n_staff,
                                      sim_t           = sim_t,
                                      inf_pars_fx     = sim_inf_pars,  
                                      schedule_days   = schedule_days,
                                      schedule_shifts = schedule_shifts,
                                      testdays        = c(1,3),
                                      test_freq       = 2)

workers_testday24 <- generate_workers(n_workers       = n_staff,
                                      sim_t           = sim_t,
                                      inf_pars_fx     = sim_inf_pars,  
                                      schedule_days   = schedule_days,
                                      schedule_shifts = schedule_shifts,
                                      testdays        = c(2,4),
                                      test_freq       = 2)

workers_testday1234 <- generate_workers(n_workers       = n_staff,
                                        sim_t           = sim_t,
                                        inf_pars_fx     = sim_inf_pars,  
                                        schedule_days   = schedule_days,
                                        schedule_shifts = schedule_shifts,
                                        testdays        = c(1:4),
                                        test_freq       = 4)

# Workers with random testing, regular schedules ---------
# Single random every other week
workers_testday_r1_biweekly <- generate_workers_random(n_workers        = n_staff,
                                                       sim_t            = sim_t,
                                                       inf_pars_fx     = sim_inf_pars,  
                                                       schedule_days    = schedule_days,
                                                       schedule_shifts  = schedule_shifts,
                                                       master_schedule  = master_schedule,
                                                       test_freq        = 0.5)

# Single random day during the work week
workers_testday_r1 <- generate_workers_random(n_workers        = n_staff,
                                              sim_t            = sim_t,
                                              inf_pars_fx     = sim_inf_pars,  
                                              schedule_days    = schedule_days,
                                              schedule_shifts  = schedule_shifts,
                                              master_schedule  = master_schedule,
                                              test_freq        = 1)


# Two random days during the work week
workers_testday_r2 <- generate_workers_random(n_workers        = n_staff,
                                              sim_t            = sim_t,
                                              inf_pars_fx     = sim_inf_pars,  
                                              schedule_days    = schedule_days,
                                              schedule_shifts  = schedule_shifts,
                                              master_schedule  = master_schedule,
                                              test_freq        = 2)

# Two random days during the work week
workers_testday_r4 <- generate_workers_random(n_workers        = n_staff,
                                              sim_t            = sim_t,
                                              inf_pars_fx     = sim_inf_pars,  
                                              schedule_days    = schedule_days,
                                              schedule_shifts  = schedule_shifts,
                                              master_schedule  = master_schedule,
                                              test_freq        = 4)


save.image("data/sim_workers.RData")