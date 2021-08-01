# -------------------------
# Summarizing base simulations
# Chris Hoover June 2021  
# -------------------------

library(tidyverse)

source("Analysis/0-Sim_Setup.R")
source("R/Utils.R")
sims_end <- readRDS("data/sims_end_totals.RDS")

# Summarise across all simulations for each parameter set
sims_sum <- sims_end %>% 
  group_by(lambda, R, work_sched, delay, testsys, testfreq) %>% 
  summarise(across(.cols = c("totcases", "totdays", "tottests"),
                   .fns  = list("median", "q_025", "q_25", "q_75", "q_975"))) %>% 
  ungroup()

# get expected cases in no-testing scenario for each transmission intensity, R combination, and work schedule type
# Filtering to no test delay too, but since no testing, delay won't matter. Just filtering so sample size is right
# lambda 1s
  # leaky
    l1_R1_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R1 & work_sched == "leaky" & delay == 0) %>% 
      pull(totcases_1)
    
    l1_R2_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R2 & work_sched == "leaky" & delay == 0) %>% 
      pull(totcases_1)
    
    l1_R3_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R3 & work_sched == "leaky" & delay == 0) %>% 
      pull(totcases_1)
    
  # cohorted
    l1_R1_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R1 & work_sched == "cohort" & delay == 0) %>% 
      pull(totcases_1)
    
    l1_R2_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R2 & work_sched == "cohort" & delay == 0) %>% 
      pull(totcases_1)
    
    l1_R3_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R3 & work_sched == "cohort" & delay == 0) %>% 
      pull(totcases_1)
  

# Lambda 2s
  # leaky
    l2_R1_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R1 & work_sched == "leaky" & delay == 0) %>% 
      pull(totcases_1)
    
    l2_R2_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R2 & work_sched == "leaky" & delay == 0) %>% 
      pull(totcases_1)
    
    l2_R3_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R3 & work_sched == "leaky" & delay == 0) %>% 
      pull(totcases_1)
    
  # cohorted
    l2_R1_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R1 & work_sched == "cohort" & delay == 0) %>% 
      pull(totcases_1)
    
    l2_R2_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R2 & work_sched == "cohort" & delay == 0) %>% 
      pull(totcases_1)
    
    l2_R3_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R3 & work_sched == "cohort" & delay == 0) %>% 
      pull(totcases_1)
    
# lambda 3s
  # leaky
    l3_R1_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R1 & work_sched == "leaky" & delay == 0) %>% 
      pull(totcases_1)
    
    l3_R2_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R2 & work_sched == "leaky" & delay == 0) %>% 
      pull(totcases_1)
    
    l3_R3_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R3 & work_sched == "leaky" & delay == 0) %>% 
      pull(totcases_1)
    
  # cohorted
    l3_R1_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R1 & work_sched == "cohort" & delay == 0) %>% 
      pull(totcases_1)
    
    l3_R2_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R2 & work_sched == "cohort" & delay == 0) %>% 
      pull(totcases_1)
    
    l3_R3_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R3 & work_sched == "cohort" & delay == 0) %>% 
      pull(totcases_1)
    
# Add cases avoided in reference to no-testing scenario
sims_sum2 <- sims_end %>% 
  mutate(avoidedpertest = case_when(lambda == lambda1 & R == R1 & work_sched == "leaky" ~ round((l1_R1_exp_cases_base_leaky-totcases) / tottests * 1000, 2),
                                    lambda == lambda1 & R == R2 & work_sched == "leaky" ~ round((l1_R2_exp_cases_base_leaky-totcases) / tottests * 1000, 2),
                                    lambda == lambda1 & R == R3 & work_sched == "leaky" ~ round((l1_R3_exp_cases_base_leaky-totcases) / tottests * 1000, 2),
                                    lambda == lambda2 & R == R1 & work_sched == "leaky" ~ round((l2_R1_exp_cases_base_leaky-totcases) / tottests * 1000, 2),
                                    lambda == lambda2 & R == R2 & work_sched == "leaky" ~ round((l2_R2_exp_cases_base_leaky-totcases) / tottests * 1000, 2),
                                    lambda == lambda2 & R == R3 & work_sched == "leaky" ~ round((l2_R3_exp_cases_base_leaky-totcases) / tottests * 1000, 2),
                                    lambda == lambda3 & R == R1 & work_sched == "leaky" ~ round((l3_R1_exp_cases_base_leaky-totcases) / tottests * 1000, 2),
                                    lambda == lambda3 & R == R2 & work_sched == "leaky" ~ round((l3_R2_exp_cases_base_leaky-totcases) / tottests * 1000, 2),
                                    lambda == lambda3 & R == R3 & work_sched == "leaky" ~ round((l3_R3_exp_cases_base_leaky-totcases) / tottests * 1000, 2),
                                    lambda == lambda1 & R == R1 & work_sched == "cohort" ~ round((l1_R1_exp_cases_base_cohort-totcases) / tottests * 1000, 2),
                                    lambda == lambda1 & R == R2 & work_sched == "cohort" ~ round((l1_R2_exp_cases_base_cohort-totcases) / tottests * 1000, 2),
                                    lambda == lambda1 & R == R3 & work_sched == "cohort" ~ round((l1_R3_exp_cases_base_cohort-totcases) / tottests * 1000, 2),
                                    lambda == lambda2 & R == R1 & work_sched == "cohort" ~ round((l2_R1_exp_cases_base_cohort-totcases) / tottests * 1000, 2),
                                    lambda == lambda2 & R == R2 & work_sched == "cohort" ~ round((l2_R2_exp_cases_base_cohort-totcases) / tottests * 1000, 2),
                                    lambda == lambda2 & R == R3 & work_sched == "cohort" ~ round((l2_R3_exp_cases_base_cohort-totcases) / tottests * 1000, 2),
                                    lambda == lambda3 & R == R1 & work_sched == "cohort" ~ round((l3_R1_exp_cases_base_cohort-totcases) / tottests * 1000, 2),
                                    lambda == lambda3 & R == R2 & work_sched == "cohort" ~ round((l3_R2_exp_cases_base_cohort-totcases) / tottests * 1000, 2),
                                    lambda == lambda3 & R == R3 & work_sched == "cohort" ~ round((l3_R3_exp_cases_base_cohort-totcases) / tottests * 1000, 2)),
        # ITER - Incrememntal Test Effectiveness Ratio: Number of tests necessary to avoid 1 transmission event 
         ITER = case_when(lambda == lambda1 & R == R1 & work_sched == "leaky" ~ round(tottests / (l1_R1_exp_cases_base_leaky-totcases), 2),
                          lambda == lambda1 & R == R2 & work_sched == "leaky" ~ round(tottests / (l1_R2_exp_cases_base_leaky-totcases), 2),
                          lambda == lambda1 & R == R3 & work_sched == "leaky" ~ round(tottests / (l1_R3_exp_cases_base_leaky-totcases), 2),
                          lambda == lambda2 & R == R1 & work_sched == "leaky" ~ round(tottests / (l2_R1_exp_cases_base_leaky-totcases), 2),
                          lambda == lambda2 & R == R2 & work_sched == "leaky" ~ round(tottests / (l2_R2_exp_cases_base_leaky-totcases), 2),
                          lambda == lambda2 & R == R3 & work_sched == "leaky" ~ round(tottests / (l2_R3_exp_cases_base_leaky-totcases), 2),
                          lambda == lambda3 & R == R1 & work_sched == "leaky" ~ round(tottests / (l3_R1_exp_cases_base_leaky-totcases), 2),
                          lambda == lambda3 & R == R2 & work_sched == "leaky" ~ round(tottests / (l3_R2_exp_cases_base_leaky-totcases), 2),
                          lambda == lambda3 & R == R3 & work_sched == "leaky" ~ round(tottests / (l3_R3_exp_cases_base_leaky-totcases), 2),
                          lambda == lambda1 & R == R1 & work_sched == "cohort" ~ round(tottests / (l1_R1_exp_cases_base_cohort-totcases), 2),
                          lambda == lambda1 & R == R2 & work_sched == "cohort" ~ round(tottests / (l1_R2_exp_cases_base_cohort-totcases), 2),
                          lambda == lambda1 & R == R3 & work_sched == "cohort" ~ round(tottests / (l1_R3_exp_cases_base_cohort-totcases), 2),
                          lambda == lambda2 & R == R1 & work_sched == "cohort" ~ round(tottests / (l2_R1_exp_cases_base_cohort-totcases), 2),
                          lambda == lambda2 & R == R2 & work_sched == "cohort" ~ round(tottests / (l2_R2_exp_cases_base_cohort-totcases), 2),
                          lambda == lambda2 & R == R3 & work_sched == "cohort" ~ round(tottests / (l2_R3_exp_cases_base_cohort-totcases), 2),
                          lambda == lambda3 & R == R1 & work_sched == "cohort" ~ round(tottests / (l3_R1_exp_cases_base_cohort-totcases), 2),
                          lambda == lambda3 & R == R2 & work_sched == "cohort" ~ round(tottests / (l3_R2_exp_cases_base_cohort-totcases), 2),
                          lambda == lambda3 & R == R3 & work_sched == "cohort" ~ round(tottests / (l3_R3_exp_cases_base_cohort-totcases), 2)),
         tests1000s = tottests/1000) %>% 
  group_by(lambda, R, work_sched, delay, testsys, testfreq) %>% 
  summarise(across(.cols = c("totcases", "totdays", "tests1000s", "avoidedpertest", "ITER"),
                   .fns  = list("median", "q_25", "q_75"))) %>% 
  ungroup() %>% 
  mutate(`Test Strategy` = if_else(testsys == "systematic", "Systematic", "Random"),
         `Test Strategy Delay` = paste0(`Test Strategy`, " - delay ", delay), 
         `Test Frequency`= as.factor(testfreq),
         `Test System`   = factor(case_when(testfreq == 0 ~ "None",
                                            testsys == "random" & testfreq == 0.5 ~ "Random 0.5",
                                            testsys == "random" & testfreq == 1 ~ "Random 1",
                                            testsys == "random" & testfreq == 2 ~ "Random 2",
                                            testsys == "random" & testfreq == 4 ~ "Random 4",
                                            testsys == "systematic" & testfreq == 0.5 ~ "Systematic 0.5",
                                            testsys == "systematic" & testfreq == 1 ~ "Systematic 1",
                                            testsys == "systematic" & testfreq == 2 ~ "Systematic 2",
                                            testsys == "systematic" & testfreq == 4 ~ "Systematic 4"),
                                  levels = c("None",
                                             "Random 0.5", "Systematic 0.5",
                                             "Random 1", "Systematic 1",
                                             "Random 2", "Systematic 2",
                                             "Random 4", "Systematic 4")),
         `Community Prevalence` = case_when(lambda == lambda1 ~ paste0(lambda1*100, "%"),
                                            lambda == lambda2 ~ paste0(lambda2*100, "%"),
                                            lambda == lambda3 ~ paste0(lambda3*100, "%")),
         `Rlab` = case_when(R == R1 ~ paste0("R = ", R1),
                            R == R2 ~ paste0("R = ", R2),
                            R == R3 ~ paste0("R = ", R3)))


sims_sum_ggplot <- sims_sum2 %>% 
  # Remove infinites in no test scenarios due to divide by 0 due to 0 tests
  mutate(avoidedpertest_1 = if_else(is.infinite(avoidedpertest_1), NA_real_, avoidedpertest_1),
         avoidedpertest_2 = if_else(is.infinite(avoidedpertest_2), NA_real_, avoidedpertest_2),
         avoidedpertest_3 = if_else(is.infinite(avoidedpertest_3), NA_real_, avoidedpertest_3)) %>% 
  pivot_longer(cols = totcases_1:ITER_3,
               names_sep = "_",
               names_to = c("measure", ".value")) %>% 
  rename("Med" = `1`,
         "q25" = `2`,
         "q75" = `3`) %>% 
  mutate(measure = factor(case_when(measure == "avoidedpertest" ~ "Transmissions avoided/\n1000 tests",
                                    measure == "totdays" ~ "Infectious Days",
                                    measure == "tests1000s" ~ "Tests (1000s)",
                                    measure == "totcases" ~ "Transmissions",
                                    measure == "ITER" ~ "ITER"), 
                          levels = c("Transmissions", "Infectious Days", "Tests (1000s)", "Transmissions avoided/\n1000 tests", "ITER")))

save.image(here::here("data/sim_results_processed.RData"))
