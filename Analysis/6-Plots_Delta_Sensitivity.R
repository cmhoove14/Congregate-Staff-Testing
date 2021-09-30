# -------------------------
# Summarizing base simulations
# Chris Hoover June 2021  
# -------------------------

library(tidyverse)

source("Analysis/0-Sim_Setup.R")
source("R/Utils.R")

sims_end <- readRDS("data/sims_end_totals.RDS") %>% mutate(delta = 0) %>% relocate(delta, .before = totcases)
sims_end_delta <- readRDS("data/delta_sims_end_totals.RDS")

sims_end_comp <- bind_rows(sims_end, sims_end_delta)

# Summarise across all simulations for each parameter set
delta_sims_comp <- sims_end_comp %>% 
  filter(R == 1.5, work_sched == "leaky", testsys == "systematic") %>% 
  group_by(lambda, delay, testfreq, delta) %>% 
  summarise(across(.cols = c("totcases", "totdays", "tottests"),
                   .fns  = list("median", "q_025", "q_25", "q_75", "q_975"))) %>% 
  ungroup()

delta_sims_comp %>% 
  filter(delay == 0) %>% 
  ggplot(aes(x = as.factor(testfreq), y = totcases_1, col = as.factor(delta))) +
    geom_point(position = position_dodge(0.7)) +
    geom_errorbar(aes(ymin = totcases_3, ymax = totcases_4), position = position_dodge(0.7), width = 0.2) +
    facet_wrap(.~lambda) +
    theme_classic() +
    labs(x = "Test Frequency (weekly)",
         y = "Expected cases",
         col = "Delta Variant",
         title = "Delta variant sensitivity analysis across comm prevalence")

delta_sims_comp %>% 
  filter(lambda == lambda2) %>% 
  ggplot(aes(x = as.factor(testfreq), y = totcases_1, col = as.factor(delta))) +
  geom_point(position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = totcases_3, ymax = totcases_4), position = position_dodge(0.7), width = 0.2) +
  facet_wrap(.~delay) +
  theme_classic() +
  labs(x = "Test Frequency (weekly)",
       y = "Expected cases",
       col = "Delta Variant",
       title = "Delta variant sensitivity analysis across test delay")
