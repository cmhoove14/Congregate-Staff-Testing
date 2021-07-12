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
  group_by(lambda, R, work_sched, testsys, testfreq) %>% 
  summarise(across(.cols = c("totcases", "totdays", "tottests"),
                   .fns  = list("median", "q_025", "q_25", "q_75", "q_975"))) %>% 
  ungroup()

# get expected cases in no-testing scenario for each transmission intensity, R combination, and work schedule type
# lambda 1s
  # leaky
    l1_R1_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R1 & work_sched == "leaky") %>% 
      pull(totcases_1)
    
    l1_R2_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R2 & work_sched == "leaky") %>% 
      pull(totcases_1)
    
    l1_R3_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R3 & work_sched == "leaky") %>% 
      pull(totcases_1)
    
  # cohorted
    l1_R1_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R1 & work_sched == "cohort") %>% 
      pull(totcases_1)
    
    l1_R2_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R2 & work_sched == "cohort") %>% 
      pull(totcases_1)
    
    l1_R3_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda1 & R == R3 & work_sched == "cohort") %>% 
      pull(totcases_1)
  

# Lambda 2s
  # leaky
    l2_R1_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R1 & work_sched == "leaky") %>% 
      pull(totcases_1)
    
    l2_R2_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R2 & work_sched == "leaky") %>% 
      pull(totcases_1)
    
    l2_R3_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R3 & work_sched == "leaky") %>% 
      pull(totcases_1)
    
  # cohorted
    l2_R1_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R1 & work_sched == "cohort") %>% 
      pull(totcases_1)
    
    l2_R2_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R2 & work_sched == "cohort") %>% 
      pull(totcases_1)
    
    l2_R3_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda2 & R == R3 & work_sched == "cohort") %>% 
      pull(totcases_1)
    
# lambda 3s
  # leaky
    l3_R1_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R1 & work_sched == "leaky") %>% 
      pull(totcases_1)
    
    l3_R2_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R2 & work_sched == "leaky") %>% 
      pull(totcases_1)
    
    l3_R3_exp_cases_base_leaky <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R3 & work_sched == "leaky") %>% 
      pull(totcases_1)
    
  # cohorted
    l3_R1_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R1 & work_sched == "cohort") %>% 
      pull(totcases_1)
    
    l3_R2_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R2 & work_sched == "cohort") %>% 
      pull(totcases_1)
    
    l3_R3_exp_cases_base_cohort <- sims_sum %>% 
      filter(testfreq == 0 & lambda == lambda3 & R == R3 & work_sched == "cohort") %>% 
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
  group_by(lambda, R, work_sched, testsys, testfreq) %>% 
  summarise(across(.cols = c("totcases", "totdays", "tests1000s", "avoidedpertest", "ITER"),
                   .fns  = list("median", "q_25", "q_75"))) %>% 
  ungroup() %>% 
  mutate(`Test Strategy` = if_else(testsys == "systematic", "Systematic", "Random"),
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
  mutate(avoidedpertest_2 = if_else(is.infinite(avoidedpertest_2), NA_real_, avoidedpertest_2),
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


# Plot of all four metrics in base scenario with all testing strategies---------------------------
sims_sum_ggplot %>% 
  filter(R == 1, lambda == lambda2, measure != "Transmissions avoided/\n1000 tests") %>% 
  ggplot(aes(x = work_sched,
             y = Med,
             ymin = q25,
             ymax = q75,
             fill = `Test System`)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(position =position_dodge(width = 0.9)) +
  facet_wrap(.~measure, scales = "free_y") +
  scale_fill_manual(values = c("#ba7eff",
                               "#947a00",
                               "#823180",
                               "#71d9b1",
                               "#fd2872",
                               "#da7300",
                               "#fface5",
                               "#b50b01",
                               "#7a4522")) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12)) +
  labs(x = "Staff schedule", y = "Value")

ggsave(here::here("Plots/sim_results_bars.png"),
       width = 7, height = 7, units = "in")

save.image("data/sim_results_processed.RData")

# Plot of all risk and normalized risk across comm prevalence and R across all testing strategies---------------------------
sims_sum_ggplot %>% 
  #filter(measure %in% c("Transmissions avoided/\n1000 tests", "Transmissions"), work_sched == "leaky") %>% 
  filter(measure == "Transmissions", work_sched == "leaky") %>% 
  ggplot(aes(x = `Community Prevalence`,
             y = Med,
             ymin = q25,
             ymax = q75,
             shape = `Test Strategy`,
             col = `Test Frequency`)) +
    geom_point(position = position_dodge(0.5)) +
    geom_errorbar(width = 0.1, position = position_dodge(0.5)) +
    theme_classic() +
    facet_grid(.~Rlab, scales = "free_y", switch = "y") +
    scale_color_manual(values = c("darkred",
                                  "red",
                                  "#ff94b0",
                                  "#c9ca8c",
                                  "#008126")) +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 14),
          strip.placement = "outside",
          panel.border = element_rect(color = "grey50", fill = NA, size = 0.5),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10)) +
    labs(y = "")
  
ggsave(here::here("Plots/sim_results_8panel_R_CommPrev.png"),
       width = 7, height = 5.5, units = "in")

  
# Plot focusing on difference between systematic and random strategies---------------------------
sims_sum_ggplot %>% 
  filter(measure == "Transmissions", work_sched == "leaky", R == R2) %>% 
  ggplot(aes(x = `Community Prevalence`,
             y = Med,
             ymin = q25,
             ymax = q75,
             shape = `Test Strategy`,
             col = `Test Frequency`)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0.1, position = position_dodge(0.5)) +
  theme_classic() +
  #facet_grid(measure~Rlab, scales = "free_y", switch = "y") +
  scale_color_manual(values = c("darkred",
                                "red",
                                "#ff94b0",
                                "#c9ca8c",
                                "#008126")) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  labs(y = "Transmissions")

ggsave(here::here("Plots/sim_results_transmissions.png"),
       width = 5, height = 4, units = "in")

# Plot focusing on incremental test effectiveness ratio across transmission intensities---------------------------
sims_sum_ggplot %>% 
  filter(measure == "ITER", work_sched == "leaky", testsys == "systematic", testfreq > 0) %>% 
  ggplot(aes(x = `Community Prevalence`,
             y = Med,
             ymin = q25,
             ymax = q75,
             col = as.factor(R),
             shape = `Test Frequency`)) +
  geom_point(size = 1.2, position = position_dodge(0.5)) +
  geom_errorbar(width = 0.1, position = position_dodge(0.5)) +
  scale_y_continuous(trans = "log", 
                     breaks = c(25,100, 400, 800, 1600, 3200, 6400)) +
  scale_color_manual(values = c("#f68f46ff",
                                "darkred",
                                "#7e4e90ff")) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  labs(y = "ITER",
       col = expression(italic(R)))

ggsave(here::here("Plots/sim_results_ITER.png"),
       width = 5, height = 4, units = "in")


# Plot focusing on leaky versus cohorted strategies  -----------------------------------
sims_sum_ggplot %>% 
  filter(measure == "Transmissions", testsys == "systematic", testfreq > 0) %>% 
  ggplot(aes(x = `Community Prevalence`,
             y = Med,
             ymin = q25,
             ymax = q75,
             shape = work_sched,
             col = `Test Frequency`)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0.1, position = position_dodge(0.5)) +
  theme_classic() +
  facet_wrap(.~Rlab) +
  scale_color_manual(values = c("red",
                                "#ff94b0",
                                "#c9ca8c",
                                "#008126")) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14),
        strip.placement = "outside",
        panel.border = element_rect(color = "grey50", fill = NA, size = 0.5),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10)) +
  labs(y = "Transmissions",
       shape = "Schedule Type")

ggsave(here::here("Plots/sim_results_leaky_cohort_comp.png"),
       width = 6, height = 3, units = "in")



# Plot focusing on specific day of week among systematic strategy  -----------------------------------
dow_sims <- readRDS("data/dow_sims_totals.RDS")

dow_sums <- dow_sims %>% 
  mutate(tests1000s = tottests/1000) %>% 
  group_by(lambda, R, work_sched, testday, testfreq) %>% 
  summarise(across(.cols = c("totcases", "totdays", "tests1000s"),
                   .fns  = list("median", "q_25", "q_75"))) %>% 
  ungroup() %>% 
  pivot_longer(cols = totcases_1:tests1000s_3,
               names_sep = "_",
               names_to = c("measure", ".value")) %>% 
  rename("Med" = `1`,
         "q25" = `2`,
         "q75" = `3`) %>% 
  mutate(measure = factor(case_when(measure == "totdays" ~ "Infectious Days",
                                    measure == "tests1000s" ~ "Tests (1000s)",
                                    measure == "totcases" ~ "Transmissions"), 
                          levels = c("Transmissions", "Infectious Days", "Tests (1000s)")),
         `Community Prevalence` = case_when(lambda == lambda1 ~ paste0(lambda1*100, "%"),
                                            lambda == lambda2 ~ paste0(lambda2*100, "%"),
                                            lambda == lambda3 ~ paste0(lambda3*100, "%")),
         `Test Frequency`= as.factor(testfreq),
         `Rlab` = case_when(R == R1 ~ paste0("R = ", R1),
                            R == R2 ~ paste0("R = ", R2),
                            R == R3 ~ paste0("R = ", R3)))


dow_sums %>% 
  filter(measure == "Transmissions", testfreq == 1, R == R2) %>% 
  ggplot(aes(x = testday,
             y = Med,
             ymin = q25,
             ymax = q75,
             shape = work_sched)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0.1, position = position_dodge(0.5)) +
  theme_classic() +
  facet_wrap(.~`Community Prevalence`) +
  scale_color_manual(values = c("darkred",
                                "navy")) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        #strip.placement = "outside",
        panel.border = element_rect(color = "grey50", fill = NA, size = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  labs(y = "Transmissions",
       x = "Systematic Test Day",
       shape = "Schedule Type")

ggsave(here::here("Plots/sim_results_systematic_1pweek_DayOfWeek.png"),
       width = 6, height = 3, units = "in")
