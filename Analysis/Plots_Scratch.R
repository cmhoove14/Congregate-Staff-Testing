# -------------------------
# Plot scratch
# Chris Hoover June 2021  
# -------------------------

library(tidyverse)

source("Analysis/0-Sim_Setup.R")
source("R/Utils.R")
sims_end <- readRDS("data/sims_end_totals.RDS")

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


# Plot of all risk and normalized risk across comm prevalence and R across all testing strategies---------------------------
sims_sum_ggplot %>% 
  #filter(measure %in% c("Transmissions avoided/\n1000 tests", "Transmissions"), work_sched == "leaky") %>% 
  filter(measure == "Transmissions", work_sched == "leaky") %>% 
  # Convert weekly work schedule test frequencies to daily test frequency to match analytic results
  mutate(`Test Frequency` = factor(case_when(`Test Frequency` == 0 ~ "None",
                                             `Test Frequency` == 0.5 ~ "14",
                                             `Test Frequency` == 1 ~ "7",
                                             `Test Frequency` == 2 ~ "3.5",
                                             `Test Frequency` == 4 ~ "1"),
                                   levels = c("None", "14", "7", "3.5", "1"))) %>% 
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

ggsave(here::here("Plots/sim_results_3panel_R_CommPrev.png"),
       width = 7, height = 5.5, units = "in")


# Plot focusing on difference between systematic and random strategies---------------------------
sims_sum_ggplot %>% 
  filter(measure == "Transmissions", work_sched == "leaky", R == R2) %>% 
  # Convert weekly work schedule test frequencies to daily test frequency to match analytic results
  mutate(`Test Frequency` = factor(case_when(`Test Frequency` == 0 ~ "0",
                                             `Test Frequency` == 0.5 ~ "14",
                                             `Test Frequency` == 1 ~ "7",
                                             `Test Frequency` == 2 ~ "3.5",
                                             `Test Frequency` == 4 ~ "1"),
                                   levels = c("0", "14", "7", "3.5", "1"))) %>% 
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
  mutate(`Test Frequency` = factor(case_when(`Test Frequency` == 0 ~ "0",
                                             `Test Frequency` == 0.5 ~ "14",
                                             `Test Frequency` == 1 ~ "7",
                                             `Test Frequency` == 2 ~ "3.5",
                                             `Test Frequency` == 4 ~ "1"),
                                   levels = c("0", "14", "7", "3.5", "1"))) %>% 
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
