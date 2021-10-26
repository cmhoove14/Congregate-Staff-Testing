library(tidyverse)
source(here::here("R/Utils.R"))
source(here::here("R/Sim_Functions.R"))

# Setup -------------------
# Sim inputs
#comm_prev   <- 0.005
days        <- 180
dt          <- 8/24 # Resolution is an 8-hour work shift
sim_t       <- days*(1/dt)
exp_cases   <- numeric(length = sim_t)
n_staff     <- 700
staff_state <- rep("S", n_staff)
Lambda      <- 0.01*dt
alpha       <- 1

# Generate master schedule
schedule_shifts <- rep(c("M", "E", "N"), times = days)
schedule_days   <- rep(rep(c("U", "M", "T", "W", "R", "F", "S"), each = 3), 
                       times = ceiling(days/7))[1:length(schedule_shifts)]
master_schedule <- paste(schedule_days, schedule_shifts, sep = "_")

# Setup baseline workers with no testing --------------------
# Generate list of workers with no testing for model simulation
workers <- lapply(1:n_staff, function(s){
  # Worker state and infectious profile parameters for if/when they become infected  
  state <- "S"
  t_inc <- rlnorm(1, 1.63,0.5)
  t_lnt <- t_inc + runif(1, -2.5, 0)
  t_inf <- t_lnt + runif(1, 6.5, 9.5)
  
  # Worker schedule  
  # Randomly determine if worker works morning, night or day shift with equal probability  
  draw_shift <- runif(1,0,1)
  work_shift <- if_else(draw_shift < 1/3, "M",
                        if_else(draw_shift > 2/3, "N", "E"))
  
  # Randomly determine weekly schedule from four options: 1)Sat-Tues, 2)Tues-Sat,3)Thurs-Sun, 4)Mon-Thurs with eaul probability
  draw_days <- runif(1,0,1)
  work_days <- if_else(draw_days < 1/4, "SUMT",
                       if_else(draw_days > 3/4, "MTWR", 
                               if_else(draw_days > 1/4 & draw_days < 2/4, "TWRF", "RFSU")))
  
  # Binary work schedule
  shift10 <- if_else(work_shift == schedule_shifts, 1, 0)
  work10  <- if_else(schedule_days %in% unlist(strsplit(work_days, "")), 1, 0)
  
  schedule_fin <- if_else(shift10 + work10 == 2, 1, 0)
  
  # Binary test schedule  
  test_schedule <- rep(0, sim_t)
  
  # Return worker characteristics in list  
  worker                <- list()
  worker$state          <- state
  worker$t_latent       <- t_lnt
  worker$t_incubation   <- t_inc
  worker$t_infectious   <- t_inf
  worker$infectiousness <- numeric()
  worker$t_infect       <- -Inf
  worker$work_schedule  <- schedule_fin
  worker$test_schedule  <- test_schedule
  
  return(worker)
  
})

# Setup workers with testing on first day of weekly shift --------------------
# Generate list of workers with testing on first day of shift for model simulation
workers_testday1 <- lapply(1:n_staff, function(s){
  # Worker state and infectious profile parameters for if/when they become infected  
  state <- "S"
  t_inc <- rlnorm(1, 1.63,0.5)
  t_lnt <- t_inc + runif(1, -2.5, 0)
  t_inf <- t_lnt + runif(1, 6.5, 9.5)
  
  # Worker schedule  
  # Randomly determine if worker works morning, night or day shift with equal probability  
  draw_shift <- runif(1,0,1)
  work_shift <- if_else(draw_shift < 1/3, "M",
                        if_else(draw_shift > 2/3, "N", "E"))
  
  # Randomly determine weekly schedule from four options: 1)Sat-Tues, 2)Tues-Sat,3)Thurs-Sun, 4)Mon-Thurs with eaul probability
  draw_days <- runif(1,0,1)
  work_days <- if_else(draw_days < 1/4, "SUMT",
                       if_else(draw_days > 3/4, "MTWR", 
                               if_else(draw_days > 1/4 & draw_days < 2/4, "TWRF", "RFSU")))
  
  # Binary work schedule
  shift10 <- if_else(work_shift == schedule_shifts, 1, 0)
  work10  <- if_else(schedule_days %in% unlist(strsplit(work_days, "")), 1, 0)
  
  schedule_fin <- if_else(shift10 + work10 == 2, 1, 0)
  
  # Binary test schedule with test occurring on day 1 of work shift
  test_schedule <- if_else(substr(work_days,1,1) == schedule_days & work_shift == schedule_shifts,
                           1, 0 )
  
  # Return worker characteristics in list  
  worker                <- list()
  worker$state          <- state
  worker$t_latent       <- t_lnt
  worker$t_incubation   <- t_inc
  worker$t_infectious   <- t_inf
  worker$infectiousness <- numeric()
  worker$t_infect       <- -Inf
  worker$work_schedule  <- schedule_fin
  worker$test_schedule  <- test_schedule
  
  return(worker)
  
})

workers_testday2 <- lapply(1:n_staff, function(s){
  # Worker state and infectious profile parameters for if/when they become infected  
  state <- "S"
  t_inc <- rlnorm(1, 1.63,0.5)
  t_lnt <- t_inc + runif(1, -2.5, 0)
  t_inf <- t_lnt + runif(1, 6.5, 9.5)
  
  # Worker schedule  
  # Randomly determine if worker works morning, night or day shift with equal probability  
  draw_shift <- runif(1,0,1)
  work_shift <- if_else(draw_shift < 1/3, "M",
                        if_else(draw_shift > 2/3, "N", "E"))
  
  # Randomly determine weekly schedule from four options: 1)Sat-Tues, 2)Tues-Sat,3)Thurs-Sun, 4)Mon-Thurs with eaul probability
  draw_days <- runif(1,0,1)
  work_days <- if_else(draw_days < 1/4, "SUMT",
                       if_else(draw_days > 3/4, "MTWR", 
                               if_else(draw_days > 1/4 & draw_days < 2/4, "TWRF", "RFSU")))
  
  # Binary work schedule
  shift10 <- if_else(work_shift == schedule_shifts, 1, 0)
  work10  <- if_else(schedule_days %in% unlist(strsplit(work_days, "")), 1, 0)
  
  schedule_fin <- if_else(shift10 + work10 == 2, 1, 0)
  
  # Binary test schedule with test occurring on day 1 of work shift
  test_schedule <- if_else(substr(work_days,2,2) == schedule_days & work_shift == schedule_shifts,
                           1, 0 )
  
  # Return worker characteristics in list  
  worker                <- list()
  worker$state          <- state
  worker$t_latent       <- t_lnt
  worker$t_incubation   <- t_inc
  worker$t_infectious   <- t_inf
  worker$infectiousness <- numeric()
  worker$t_infect       <- -Inf
  worker$work_schedule  <- schedule_fin
  worker$test_schedule  <- test_schedule
  
  return(worker)
  
})

workers_testday3 <- lapply(1:n_staff, function(s){
  # Worker state and infectious profile parameters for if/when they become infected  
  state <- "S"
  t_inc <- rlnorm(1, 1.63,0.5)
  t_lnt <- t_inc + runif(1, -2.5, 0)
  t_inf <- t_lnt + runif(1, 6.5, 9.5)
  
  # Worker schedule  
  # Randomly determine if worker works morning, night or day shift with equal probability  
  draw_shift <- runif(1,0,1)
  work_shift <- if_else(draw_shift < 1/3, "M",
                        if_else(draw_shift > 2/3, "N", "E"))
  
  # Randomly determine weekly schedule from four options: 1)Sat-Tues, 2)Tues-Sat,3)Thurs-Sun, 4)Mon-Thurs with eaul probability
  draw_days <- runif(1,0,1)
  work_days <- if_else(draw_days < 1/4, "SUMT",
                       if_else(draw_days > 3/4, "MTWR", 
                               if_else(draw_days > 1/4 & draw_days < 2/4, "TWRF", "RFSU")))
  
  # Binary work schedule
  shift10 <- if_else(work_shift == schedule_shifts, 1, 0)
  work10  <- if_else(schedule_days %in% unlist(strsplit(work_days, "")), 1, 0)
  
  schedule_fin <- if_else(shift10 + work10 == 2, 1, 0)
  
  # Binary test schedule with test occurring on day 1 of work shift
  test_schedule <- if_else(substr(work_days,3,3) == schedule_days & work_shift == schedule_shifts,
                           1, 0 )
  
  # Return worker characteristics in list  
  worker                <- list()
  worker$state          <- state
  worker$t_latent       <- t_lnt
  worker$t_incubation   <- t_inc
  worker$t_infectious   <- t_inf
  worker$infectiousness <- numeric()
  worker$t_infect       <- -Inf
  worker$work_schedule  <- schedule_fin
  worker$test_schedule  <- test_schedule
  
  return(worker)
  
})

workers_testday4 <- lapply(1:n_staff, function(s){
  # Worker state and infectious profile parameters for if/when they become infected  
  state <- "S"
  t_inc <- rlnorm(1, 1.63,0.5)
  t_lnt <- t_inc + runif(1, -2.5, 0)
  t_inf <- t_lnt + runif(1, 6.5, 9.5)
  
  # Worker schedule  
  # Randomly determine if worker works morning, night or day shift with equal probability  
  draw_shift <- runif(1,0,1)
  work_shift <- if_else(draw_shift < 1/3, "M",
                        if_else(draw_shift > 2/3, "N", "E"))
  
  # Randomly determine weekly schedule from four options: 1)Sat-Tues, 2)Tues-Sat,3)Thurs-Sun, 4)Mon-Thurs with eaul probability
  draw_days <- runif(1,0,1)
  work_days <- if_else(draw_days < 1/4, "SUMT",
                       if_else(draw_days > 3/4, "MTWR", 
                               if_else(draw_days > 1/4 & draw_days < 2/4, "TWRF", "RFSU")))
  
  # Binary work schedule
  shift10 <- if_else(work_shift == schedule_shifts, 1, 0)
  work10  <- if_else(schedule_days %in% unlist(strsplit(work_days, "")), 1, 0)
  
  schedule_fin <- if_else(shift10 + work10 == 2, 1, 0)
  
  # Binary test schedule with test occurring on day 1 of work shift
  test_schedule <- if_else(substr(work_days,4,4) == schedule_days & work_shift == schedule_shifts,
                           1, 0 )
  
  # Return worker characteristics in list  
  worker                <- list()
  worker$state          <- state
  worker$t_latent       <- t_lnt
  worker$t_incubation   <- t_inc
  worker$t_infectious   <- t_inf
  worker$infectiousness <- numeric()
  worker$t_infect       <- -Inf
  worker$work_schedule  <- schedule_fin
  worker$test_schedule  <- test_schedule
  
  return(worker)
  
})

# alpha =0  scenarios --------------------
n_sims <- 100

no_test_sims_a0 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 0,
                        R       = 1,
                        workers = workers,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 0,
           testday = "None")
}))

# Alpha 0 (all transmission) from community and test on first day of work week
test_day1_sims_a0 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 0,
                        R       = 1,
                        workers = workers_testday1,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 0,
           testday = "1")
}))

test_day2_sims_a0 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 0,
                        R       = 1,
                        workers = workers_testday2,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 0,
           testday = "2")
}))

test_day3_sims_a0 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 0,
                        R       = 1,
                        workers = workers_testday3,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 0,
           testday = "3")
}))

test_day4_sims_a0 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 0,
                        R       = 1,
                        workers = workers_testday4,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 0,
           testday = "4")
}))

# no_test_sims_sum <- no_test_sims %>% 
#   group_by(time) %>% 
#   summarise(across(.cols = c("exp_cases", "tests_adm"),
#                    .fns  = list("median", "q_025", "q_25", "q_75", "q_975")))


# alpha =1  scenarios --------------------
n_sims <- 100

no_test_sims_a1 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 1,
                        R       = 1,
                        workers = workers,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 1,
           testday = "None")
}))

test_day1_sims_a1 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 1,
                        R       = 1,
                        workers = workers_testday1,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 1,
           testday = "1")
}))

test_day2_sims_a1 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 1,
                        R       = 1,
                        workers = workers_testday2,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 1,
           testday = "2")
}))

test_day3_sims_a1 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 1,
                        R       = 1,
                        workers = workers_testday3,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 1,
           testday = "3")
}))

test_day4_sims_a1 <- bind_rows(lapply(1:n_sims, function(x){
  sim_work_transmission(Lambda  = 0.01,
                        alpha   = 1,
                        R       = 1,
                        workers = workers_testday4,
                        sim_t   = sim_t,
                        dt      = dt,
                        verbose = F) %>% 
    mutate(sim = x,
           alpha = 1,
           testday = "4")
}))

# Put together sim results and plot -------------
sims_sum <- bind_rows(
  no_test_sims_a0,
  test_day1_sims_a0,
  test_day2_sims_a0,
  test_day3_sims_a0,
  test_day4_sims_a0,
  no_test_sims_a1,
  test_day1_sims_a1,
  test_day2_sims_a1,
  test_day3_sims_a1,
  test_day4_sims_a1
) %>% 
  group_by(time, alpha, testday) %>% 
  summarise(across(.cols = c("exp_cases", "tests_adm"),
                   .fns  = list("median", "q_025", "q_25", "q_75", "q_975")))

sims_sum %>% 
  ggplot() +
    geom_smooth(aes(x = time, y = exp_cases_1,
                    col = testday)) +
    facet_wrap(alpha~.) +
    theme_classic()

ggsave("Plots/Initial_Sims_exp_cases_time.png",
       height = 4, width = 6, units = "in")


sims_sum2 <- bind_rows(
  no_test_sims_a0,
  test_day1_sims_a0,
  test_day2_sims_a0,
  test_day3_sims_a0,
  test_day4_sims_a0,
  no_test_sims_a1,
  test_day1_sims_a1,
  test_day2_sims_a1,
  test_day3_sims_a1,
  test_day4_sims_a1
) %>% 
  group_by(sim, alpha, testday) %>% 
  summarise(across(.cols = c("exp_cases", "tests_adm"),
                   .fns  = list("sum")))

sims_sum2 %>% 
  ggplot(aes(x = as.factor(alpha), y = exp_cases_1, fill = testday)) +
    geom_violin() +
    # stat_summary(fun = "median",
    #              geom = "crossbar",size = 0.2, width = 0.7) +
    theme_classic()

ggsave("Plots/Initial_Sims_exp_cases_tot.png",
       height = 4, width = 6, units = "in")
