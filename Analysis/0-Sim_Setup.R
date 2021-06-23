# --------------------------
# IBM Setup   
# Chris Hoover June 2021  
# --------------------------

library(tidyverse)

# Sim inputs
n_sims      <- 100
lambda1     <- 0.1/100
lambda2     <- 0.5/100
lambda3     <- 1/100
R1          <- 0.5
R2          <- 1.0
R3          <- 1.5
days        <- 180
dt          <- 8/24
sim_t       <- days*(1/dt)
n_staff     <- 700
staff_state <- rep("S", n_staff)

# Generate master schedule
work_weeks         <- ceiling(days/7)
schedule_shifts    <- rep(c("M", "E", "N"), times = days)
schedule_days      <- rep(rep(c("U", "M", "T", "W", "R", "F", "S"), each = 1/dt), 
                          times = work_weeks)[1:length(schedule_shifts)]
schedule_work_week <- rep(1:work_weeks, each = 7/dt)[1:length(schedule_shifts)]
master_schedule    <- paste(schedule_days, schedule_shifts, schedule_work_week, sep = "_")
