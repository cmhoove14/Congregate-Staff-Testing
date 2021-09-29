# Generate vector or matrix of infection trajectory parameters  
sim_inf_pars <- function(n){
  t_inc <- rlnorm(n, 1.63, 0.5)
  t_lnt <- t_inc - runif(n, 0, 2)
  t_inf <- runif(n, 7, 10)
  
  return(cbind(t_inc, t_lnt, t_inf))
}

# Parameters of incubation period for delta variant solved from reported mean and median reported in http://weekly.chinacdc.cn/fileCCDCW/journal/article/ccdcw/2021/27/PDF/Guangdongnote2.pdf assuming lognormal distribution
# mu = exp(median) ; reported median = 4.0
# mean = exp(mu+(s^2)/2) ; reported mean = 4.4, mu solved from above, solve for s^2
sim_inf_pars_delta <- function(n){
  t_inc <- rlnorm(n, 1.39, 0.18)
  t_lnt <- t_inc - runif(n, 0, 2)
  t_inf <- runif(n, 7, 10)
  
  return(cbind(t_inc, t_lnt, t_inf))
}

# plot(density(rlnorm(1000, 1.63, 0.5)), col = "grey30", ylim = c(0,0.5))
#   lines(density(rlnorm(1000, 1.39, 0.18)), col = "red")

# Generate workers with systematic test schedules
generate_workers <- function(n_workers, sim_t, inf_pars_fx, schedule_days, schedule_shifts, testdays, test_freq){
  lapply(1:n_workers, function(s){
    # Worker state and infectious profile parameters for if/when they become infected  
    state <- character(sim_t+1)
    state[1] <- "S"
    
    
    inf_pars <- inf_pars_fx(1)
    t_inc <- inf_pars[1]
    t_lnt <- inf_pars[2]
    t_inf <- inf_pars[3]
    
    # Worker schedule  
    # Randomly determine if worker works morning, night or day shift with equal probability  
    draw_shift <- runif(1,0,1)
    work_shift <- if_else(draw_shift < 1/3, "M",
                          if_else(draw_shift > 2/3, "N", "E"))
    
    # Randomly determine weekly schedule from four options: 1)Sat-Wed, 2)Tues-Sun,3)Thurs-Mon, 4)Mon-Fri with eaual probability
    draw_days <- runif(1,0,1)
    work_days <- if_else(draw_days < 1/4, "SUMTW",
                         if_else(draw_days > 3/4, "MTWRF", 
                                 if_else(draw_days > 1/4 & draw_days < 2/4, "TWRFS", "RFSUM")))
    
    # Binary work schedule
    shift10 <- if_else(work_shift == schedule_shifts, 1, 0)
    work10  <- if_else(schedule_days %in% unlist(strsplit(work_days, "")), 1, 0)
    
    schedule_fin <- if_else(shift10 + work10 == 2, 1, 0)
    
    # Binary test schedule
    test_schedule <- rep(0, length(schedule_days))
    
    if(test_freq > 0){
      days_tested <- sapply(testdays, function(d){
        substr(work_days,d,d)
      })
      
      test_schedule[which(schedule_days %in% days_tested & schedule_shifts %in% work_shift)] <- 1
      
      # Adjust if testing less than once per week by removing tests
      if(test_freq < 1){
        testdays_num  <- which(test_schedule == 1)   
        subweek_remove <- testdays_num[seq(1/test_freq, length(testdays_num), 1/test_freq)]
        test_schedule[subweek_remove] <- 0
      }
    }
    
    # Return worker characteristics in list  
    worker                <- list()
    worker$state          <- state
    worker$t_latent       <- t_lnt
    worker$t_incubation   <- t_inc
    worker$t_infectious   <- t_inf
    worker$infectiousness <- numeric()
    worker$t_infect       <- -Inf
    worker$delay          <- 0
    worker$work_schedule  <- schedule_fin
    worker$test_schedule  <- test_schedule
    
    return(worker)
    
  })
}

# Generate workers with systematic test schedule, one random work shift per week
generate_workers_leaky <- function(n_workers, sim_t, inf_pars_fx, schedule_days, schedule_shifts, testdays, test_freq){
  
  work_weeks <- ceiling(length(schedule_shifts)/length(unique(schedule_shifts))/7)
  
  lapply(1:n_workers, function(s){
    # Worker state and infectious profile parameters for if/when they become infected  
    state <- character(sim_t+1)
    state[1] <- "S"
    
    
    inf_pars <- inf_pars_fx(1)
    t_inc <- inf_pars[1]
    t_lnt <- inf_pars[2]
    t_inf <- inf_pars[3]
    
    # Worker schedule  
    # Randomly determine if worker works morning, night or day shift with equal probability  
    draw_shift <- runif(1,0,1)
    work_shift <- if_else(draw_shift < 1/3, "M",
                          if_else(draw_shift > 2/3, "N", "E"))
    
    # Randomly determine core weekly schedule from four options
    draw_days <- runif(1,0,1)
    work_days <- if_else(draw_days < 1/4, "SUMT",
                         if_else(draw_days > 3/4, "MTWR", 
                                 if_else(draw_days > 1/4 & draw_days < 2/4, "TWRF", "RFSU")))
    # Binary work schedule
    work_days_string <- sapply(1:nchar(work_days), function(x){substr(work_days,x,x)})
    
    work_days_schedule <- paste(work_days_string, work_shift, rep(c(1:work_weeks), each = nchar(work_days)), sep = "_")
    
    # Add in random shift
    rand_days   <- sample(c("U", "M", "T", "W", "R", "F", "S"), work_weeks, replace = T)
    rand_shifts <- sample(c("M", "N", "E"), work_weeks, replace = T)
    
    rand_days_schedule <- paste(rand_days, rand_shifts, c(1:work_weeks), sep = "_")
    
    # Enforce same number of work days each week by resampling for weeks where random shift scheduled is already a work shift
    while(sum(rand_days_schedule %in% work_days_schedule)){
      overlap_shifts <- rand_days_schedule[rand_days_schedule %in% work_days_schedule]
      
      redo_weeks <- sapply(overlap_shifts, function(i) as.numeric(substr(i, 5, 6)))
      
      redo_rand_days   <- sample(c("U", "M", "T", "W", "R", "F", "S"), length(redo_weeks), replace = T)
      redo_rand_shifts <- sample(c("M", "N", "E"), length(redo_weeks), replace = T)
      
      redo_shifts <- paste(redo_rand_days, redo_rand_shifts, redo_weeks, sep = "_")
      
      rand_days_schedule[which(rand_days_schedule %in% work_days_schedule)] <- redo_shifts
      
    }
    
    schedule_fin <- if_else(master_schedule %in% c(work_days_schedule, rand_days_schedule), 1, 0)
    
    # Binary test schedule
    test_schedule <- rep(0, length(schedule_days))
    
    if(test_freq > 0){
      days_tested <- sapply(testdays, function(d){
        substr(work_days,d,d)
      })
      
      test_schedule[which(schedule_days %in% days_tested & schedule_shifts %in% work_shift)] <- 1
      
      # Adjust if testing less than once per week by removing tests
      if(test_freq < 1){
        testdays_num  <- which(test_schedule == 1)   
        subweek_remove <- testdays_num[seq(1/test_freq, length(testdays_num), 1/test_freq)]
        test_schedule[subweek_remove] <- 0
      }
    }
    
    # Return worker characteristics in list  
    worker                <- list()
    worker$state          <- state
    worker$t_latent       <- t_lnt
    worker$t_incubation   <- t_inc
    worker$t_infectious   <- t_inf
    worker$infectiousness <- numeric()
    worker$t_infect       <- -Inf
    worker$delay          <- 0
    worker$work_schedule  <- schedule_fin
    worker$test_schedule  <- test_schedule
    
    return(worker)
    
  })
}

# Generate workers with random test schedules
generate_workers_random <- function(n_workers, sim_t, inf_pars_fx, schedule_days, schedule_shifts, master_schedule, test_freq){
  
  work_weeks <- ceiling(length(schedule_shifts)/length(unique(schedule_shifts))/7)
  
  lapply(1:n_workers, function(s){
    # Worker state and infectious profile parameters for if/when they become infected  
    state <- character(sim_t+1)
    state[1] <- "S"
    
    
    inf_pars <- inf_pars_fx(1)
    t_inc <- inf_pars[1]
    t_lnt <- inf_pars[2]
    t_inf <- inf_pars[3]
    
    # Worker schedule  
    # Randomly determine if worker works morning, night or day shift with equal probability  
    draw_shift <- runif(1,0,1)
    work_shift <- if_else(draw_shift < 1/3, "M",
                          if_else(draw_shift > 2/3, "N", "E"))
    
    # Randomly determine weekly schedule from four options: 1)Sat-Wed, 2)Tues-Sun,3)Thurs-Mon, 4)Mon-Fri with eaual probability
    draw_days <- runif(1,0,1)
    work_days <- if_else(draw_days < 1/4, "SUMTW",
                         if_else(draw_days > 3/4, "MTWRF", 
                                 if_else(draw_days > 1/4 & draw_days < 2/4, "TWRFS", "RFSUM")))
    
    # Binary work schedule
    shift10 <- if_else(work_shift == schedule_shifts, 1, 0)
    work10  <- if_else(schedule_days %in% unlist(strsplit(work_days, "")), 1, 0)
    
    schedule_fin <- if_else(shift10 + work10 == 2, 1, 0)
    
    # Binary test schedule with random days each work week
    work_days_string <- sapply(1:nchar(work_days), function(x){substr(work_days,x,x)})
    
    test_days <- unlist(lapply(1:work_weeks, function(x){sample(work_days_string, 
                                                                ifelse(test_freq < 1, 1, test_freq), 
                                                                replace = F)}))
    
    test_days_schedule <- paste(test_days, work_shift, rep(c(1:work_weeks), 
                                                           each = ifelse(test_freq < 1, 1, test_freq)), 
                                sep = "_")
    
    test_schedule <- if_else(master_schedule %in% test_days_schedule, 1, 0)
    
    # Adjust if only doing biweekly testing by removing every other test
    if(test_freq < 1){
      testdays_num  <- which(test_schedule == 1)   
      subweek_remove <- testdays_num[seq(1/test_freq, length(testdays_num), 1/test_freq)]
      test_schedule[subweek_remove] <- 0
    }
    
    # Return worker characteristics in list  
    worker                <- list()
    worker$state          <- state
    worker$t_latent       <- t_lnt
    worker$t_incubation   <- t_inc
    worker$t_infectious   <- t_inf
    worker$infectiousness <- numeric()
    worker$t_infect       <- -Inf
    worker$delay          <- 0
    worker$work_schedule  <- schedule_fin
    worker$test_schedule  <- test_schedule
    
    return(worker)
    
  })
}

# Generate workers with random test schedules, one random work shift per week
generate_workers_leaky_random <- function(n_workers, sim_t, inf_pars_fx, schedule_days, schedule_shifts, master_schedule, test_freq){
  
  work_weeks <- ceiling(length(schedule_shifts)/length(unique(schedule_shifts))/7)
  
  lapply(1:n_workers, function(s){
    # Worker state and infectious profile parameters for if/when they become infected  
    state <- character(sim_t+1)
    state[1] <- "S"
    
    
    inf_pars <- inf_pars_fx(1)
    t_inc <- inf_pars[1]
    t_lnt <- inf_pars[2]
    t_inf <- inf_pars[3]
    
    # Worker schedule  
    # Randomly determine if worker works morning, night or day shift with equal probability  
    draw_shift <- runif(1,0,1)
    work_shift <- if_else(draw_shift < 1/3, "M",
                          if_else(draw_shift > 2/3, "N", "E"))
    
    # Randomly determine core weekly schedule from four options
    draw_days <- runif(1,0,1)
    work_days <- if_else(draw_days < 1/4, "SUMT",
                         if_else(draw_days > 3/4, "MTWR", 
                                 if_else(draw_days > 1/4 & draw_days < 2/4, "TWRF", "RFSU")))
    # Binary work schedule
    work_days_string <- sapply(1:nchar(work_days), function(x){substr(work_days,x,x)})
    
    work_days_schedule <- paste(work_days_string, work_shift, rep(c(1:work_weeks), each = nchar(work_days)), sep = "_")
    
    # Add in random shift
    rand_days   <- sample(c("U", "M", "T", "W", "R", "F", "S"), work_weeks, replace = T)
    rand_shifts <- sample(c("M", "N", "E"), work_weeks, replace = T)
    
    rand_days_schedule <- paste(rand_days, rand_shifts, c(1:work_weeks), sep = "_")
    
    # Enforce same number of work days each week by resampling for weeks where random shift scheduled is already a work shift
    while(sum(rand_days_schedule %in% work_days_schedule)){
      overlap_shifts <- rand_days_schedule[rand_days_schedule %in% work_days_schedule]
      
      redo_weeks <- sapply(overlap_shifts, function(i) as.numeric(substr(i, 5, 6)))
      
      redo_rand_days   <- sample(c("U", "M", "T", "W", "R", "F", "S"), length(redo_weeks), replace = T)
      redo_rand_shifts <- sample(c("M", "N", "E"), length(redo_weeks), replace = T)
      
      redo_shifts <- paste(redo_rand_days, redo_rand_shifts, redo_weeks, sep = "_")
      
      rand_days_schedule[which(rand_days_schedule %in% work_days_schedule)] <- redo_shifts
      
    }
    
    schedule_fin <- if_else(master_schedule %in% c(work_days_schedule, rand_days_schedule), 1, 0)
    
    # Binary test schedule with random days each work week
    work_days_string <- sapply(1:nchar(work_days), function(x){substr(work_days,x,x)})
    
    test_days <- unlist(lapply(1:work_weeks, function(x){sample(work_days_string, 
                                                                ifelse(test_freq < 1, 1, test_freq), 
                                                                replace = F)}))
    test_days_schedule <- paste(test_days, work_shift, rep(c(1:work_weeks), 
                                                           each = ifelse(test_freq < 1, 1, test_freq)), 
                                sep = "_")
    
    test_schedule <- if_else(master_schedule %in% test_days_schedule, 1, 0)
    
    # Adjust if only doing biweekly testing by removing every other test
    if(test_freq < 1){
      testdays_num  <- which(test_schedule == 1)   
      subweek_remove <- testdays_num[seq(1/test_freq, length(testdays_num), 1/test_freq)]
      test_schedule[subweek_remove] <- 0
    }
    
    # Return worker characteristics in list  
    worker                <- list()
    worker$state          <- state
    worker$t_latent       <- t_lnt
    worker$t_incubation   <- t_inc
    worker$t_infectious   <- t_inf
    worker$infectiousness <- numeric()
    worker$t_infect       <- -Inf
    worker$delay          <- 0
    worker$work_schedule  <- schedule_fin
    worker$test_schedule  <- test_schedule
    
    return(worker)
    
  })
}

# Simulate transmission given parameters, time characteristics, and workers
sim_work_transmission <- function(Lambda, R_work, R, delay, test_thresh, test_sens, test_spec, workers, sim_t, dt, symps = FALSE, p_symp = 0.8, p_selfiso = 1, verbose = FALSE){
  
  inf_days  <- numeric(sim_t)
  exp_cases <- numeric(sim_t)
  tests_adm <- numeric(sim_t)
  
  for(t in 2:sim_t){
    # Advance infections ----------
    states       <- unlist(lapply(workers, function(w) w$state[t-1]))
    infecteds    <- which(states %in% c("E", "I", "T", "Q"))
    
    for(i in infecteds){
      # Advance time infected
      workers[[i]]$t_infect <- workers[[i]]$t_infect+1
      
      # Worker becomes infectious if past latent period and not yet tested/quarantined
      if(workers[[i]]$t_infect*dt > workers[[i]]$t_latent & 
         workers[[i]]$state[t-1] == "E"){
        workers[[i]]$state[t] <- "I"
      }
      
      # Worker is recovered if past end of infectious period
      if(workers[[i]]$t_infect > length(workers[[i]]$infectiousness)){
        workers[[i]]$state[t] <- "R"
      }
    }
    
    # Testing and isolation  -----------
    tested <- which(unlist(lapply(workers, function(w) w$test_schedule[t])) == 1) 
    tests_adm[t] <- length(tested)
    
    if(verbose){
      cat(tests_adm[t], "tests administered\n")
    }
    
    # Testing conducted. See Utils.R `test_workers` function for details
    if(length(tested) > 0){
      workers <- test_workers(test_indices = tested, workers = workers, timestep = t, 
                              test_thresh = test_thresh, test_sens = test_sens, test_spec = test_spec, delay = delay*1/dt)
    }
    
    states_updated <- unlist(lapply(workers, function(w) w$state[t]))
    
    # Test delay and quarantine on notification of positive if delay > 0. If delay=0, this will send positive workers just tested straight to quarantine
    for(i in which(states_updated == "T")){
      workers[[i]]$delay <- workers[[i]]$delay - 1
      if(workers[[i]]$delay <= 0){
        workers[[i]]$state[t] <- "Q"
        # Workers who are isolated after testing positive not tested for 90 days per guidance
        workers[[i]]$test_schedule[t:max(c(t+90*(1/dt), sim_t))] <- 0 
      }
    }
    
    # Self-isolation due to symptoms if modeling symptoms
    if(symps){
      for(i in which(states_updated == "I")){
        # Assume that symptoms start at time of peak infectiousness and that workers who self isolate do so upon symptom onset
        if(workers[[i]]$selfiso == 1 & workers[[i]]$t_infect >= which.max(workers[[i]]$infectiousness)){
          workers[[i]]$state[t] <- "Q"
        } 
      }
    }
    
    # New infections ------------
    # Determine who's working and infectious
    working <- which(unlist(lapply(workers, function(w) w$work_schedule[t])) == 1)
    infectors_t  <- which(states_updated %in% c("I", "T")) # Workers infectious or waiting on test will transmit
    inf_work_t   <- unlist(lapply(infectors_t, function(i) workers[[i]]$work_schedule[t]))
    infectious_t <- unlist(lapply(infectors_t, function(i) workers[[i]]$infectiousness[workers[[i]]$t_infect]))
    
    # FOIs
    Lambda_it          <- rep(Lambda*dt, length(workers))                     # Community infectivity
    Lambda_it[working] <- sum(inf_work_t*infectious_t*R_work)/length(working) # Workplace infectivity for those working. 
    # beta*I/N except beta*I is weighted by infectiousness and determined by workplace transmission, R_work
    
    # Bernoulli trials determine who is exposed, become infected if exposed and susceptible
    bernoullis <- rbinom(length(workers),1,Lambda_it)
    
    new_Is <- which(bernoullis == 1 & states == "S")
    
    # Assign characteristics for newly exposed
    if(length(new_Is) > 0){
      for(i in new_Is){
        workers[[i]]$state[t] <- "E"
        workers[[i]]$t_infect <- 0
        workers[[i]]$infectiousness <- infectious_profile(t_latent     = workers[[i]]$t_latent, 
                                                          t_peak       = workers[[i]]$t_incubation, 
                                                          t_infectious = workers[[i]]$t_infectious, 
                                                          dt           = dt)
        if(symps){
          # All we really care about is if someone ends up self-isolating due to symptoms, so only record selfiso variable
          symp                 <- rbinom(1,1,p_symp)
          workers[[i]]$selfiso <- ifelse(symp == 1, rbinom(1,1,p_selfiso), 0)
        }
      }
    }
    
    # Finalize advancement of states
    states_advanced <- unlist(lapply(workers, function(w) w$state[t]))
    unchanged <- which(states_advanced == "")
    for(i in unchanged){
      workers[[i]]$state[t] <- workers[[i]]$state[t-1]
    }
    
    
    # Infectious work days -------
    infectors_t  <- which(states_advanced %in% c("I", "T")) # Workers infectious or waiting on test will transmit
    inf_work_t   <- unlist(lapply(infectors_t, function(i) workers[[i]]$work_schedule[t]))
    infectious_t <- unlist(lapply(infectors_t, function(i) workers[[i]]$infectiousness[workers[[i]]$t_infect]))
    
    if(verbose){
      cat("S -", sum(states_advanced=="S"), "  ",
          "E -", sum(states_advanced=="E"), "  ",
          "I -", sum(states_advanced=="I"), "  ",
          "T -", sum(states_advanced=="T"), "  ",
          "Q -", sum(states_advanced=="Q"), "  ",
          "R -", sum(states_advanced=="R"), "\n")
    }
    
    exp_cases[t] <- sum(inf_work_t*infectious_t*R)
    inf_days[t]  <- sum(inf_work_t*infectious_t*R > 0)
  }
  
  out_cases_tests <- tibble("exp_cases" = exp_cases,
                            "inf_days"  = inf_days,
                            "tests_adm" = tests_adm,
                            "time"      = 1:sim_t)
  
  out_list <- list()
  out_list$workers   <- workers
  out_list$cases_tests <- out_cases_tests
  
  return(out_list)
} 

