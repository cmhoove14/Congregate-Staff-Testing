library(triangle)

infectious_profile <- function(t_latent, t_peak, t_infectious, dt){
  if(t_latent > t_peak){
    stop("Latent period ", t_latent,
         "is greater than incubation period ", t_peak)
  }
  t_tot     <- t_latent + t_infectious
  
  dtriangle(x = seq(0, t_tot, by = dt),
            a = t_latent,
            b = t_tot,
            c = t_peak)*dt
}


R_iso <- function(t_latent, t_peak, t_infectious, t_iso, R, dt){
  if(t_iso < t_latent){
    R_red <- 1
  } else {
    t_tot     <- t_latent + t_infectious
    
    i_tot <- dtriangle(x = seq(0, t_tot, by = dt),
                       a = t_latent,
                       b = t_tot,
                       c = t_peak)*dt
    
    if(sum(i_tot) != 1){
      warning("Triangle probability density does not equal 1")
    }
    
    R_red <- sum(i_tot[(t_iso/dt):(t_tot/dt)])
    
  }
    
  return(R*(1-R_red))
}

R_iso_f <- function(t_latent, t_peak, t_infectious, t_freq, d, dt, R){
  t_tot     <- t_latent + t_infectious
  t_eval    <- seq(t_latent+d, t_tot, by = dt)
  
  if(t_freq == 1 & d == 0 & dt == 1){
    
    p_t_test = rep(1, length(t_eval))
    
  } else {
    
    # Hack 1 into first entry because imprecision leading to near 0 (e^-16 numbers) in exponent leading to -Inf
    p_t_test <- c(1, 1 - (1-t_freq)^(t_eval[-1]-t_latent-d))
    
  }
  
  beta_t <- dtriangle(x = t_eval,
                      a = t_latent,
                      b = t_tot,
                      c = t_peak)*dt
  
  r_red <- R*sum(beta_t*p_t_test)
  
  if(round(r_red, 2) > 1 | round(r_red, 2) < 0){
    stop(cat("R reduction out of range, R_red =", r_red, "\n",
             "t_lat =", t_latent, "t_peak =",t_peak, "t_inf =",t_infectious, "f =",t_freq,"d =", d))
  }
  
  R_iso_f <- R - r_red
  
  return(R_iso_f)
}


test_workers <- function(test_indices, workers, timestep, test_thresh = 0, test_sens = 1, test_spec = 1, delay = 0){
  n_workers = length(test_indices)
  
  #Simulate false positives
  if(test_spec < 1){
    FP <- rbinom(n_workers, 1, 1-test_spec)
  }
  
  for(i in 1:n_workers){
    worker = test_indices[i]
    
    # If false positive test, worker enters delay then quarantine as if true positive. else progresses as normal
    if(test_spec < 1){
      
      if(FP[i] == 1){
        workers[[worker]]$state[timestep] <- "T"
        workers[[worker]]$delay <- delay
      }
      
    } else {
      # If worker actively infectious
      if(workers[[worker]]$state[timestep-1] == "I" &
         workers[[worker]]$state[timestep] != "R"){
        # If worker's infectiousness is greater than test threshold
        if(workers[[worker]]$infectiousness[workers[[worker]]$t_infect] > test_thresh){
          # If imperfect test sensitivity, check for false negative then assign state as tested if true positive, remain I if false negative
          if(test_sens < 1){
            FN <- rbinom(1, 1, 1-test_sens)
            new_state <- ifelse(FN == 1, "I", "T")
            new_delay <- ifelse(FN == 1, 0, delay)
            
            workers[[worker]]$state[timestep] <- new_state
            workers[[worker]]$delay <- new_delay
            # If perfect test sensitivity, infectious worker who is tested automatically changes states  
          } else {
            workers[[worker]]$state[timestep] <- "T"
            workers[[worker]]$delay <- delay
          }
        }  
      }  
    }
  }  
  
  return(workers)
}


q_025 <- function(vector){
  quantile(vector, 0.025)
}

q_25 <- function(vector){
  quantile(vector, 0.25)
}

q_75 <- function(vector){
  quantile(vector, 0.75)
}

q_975 <- function(vector){
  quantile(vector, 0.975)
}