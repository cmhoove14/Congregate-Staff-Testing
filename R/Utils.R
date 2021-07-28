library(triangle)

infectious_profile <- function(t_latent, t_peak, t_infectious, dt){
  if(t_latent > t_peak){
    stop("Latent period ", t_latent,
         "is greater than incubation period ", t_peak)
  }
  t_tot     <- t_latent + (t_peak-t_latent) + t_infectious
  
  dtriangle(x = seq(0, t_tot, by = dt),
            a = t_latent,
            b = t_tot,
            c = t_peak)
}

R_iso <- function(t_latent, t_peak, t_infectious, t_iso, R){
  t_tot     <- t_latent + (t_peak-t_latent) + t_infectious

  if(t_iso <= t_latent){
    
    R_red <- 1
    
  } else {
    
    d_iso <- dtriangle(x = t_iso,
                       a = t_latent,
                       b = t_tot,
                       c = t_peak)
    
    if(t_iso > t_peak){
      
      # Area of triangle made by t_iso
      R_red <- d_iso*(t_tot - t_iso)/2 
      
    } else {
      
      d_peak <- dtriangle(x = t_peak,
                          a = t_latent,
                          b = t_tot,
                          c = t_peak)
      
      # Area of triangle made by t_peak to t_tot plus area of trapezoid between t_iso and t_peak
      R_red <- d_peak*(t_tot - t_peak)/2 + (d_iso+d_peak)*(t_peak - t_iso)/2
      
    }
    
  }
  
  return(R*(1-R_red))
}

R_iso_f <- function(t_latent, t_peak, t_infectious, t_freq, d, dt, R){
  t_tot     <- t_latent + (t_peak-t_latent) + t_infectious
  t_eval    <- seq(t_latent, t_tot, by = dt)
  
  beta_t <- dtriangle(x = t_eval,
                      a = t_latent,
                      b = t_tot,
                      c = t_peak)*dt
  
  if(round(sum(beta_t),2) != 1.0){
    warning("PDF summed to", sum(beta_t))
  }
  
  p_t_test <- (1-t_freq)^(t_eval-t_latent-d)
  
  p_t_test[(t_eval-t_latent) < d] <- 1
  
  r_red <- sum(beta_t*p_t_test)
  
  if(r_red > 1 | r_red < 0){
    stop(cat("R reduction out of range, R_red =", r_red))
  }
  
  R_iso_f <- R * r_red
  
  return(R_iso_f)
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