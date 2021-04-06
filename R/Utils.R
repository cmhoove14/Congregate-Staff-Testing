library(triangle)

# Parameters and transition functions  
N_staff   <- 500
comm_prev <- 0.005
asymp_frac <- 0.4

# Latent period (time between infection and infectiousness)
incubate_fx <- function(n){
  rgamma(n, 4,1)
}  

# Presymptomatic period (Time between infectiousness and symptoms among symptomatic. t_presymp+t_latent=t_incubation)
presymp_fx <- function(n){
  rgamma(n,10,7)
}

# Duration of infectious period for asymptomatic cases
asymp_fx <- function(n){
  rgamma(n, 3, 1)
}

# Duration of infectious period for symptomatic cases
symp_fx <- function(n){
  rgamma(n, 7, 1)
}

# 
gen_triangle <- function(peak_i, max_i, interval){
  dtriangle(seq(0, max_i, by = interval), a = 0, b = max_i, c = peak_i)
}
