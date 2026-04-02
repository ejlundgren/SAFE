# SMD

library(data.table)
library(MASS)
library(tmvtnorm)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. FORMULAS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_SMD <- function(x1, x2, sd1, sd2, n) {
  numerator <- x1 - x2
  # Pooled SD calculation
  pooled_sd <- sqrt( ((n - 1) * sd1^2 + (n - 1) * sd2^2) / (2 * n - 2) )
  return(numerator / pooled_sd)
}

calc_Hedges_g <- function(d, n) {
  df <- 2 * n - 2
  j_factor <- exp(lgamma(df/2) - log(sqrt(df/2)) - lgamma((df - 1)/2))
  return(j_factor * d)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. SIMULATION PARAMETERS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n_sim <- 10000        
N <- 5                
true_m1 <- 15         
true_m2 <- 14         
true_sd1 <- 2.3       
true_sd2 <- 1.7     
cor_levels <- c(0, 0.5, 0.8) 

# Theoretical "Truth" uses the pooled population SD
true_pooled_sd <- sqrt((true_sd1^2 + true_sd2^2) / 2)
true_yi <- (true_m1 - true_m2) / true_pooled_sd

results_list <- list()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. RUN SIMULATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(rho in cor_levels){
  
  sigma <- matrix(c(true_sd1^2, rho*true_sd1*true_sd2, 
                    rho*true_sd1*true_sd2, true_sd2^2), 2, 2)
  
  sim_out <- replicate(n_sim, {
    dat <- rtmvnorm(n = N, mean = c(true_m1, true_m2), sigma = sigma) 
    
    m1_hat <- mean(dat[,1])
    m2_hat <- mean(dat[,2])
    sd1_hat <- sd(dat[,1])
    sd2_hat <- sd(dat[,2])
    
    yi_plugin <- calc_SMD(m1_hat, m2_hat, sd1_hat, sd2_hat, N)
    
    # --- MINI SAFE PROCEDURE ---
    cloud_m1 <- rnorm(1000, m1_hat, sd1_hat/sqrt(N)) 
    cloud_m2 <- rnorm(1000, m2_hat, sd2_hat/sqrt(N))
    
    # We pass individual sample SDs to the plugin within the cloud
    cloud_yi <- calc_SMD(cloud_m1, cloud_m2, sd1_hat, sd2_hat, N)
    
    bias_safe <- mean(cloud_yi, na.rm=T) - yi_plugin
    yi_safe <- yi_plugin - bias_safe
    
    yi_hedges <- calc_Hedges_g(yi_plugin, N)
    
    return(c(yi_plugin = yi_plugin, yi_safe = yi_safe, yi_hedges = yi_hedges))
  })
  
  res <- as.data.table(t(sim_out))
  summary <- res[, .(
    r = rho,
    mean_plugin = mean(yi_plugin, na.rm=T),
    mean_safe = mean(yi_safe, na.rm=T),
    mean_hedges = mean(yi_hedges, na.rm=T),
    bias_plugin = mean(yi_plugin, na.rm=T) - true_yi,
    bias_safe = mean(yi_safe, na.rm=T) - true_yi,
    bias_hedges = mean(yi_hedges, na.rm=T) - true_yi
  )]
  
  results_list[[as.character(rho)]] <- summary
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. RESULTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
final_results <- rbindlist(results_list)
print(paste("Theoretical SMD Target:", round(true_yi, 4)))
print(final_results)

# lnRoM

library(data.table)
library(MASS)
library(tmvtnorm)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. FORMULAS (Directly from your remote_universal_SAFE.R)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_lnRoM <- function(x1, x2) {
  log(x1 / x2)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. SIMULATION PARAMETERS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n_sim <- 10000        # Iterations
N <- 5                # Small sample size
true_m1 <- 15         # Mean 1
true_m2 <- 10         # Mean 2
true_sd1 <- 2.2       # SD for Group 1
true_sd2 <- 1.8       # SD for Group 2
cor_levels <- c(0, 0.5, 0.8) 

# The "True" Estimand based on population parameters
true_yi <- log(true_m1 / true_m2)

results_list <- list()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. RUN SIMULATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(rho in cor_levels){
  
  # Covariance matrix using distinct SDs
  sigma <- matrix(c(true_sd1^2, rho*true_sd1*true_sd2, 
                    rho*true_sd1*true_sd2, true_sd2^2), 2, 2)
  
  sim_out <- replicate(n_sim, {
    dat <- rtmvnorm(n = N, mean = c(true_m1, true_m2), sigma = sigma, lower = c(0,0))
    
    m1_hat <- mean(dat[,1])
    m2_hat <- mean(dat[,2])
    sd1_hat <- sd(dat[,1])
    sd2_hat <- sd(dat[,2])
    
    yi_plugin <- calc_lnRoM(m1_hat, m2_hat)
    
    # --- MINI SAFE PROCEDURE ---
    # Cloud generation using individual sample SDs
    cloud_m1 <- rnorm(1000, m1_hat, sd1_hat/sqrt(N)) 
    cloud_m2 <- rnorm(1000, m2_hat, sd2_hat/sqrt(N))
    
    cloud_yi <- calc_lnRoM(cloud_m1, cloud_m2)
    bias_safe <- mean(cloud_yi) - yi_plugin
    yi_safe <- yi_plugin - bias_safe
    
    return(c(yi_plugin = yi_plugin, yi_safe = yi_safe))
  })
  
  res <- as.data.table(t(sim_out))
  summary <- res[, .(
    r = rho,
    mean_plugin = mean(yi_plugin, na.rm=T),
    mean_safe = mean(yi_safe, na.rm=T),
    bias_plugin = mean(yi_plugin, na.rm=T) - true_yi,
    bias_safe = mean(yi_safe, na.rm=T) - true_yi
  )]
  
  results_list[[as.character(rho)]] <- summary
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. RESULTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
final_results <- rbindlist(results_list)
print(paste("True lnRoM Target:", round(true_yi, 4)))
print(final_results)