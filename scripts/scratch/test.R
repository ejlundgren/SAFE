
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
true_m1 <- 13.4         
true_m2 <- 16.1         
true_sd1 <- 4.6       
true_sd2 <- 3.9     
cor_levels <- c(0, 0.5, 0.8) 

# Theoretical "Truth" uses the pooled population SD
true_pooled_sd <- sqrt((true_sd1^2 + true_sd2^2) / 2)
true_yi <- (true_m1 - true_m2) / true_pooled_sd

results_list <- list()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. RUN SIMULATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(rho in cor_levels){
  
  sigma <- matrix(c(true_sd1^2, 
                    rho*true_sd1*true_sd2, 
                    rho*true_sd1*true_sd2, 
                    true_sd2^2), 
                  2, 2)
  
  sim_out <- replicate(n_sim, {
    dat <- rtmvnorm(n = N, 
                    mean = c(true_m1, true_m2), 
                    sigma = sigma) 
    
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
true_m1 <- 13.4         # Mean 1
true_m2 <-  16.1         # Mean 2
true_sd1 <- 4.6       # SD for Group 1
true_sd2 <- 3.9       # SD for Group 2
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


# TEST with EJL functions. No SAFE -------------------------------------------------
source("scripts/SAFE_function.R")
#

data_generation <- function(scens, lower_filter){
  
  conditional <- paste("group1 >", lower_filter, "& group2 >", lower_filter)
  
  lapply(1:nrow(scens), function(x){
    
    sig <- matrix(
      c(scens$true_sd1[x]^2,
        scens$r[x] * scens$true_sd1[x] * scens$true_sd2[x],
        scens$r[x] * scens$true_sd1[x] * scens$true_sd2[x],
        scens$true_sd2[x]^2),
      nrow = 2, ncol = 2, byrow = TRUE
    )
    
    means <- c(m1 = scens$true_mean1[x],
               m2 = scens$true_mean2[x])
    
    # Need to do in a while loop.
    sim_length <- 0
    out <- list()
    index <- 1
    
    while(sim_length < scens$n[x]){
      out[[index]] <- MASS::mvrnorm(n = scens$n[x],
                                    mu = means,
                                    Sigma = sig) |>
        as.data.frame() |>
        setDT()
      setnames(out[[index]], 
               c("m1", "m2"),
               c("group1", "group2"))
      
      out[[index]] <- out[[index]][eval(parse(text = conditional)), ]
      sim_length <- sapply(out, nrow) |> sum()
      index <- index + 1
    }
    out <- rbindlist(out)
    out <- out[1:scens$n[x], ]
    
    #
    cor <- cor.test(out$group1, out$group2)
    
    out <- data.table(sim_mean1 = mean(out$group1),
                      sim_mean2 = mean(out$group2),
                      sim_sd1 = sd(out$group1),
                      sim_sd2 = sd(out$group2),
                      sim_r = cor$estimate,
                      sim_n = nrow(out))
    return(out)
  }) |> rbindlist()
}

# Set up scenarios
scenario <- CJ(true_mean1 = 13.4,         
               true_mean2 = 16.1,         
               true_sd1 = 4.6,       
               true_sd2 = 3.9,     
               n = c(5),
               r = c(0, 0.5, 0.8) )
#
scenario

# scenario[, true_lnRoM := log(true_mean1 / true_mean2)]
smd_truth <- eff_size(x1 = scenario$true_mean1,
                  x2 = scenario$true_mean2,
                  sd1 = scenario$true_sd1,
                  sd2 = scenario$true_sd2,
                  r = scenario$r,
                  n = scenario$n,
                  SAFE = FALSE,
                  verbose = FALSE,
                  effect_type = "SMD_paired")
setnames(smd_truth, "yi_first", "yi_truth")
smd_truth <- cbind(scenario, smd_truth[, .(yi_truth)])
smd_truth[, effect_type := "SMD_paired"]

#
rom_truth <- eff_size(x1 = scenario$true_mean1,
                      x2 = scenario$true_mean2,
                      sd1 = scenario$true_sd1,
                      sd2 = scenario$true_sd2,
                      r = scenario$r,
                      n = scenario$n,
                      SAFE = FALSE,
                      verbose = FALSE,
                      effect_type = "lnRoM_paired")
setnames(rom_truth, "yi_first", "yi_truth")
rom_truth <- cbind(scenario, rom_truth[, .(yi_truth)])
rom_truth[, effect_type := "lnRoM_paired"]


# lnCVR
cvr_truth <- eff_size(x1 = scenario$true_mean1,
                      x2 = scenario$true_mean2,
                      sd1 = scenario$true_sd1,
                      sd2 = scenario$true_sd2,
                      r = scenario$r,
                      n = scenario$n,
                      SAFE = FALSE,
                      verbose = FALSE,
                      effect_type = "lnCVR_paired")
setnames(cvr_truth, "yi_first", "yi_truth")
cvr_truth <- cbind(scenario, cvr_truth[, .(yi_truth)])
cvr_truth[, effect_type := "lnCVR_paired"]

master_scenarios <- rbind(smd_truth, rom_truth, cvr_truth)
master_scenarios

#
full_res <- list()
res <- list()
i <- 1
scenario <- c()
types <- unique(master_scenarios$effect_type)
k <- 1

for(k in 1:length(types)){
  scenario <- master_scenarios[effect_type == types[k]]
  
  for(i in 1:1000){
    set.seed(i)
    dat <- data_generation(scenario,
                           lower_filter = -Inf)
    
    res[[i]] <- cbind(scenario,
                      eff_size(x1 = dat$sim_mean1,
                               x2 = dat$sim_mean2,
                               sd1 = dat$sim_sd1,
                               sd2 = dat$sim_sd2,
                               r = dat$sim_r,
                               n = scenario$n,
                               SAFE = TRUE,
                               SAFE_boots = 1e3,
                               verbose = FALSE,
                               effect_type = unique(scenario$effect_type)))
    
    cat(i, "/", 1000, "\r")
    
  }
  full_res[[k]] <- rbindlist(res)  
  
}


# 
# res[[i]]
full_res <- rbindlist(full_res, fill = TRUE)

# res[yi_alt != yi_first]
# Should be 0 rows. OK. It is.

#
summary <- full_res[, .(bias_plugin = mean(yi_first) - yi_truth,
                        bias_safe = mean(yi_safe) - yi_truth),
                         by = .(r, n, effect_type)] |> unique()
summary



final_results

