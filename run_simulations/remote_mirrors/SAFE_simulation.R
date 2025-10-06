# July 17th 2025
#
#
# Remote simulations, including of B length
#
#
#
# 0. Prepare environment --------------------------------------------------

rm(list = ls())

library("data.table")
library("MASS")
library("tmvtnorm")

local <- FALSE

if(local){
  setwd("remote_mirrors/final_simulations//")
  
  index <- 1 # This is the chunk number
  
  # setwd("/Users/ejlundgren/GenomeDK/meta_megafauna/meta_simulations/")
  source('remote_universal_SAFE.R')
  
  scenarios <- readRDS("data/scenarios.Rds")
  length(unique(scenarios$run_ID))
  
  scenarios[,.SD[1], by = .(effect_type)]$run_ID
  
  scenarios[, .(n_boots = uniqueN(boots)), by = run_ID][n_boots > 1, ]
  # ABSOLUTELY MUST BE 0 rows
  guide <- scenarios[run_ID == "SMD_Wishart_1000_run850"]
  scens <- copy(guide) # for inside functions
  length(unique(scens$scenario_id))
  unique(scens$boots)

}else{
  
  scenarios <- readRDS("data/working_scenarios.Rds")
  
  # get model number for this iteration
  args <- commandArgs()
  print(args)
  
  index <- as.numeric(args[6]) # get index value from bash script
  source('remote_universal_SAFE.R')
  
  # Chunk should be same for an entire effect size / runID combination
  guide <- scenarios[chunk == index, ]
}

print(paste(nrow(guide), "scenarios to run"))

guide

# stopifnot(nrow(guide) == 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# >>> Encapsulate each effect size sim ----------------------------------------------------
#' *Each effect size is encapsulated given their unique data simulation details*
# Each function takes a subset of the scenario guide as the object 'scens'

reciprocal <- function(scens){
  
  # Simulate data for each scens
  sim_dat <- lapply(1:nrow(scens), function(x){
    y <-  rnorm(n = scens$sample_size[x],
                mean = scens$true_mean[x],
                sd = scens$true_sd[x])
    
    sim_dat <- data.table(sim_mean = mean(y),
                          sim_sd = sd(y),
                          sim_n = length(y))
    
  }) |> rbindlist()
  
  # Calculate true effect sizes:
  true_point <- eff_size(n = scens$sample_size,
                         x = scens$true_mean,
                         sd = scens$true_sd,
                         effect_type = "reciprocal",
                         SAFE = TRUE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  # change names:
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_safe", "vi_safe"),
           c("true_y_plugin_1st", "true_v_plugin_1st", 
             "true_yi_safe", "true_vi_safe"))
  
  # Calculate effect sizes off of simulated data:
  out <- eff_size(x = sim_dat$sim_mean,
                  sd = sim_dat$sim_sd, #' [we used to use sd_of_hyperpopulation. But this needs to be clarified programmatically]
                  n = sim_dat$sim_n,
                  effect_type = "reciprocal",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  
  # Change names:
  setnames(out,
           c("yi_first", "vi_first"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st"))
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        true_point,
                        out)
  
  return(results)
  
}

lnRoM <- function(scens){
  
  sim_dat <- lapply(1:nrow(scens), function(x){
    
    # Simulate data for scens
    sig <- diag(c(scens$true_sd1[x]^2,
                  scens$true_sd2[x]^2))
    
    means <- c(m1 = scens$true_mean1[x],
               m2 = scens$true_mean2[x])
    
    y <- rtmvnorm(n = max(c(scens$sample_size1[x], scens$sample_size2[x])),
                  mean = means,
                  sigma = sig,
                  lower = rep(0, length(means)),
                  upper = rep(Inf, length(means)),
                  algorithm = "gibbs") |>
      as.data.frame() |>
      setDT()
    names(y) <- c("m1", "m2")
    
    #' [Filter to number of samples per treatment]
    sim_dat <- list(x1 = y[1:scens$sample_size1[x], ]$m1,
                    x2 = y[1:scens$sample_size2[x], ]$m2)
    
    sim_dat <- data.table(sim_mean1 = mean(sim_dat$x1),
                          sim_mean2 = mean(sim_dat$x2),
                          sim_sd1 = sd(sim_dat$x1),
                          sim_sd2 = sd(sim_dat$x2),
                          sim_n1 = length(sim_dat$x1),
                          sim_n2 = length(sim_dat$x2))
    return(sim_dat)
  }) |> rbindlist()
  
  # Calculate true effect sizes
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "lnRoM",
                         SAFE = TRUE,
                         verbose = FALSE,
                         SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second",
             "yi_safe", "vi_safe"),
           c("true_y_plugin_1st", "true_v_plugin_1st", 
             "true_y_plugin_2nd", "true_v_plugin_2nd",
             "true_yi_safe", "true_vi_safe"))
  
  # Calculate simulated effect sizes
  out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n1 = scens$sample_size1, n2 = scens$sample_size2, 
                  effect_type = "lnRoM",
                  SAFE = TRUE,
                  verbose = FALSE,
                  SAFE_boots = unique(scens$boots))
  setnames(out,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st",
             "sim_y_plugin_2nd", "sim_v_plugin_2nd"))
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        true_point,
                        out)
  
  return(results)
  
}

SMD <- function(scens){
  
  # Simulate data:
  sim_dat <- lapply(1:nrow(scens), function(x){
    sig <- diag(c(scens$true_sd1[x]^2,
                  scens$true_sd2[x]^2))
    
    y <- mvrnorm(n = max(c(scens$sample_size1[x], scens$sample_size2[x])),
                 mu = c(m1 = scens$true_mean1[x],
                        m2 = scens$true_mean2[x]),
                 Sigma = sig) |> 
      as.data.frame() |> 
      setDT()
    sim_dat <- list(x1 = y$m1[1:scens$sample_size1[x]],
                    x2 = y$m2[1:scens$sample_size2[x]])
    
    sim_dat <- data.table(sim_mean1 = mean(sim_dat$x1),
                          sim_mean2 = mean(sim_dat$x2),
                          sim_sd1 = sd(sim_dat$x1),
                          sim_sd2 = sd(sim_dat$x2),
                          sim_n1 = length(sim_dat$x1),
                          sim_n2 = length(sim_dat$x2))
    sim_dat
  }) |> rbindlist()
  sim_dat
  
  # Calculate true effect sizes
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "SMD",
                         SAFE_distribution = "4_multivariate_normal",
                         SAFE = TRUE,
                         verbose = FALSE,
                         SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second",
             "yi_safe", "vi_safe"),
           c("true_y_plugin_1st", "true_v_plugin_1st", 
             "true_y_plugin_2nd", "true_v_plugin_2nd",
             "true_yi_safe", "true_vi_safe"))
  
  # Calculate simulated effect sizes
  out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n1 = scens$sample_size1, n2 = scens$sample_size2, 
                  effect_type = "SMD",
                  SAFE_distribution = "4_multivariate_normal",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  setnames(out,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st",
             "sim_y_plugin_2nd", "sim_v_plugin_2nd"))
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        true_point,
                        out)
  
  return(results)
}


SMD_Wishart <- function(scens){
  
  # Simulate data for scen
  sim_dat <- lapply(1:nrow(scens), function(x){
    sig <- diag(c(scens$true_sd1[x]^2,
                  scens$true_sd2[x]^2))
    
    y <- mvrnorm(n = scens$sample_size1[x],
                 mu = c(m1 = scens$true_mean1[x],
                        m2 = scens$true_mean2[x]),
                 Sigma = sig) |> 
      as.data.frame() |> 
      setDT()
    
    sim_dat <- list(x1 = y$m1[1:scens$sample_size1[x]],
                    x2 = y$m2[1:scens$sample_size2[x]])
    
    sim_dat <- data.table(sim_mean1 = mean(sim_dat$x1),
                          sim_mean2 = mean(sim_dat$x2),
                          sim_sd1 = sd(sim_dat$x1),
                          sim_sd2 = sd(sim_dat$x2),
                          sim_n1 = length(sim_dat$x1),
                          sim_n2 = length(sim_dat$x2))
    sim_dat
  }) |> rbindlist()
  
  # Calculate true effect sizes
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "SMD",
                         SAFE_distribution = "4_multivariate_normal_wishart",
                         SAFE = TRUE,
                         verbose = FALSE,
                         SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second",
             "yi_safe", "vi_safe"),
           c("true_y_plugin_1st", "true_v_plugin_1st", 
             "true_y_plugin_2nd", "true_v_plugin_2nd",
             "true_yi_safe", "true_vi_safe"))
  
  # Calculate simulated effect sizes
  out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n1 = scens$sample_size1, n2 = scens$sample_size2, 
                  effect_type = "SMD",
                  SAFE_distribution = "4_multivariate_normal_wishart",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  setnames(out,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st",
             "sim_y_plugin_2nd", "sim_v_plugin_2nd"))
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        true_point,
                        out)
  
  return(results)
}

lnOR <- function(scens){
  
  # Simulate data:
  sim_dat <- lapply(1:nrow(scens),
                    function(x){
                      y <- data.table(sim_a = sum(rbinom(scens$n1[x], 1, scens$true_p_a[x])),
                                      sim_c = sum(rbinom(scens$n2[x], 1, scens$true_p_c[x])))
                      y[, `:=` (sim_b = scens$n1[x] - sim_a,
                                sim_d = scens$n2[x] - sim_c)]
                      
                      # Add 0.5 to rows with ANY zero
                      y[(sim_a == 0 | sim_b == 0 | sim_c == 0 | sim_d == 0), 
                        `:=` (sim_a = sim_a + 0.5,
                              sim_b = sim_b + 0.5,
                              sim_c = sim_c + 0.5,
                              sim_d = sim_d + 0.5)]
                      
                      return(y)
                    }) |> 
    rbindlist()
  
  # Calculate true effect sizes
  true_point <- eff_size(a = scens$true_a, b = scens$true_b,
                         c = scens$true_c,  d = scens$true_d,
                         effect_type = "lnOR",
                         SAFE_distribution = "4_binomial",
                         SAFE = TRUE,
                         verbose = FALSE,
                         SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first",
             "yi_safe", "vi_safe"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_yi_safe", "true_vi_safe"))

  # Calculate simulated effect sizes
  out <- eff_size(a = sim_dat$sim_a, b = sim_dat$sim_b,
                  c = sim_dat$sim_c,  d = sim_dat$sim_d,
                  effect_type = "lnOR",
                  SAFE_distribution = "4_binomial",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  
  setnames(out,
           c("yi_first", "vi_first"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st"))
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        true_point,
                        out)
  
  return(results)
  
}

lnRR <- function(scens){
  #' [Simulate data:]
  # Add 0.5 just to affected group and 1 to n, unlike with OR (because a, b, c, d require symmetry but lnRR doens't)
  sim_dat <- lapply(1:nrow(scens),
                    function(x){
                      y <- data.table(sim_a = sum(rbinom(scens$n1[x], 1, scens$true_p_a[x]) |> as.double()),
                                      sim_c = sum(rbinom(scens$n2[x], 1, scens$true_p_c[x]) |> as.double())) # TOOD - this is modifed
                      y[, `:=` (sim_n1 = scens$n1[x],
                                sim_n2 = scens$n2[x])]
                      y[sim_a == 0, `:=` (sim_a = sim_a + 0.5, 
                                          sim_n1 = sim_n1 + 1)]
                      y[sim_c == 0, `:=` (sim_c = sim_c + 0.5, 
                                          sim_n2 = sim_n2 + 1)]
                      
                      return(y)
                    }) |> 
    rbindlist()
  
  
  true_point <- eff_size(a = scens$true_a, c = scens$true_c,
                         n1 = scens$n1,  n2 = scens$n2,
                         effect_type = "lnRR",
                         SAFE_distribution = "2_binomial",
                         SAFE = TRUE,
                         verbose = FALSE,
                         SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first",
             "yi_safe", "vi_safe"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_yi_safe", "true_vi_safe"))
  
  # a = sim_dat$sim_a; c = sim_dat$sim_c
  # n1 = sim_dat$n1;  n2 = sim_dat$n2
  
  out <- eff_size(a = sim_dat$sim_a, c = sim_dat$sim_c,
                  n1 = sim_dat$sim_n1,  n2 = sim_dat$sim_n2,
                  effect_type = "lnRR",
                  SAFE_distribution = "2_binomial",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  
  setnames(out,
           c("yi_first", "vi_first"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st"))
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        true_point,
                        out)
  
  return(results)

}

lnCVR <- function(scens){
  
  # Simulate data:
  sim_dat <- lapply(1:nrow(scens), function(x){
    sig <- diag(c(scens$true_sd1[x]^2,
                  scens$true_sd2[x]^2))
    
    means <- c(m1 = scens$true_mean1[x],
               m2 = scens$true_mean2[x])
    y <- rtmvnorm(n = max(c(scens$sample_size1[x], scens$sample_size2[x])),
                  mean = means,
                  sigma = sig,
                  lower = rep(0, length(means)),
                  upper = rep(Inf, length(means)),
                  algorithm = "gibbs") |>
      as.data.frame() |>
      setDT()
    names(y) <- c("m1", "m2")
    
    sim_dat <- list(x1 = y$m1[1:scens$sample_size1[x]],
                    x2 = y$m2[1:scens$sample_size2[x]])
    
    sim_dat <- data.table(sim_mean1 = mean(sim_dat$x1),
                          sim_mean2 = mean(sim_dat$x2),
                          sim_sd1 = sd(sim_dat$x1),
                          sim_sd2 = sd(sim_dat$x2),
                          sim_n1 = length(sim_dat$x1),
                          sim_n2 = length(sim_dat$x2))
    sim_dat
  }) |> rbindlist()
  sim_dat
  
  # Calculate true effect sizes
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "lnCVR",
                         SAFE_distribution = "4_multivariate_normal",
                         SAFE = TRUE,
                         verbose = FALSE,
                         SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second",
             "yi_safe", "vi_safe"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_y_plugin_2nd", "true_v_plugin_2nd",
             "true_yi_safe", "true_vi_safe"))
  
  # Calculate simulated effect sizes
  out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n1 = scens$sample_size1, n2 = scens$sample_size2, 
                  effect_type = "lnCVR",
                  SAFE_distribution = "4_multivariate_normal",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  
  setnames(out,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st",
             "sim_y_plugin_2nd", "sim_v_plugin_2nd"))
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        true_point,
                        out)
  
  return(results)

}

lnCVR_Wishart <- function(scens){
  
  # Simulate data:
  sim_dat <- lapply(1:nrow(scens), function(x){
    sig <- diag(c(scens$true_sd1[x]^2,
                  scens$true_sd2[x]^2))
    
    means <- c(m1 = scens$true_mean1[x],
               m2 = scens$true_mean2[x])
    
    y <- rtmvnorm(n = max(c(scens$sample_size1[x], scens$sample_size2[x])),
                  mean = means,
                  sigma = sig,
                  lower = rep(0, length(means)),
                  upper = rep(Inf, length(means)),
                  algorithm = "gibbs") |>
      as.data.frame() |>
      setDT()
    names(y) <- c("m1", "m2")
    
    sim_dat <- list(x1 = y$m1[1:scens$sample_size1[x]],
                    x2 = y$m2[1:scens$sample_size2[x]])
    
    sim_dat <- data.table(sim_mean1 = mean(sim_dat$x1),
                          sim_mean2 = mean(sim_dat$x2),
                          sim_sd1 = sd(sim_dat$x1),
                          sim_sd2 = sd(sim_dat$x2),
                          sim_n1 = length(sim_dat$x1),
                          sim_n2 = length(sim_dat$x2))
    sim_dat
  }) |> rbindlist()
  sim_dat
  
  # calculate true effect sizes
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "lnCVR",
                         SAFE_distribution = "4_multivariate_normal_wishart",
                         SAFE = TRUE,
                         verbose = FALSE,
                         SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second",
             "yi_safe", "vi_safe"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_y_plugin_2nd", "true_v_plugin_2nd",
             "true_yi_safe", "true_vi_safe"))
  
  # calculate simulated effect sizes
  out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n1 = scens$sample_size1, n2 = scens$sample_size2, 
                  effect_type = "lnCVR",
                  SAFE_distribution = "4_multivariate_normal_wishart",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  
  setnames(out,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st",
             "sim_y_plugin_2nd", "sim_v_plugin_2nd"))
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        true_point,
                        out)
  
  return(results)

}

lnHWE <- function(scens){

  # Simulate data:
  sim_dat <- lapply(1:nrow(scens),
                    function(x){
                      
                      y <- data.table(obj = sample(x = c("AA", "Aa", "aa"),
                                                   size = scens$n[x],
                                                   prob = c(scens$p_AA[x], scens$p_Aa[x], scens$p_aa[x]),
                                                   replace = TRUE))
                      y <- y[, .(n = as.double(.N)), by = .(obj)]
                      
                      if(!all(c("AA", "Aa", "aa") %in% y$obj)){
                        y <- rbind(y,
                                   data.table(obj = setdiff(c("AA", "Aa", "aa"), y$obj),
                                              n = 0))
                        y[, n := n + 0.5]
                      }
                      
                      y[, obj := paste0("sim_n_", obj)]
                      y[, scenario_id := scens$scenario_id[x]]
                      y <- dcast(y, scenario_id ~ obj, 
                                 value.var = "n")
                      y$scenario_id <- NULL
                      return(y)
                    }) |> 
    rbindlist()
  sim_dat
  
  # Calculate true effect sizes
  true_point <- eff_size(n_AA = scens$true_n_AA, 
                         n_Aa = scens$true_n_Aa,
                         n_aa = scens$true_n_aa,  
                         effect_type = "lnHWE_A",
                         SAFE = TRUE,
                         verbose = FALSE,
                         SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_safe", "vi_safe"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_yi_safe", "true_vi_safe"))

  # Calculate simulated effect sizes
  out <- eff_size(n_AA = sim_dat$sim_n_AA, 
                  n_Aa = sim_dat$sim_n_Aa,
                  n_aa = sim_dat$sim_n_aa,  
                  effect_type = "lnHWE_A",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  
  setnames(out,
           c("yi_first", "vi_first"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st"))
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        true_point,
                        out)
  
  return(results)
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# >>> Prepare loop --------------------------------------------------

res <- list()
i <- 1
start <- 1

end <- unique(guide$iterations_per_core) 

# Save a checkpoint file every N iterations:
checkpoint_length <- 20

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# >>> Load checkpoint files... --------------------------------------------
if(file.exists(unique(guide$checkpoint_path))){
  # Some checkpoint files get corrupted, thus the tryCatch
  tryCatch(expr={
    res <- readRDS(unique(guide$checkpoint_path))
    start <- length(res)+1
    print("CHECKPOINT FILE LOADED SUCCESSFULLY")
    print(paste0("Starting at iteration ", start))
  },
  error = function(e){
    print("ERROR IN READING CHECKPOINT")
  })
  
  # If corrupted, delete the file:
  if(length(res) == 0){
    file.remove(unique(guide$checkpoint_path))
    res <- list()
    start <- 1
  }
  # end <- 1e5 - max(res[[1]]$iter)
  # Seed would lead to exact same results otherwise...
  
  set.seed(as.integer(guide$seed) + start) # if each chunk has the same seed, we'll be in trouble...
  
}else{
  print("No checkpoint found. Starting with n=1")
  
  start <- 1
  set.seed(as.integer(guide$seed)) # if each chunk has the same seed, we'll be in trouble...
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# >>> Loop --------------------------------------------------

print(paste("Beginning simulation for", unique(guide$run_ID)))
i <- start

for(i in start:end){ 
  type <- unique(guide$effect_type)
  if(length(type) > 1){
    stop("More than one effect type in loop")
  }
  
  if(type == "reciprocal") res[[i]] <- reciprocal(scens = guide)
  if(type == "lnRoM") res[[i]] <- lnRoM(scens = guide)
  if(type == "SMD_normal") res[[i]] <- SMD(scens = guide)
  if(type == "lnOR") res[[i]] <- lnOR(scens = guide)
  if(type == "lnOR_normal") res[[i]] <- lnOR_normal(scens = guide)
  if(type == "lnRR") res[[i]] <- lnRR(scens = guide)
  if(type == "lnRR_normal") res[[i]] <- lnRR_normal(scens = guide)
  if(type == "lnCVR_normal") res[[i]] <- lnCVR(scens = guide)
  if(type == "lnHWE_A") res[[i]] <- lnHWE(scens = guide)
  if(type == "SMD_Wishart") res[[i]] <- SMD_Wishart(scens = guide)
  if(type == "lnCVR_Wishart") res[[i]] <- lnCVR_Wishart(scens = guide)
  
  if(nrow(res[[i]][is.na(yi_safe) | is.na(vi_safe), ]) > 0){
    stop("NAs in SAFE results")
  }
  
  res[[i]][, iter := i]
  print(paste("iteration", i, "successful"))

  if(i %% checkpoint_length == 0){
    saveRDS(res,
            unique(guide$checkpoint_path))
    print("Saving checkpoint file")
  }
  
}
# Bind results:
results.dat <- rbindlist(res, fill = TRUE)
print(nrow(results.dat))

# Save:
saveRDS(results.dat, unique(guide$file_path))

print("Saved successfully")
