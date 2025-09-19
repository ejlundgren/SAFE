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

# >>> Encapsulate each effect size sim ----------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------

# Encapsulate each function
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
  
  true_point <- eff_size(n = scens$sample_size,
                         x = scens$true_mean,
                         sd = scens$true_sd,
                         effect_type = "reciprocal",
                         SAFE = FALSE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first"),
           c("true_y_plugin_1st", "true_v_plugin_1st"))
  
  out <- eff_size(x = sim_dat$sim_mean,
                  sd = sim_dat$sim_sd, #' [we used to use sd_of_hyperpopulation. But this needs to be clarified programmatically]
                  n = sim_dat$sim_n,
                  effect_type = "reciprocal",
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

lnRoM <- function(scens){
  
  x <- 1
  sim_dat <- lapply(1:nrow(scens), function(x){
    
    # Simulate data for scens
    #' [Note that unlike with SAFE, this uses SD2 because it's simulating a population not a parameter]
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
  
  
  #
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "lnRoM",
                         SAFE = FALSE,
                         verbose = FALSE,
                         SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_y_plugin_2nd", "true_v_plugin_2nd"))
  
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
  
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "SMD",
                         SAFE_distribution = "4_multivariate_normal",
                         SAFE = FALSE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_y_plugin_2nd", "true_v_plugin_2nd"))
  
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
  # x <- 1
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
  
  #' [Filter to number of samples per treatment]
  
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "SMD",
                         SAFE_distribution = "4_multivariate_normal_wishart",
                         SAFE = FALSE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_y_plugin_2nd", "true_v_plugin_2nd"))
  
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


# OR 0.5 add to all groups; lnRR add 0.5 to 0 group and 1 to n but NOT to all groups
lnOR <- function(scens){
  
  #' [Simulate data:]
  sim_dat <- lapply(1:nrow(scens),
                    function(x){
                      y <- data.table(sim_a = sum(rbinom(scens$n1[x], 1, scens$true_p_a[x])),
                                      sim_c = sum(rbinom(scens$n2[x], 1, scens$true_p_c[x])))
                      y[, `:=` (sim_b = scens$n1[x] - sim_a,
                                sim_d = scens$n2[x] - sim_c)]
                      
                      
                      # Add 0.5 to rows with ANY zero
                      # if(nrow(y[(sim_a == 0 | sim_b == 0 | sim_c == 0 | sim_d == 0), ]) > 0){
                      y[(sim_a == 0 | sim_b == 0 | sim_c == 0 | sim_d == 0), 
                        `:=` (sim_a = sim_a + 0.5,
                              sim_b = sim_b + 0.5,
                              sim_c = sim_c + 0.5,
                              sim_d = sim_d + 0.5)]
                      # }
                      
                      return(y)
                    }) |> 
    rbindlist()
  
  true_point <- eff_size(a = scens$true_a, b = scens$true_b,
                         c = scens$true_c,  d = scens$true_d,
                         effect_type = "lnOR",
                         SAFE_distribution = "4_binomial",
                         SAFE = FALSE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first"),
           c("true_y_plugin_1st", "true_v_plugin_1st"))
  
  # This will take a while (not parallelizing SAFE because of cluster conflicts...)
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


lnOR_normal <- function(scens){

  #' [Simulate data:]
  sim_dat <- lapply(1:nrow(scens),
                    function(x){

                      sig <- diag(c((scens$true_p_a[x] * (1 - scens$true_p_a[x]))  / scens$n1[x] ,
                                    (scens$true_p_c[x] * (1 - scens$true_p_c[x]))  / scens$n2[x] ))

                      means <- c(p1 = scens$true_p_a[x],
                                 p2 = scens$true_p_c[x])
                      y <- rtmvnorm(n = max(c(scens$n1[x], scens$n2[x])),
                                    mean = means,
                                    sigma = sig,
                                    lower = rep(0, length(means)),
                                    upper = rep(1, length(means)),
                                    algorithm = "gibbs") |>
                        as.data.frame() |>
                        setDT()
                      names(y) <- c("p1", "p2")

                      sim_dat <- list(p1 = y[1:scens$n1[x], ]$p1,
                                      p2 = y[1:scens$n2[x], ]$p2)

                      sim_dat <- data.table(sim_p1 = mean(sim_dat$p1),
                                            sim_p2 = mean(sim_dat$p2),
                                            sim_n1 = length(sim_dat$p1),
                                            sim_n2 = length(sim_dat$p2))
                      sim_dat

                      #' [need to draw from binomial with the new p...]

                    }) |>
    rbindlist()

  sim_dat[, `:=` (sim_a = round(sim_p1 * sim_n1) |> as.double(),
                  sim_c = round(sim_p2 * sim_n2) |> as.double())]

  sim_dat[, `:=` (sim_b = sim_n1-sim_a,
                  sim_d = sim_n2-sim_c)]

  sim_dat[(sim_a == 0 | sim_b == 0 | sim_c == 0 | sim_d == 0),
          `:=` (sim_a = sim_a+0.5,
                sim_b = sim_b+0.5,
                sim_c = sim_c+0.5,
                sim_d = sim_d+0.5)]

  true_point <- eff_size(a = scens$true_a, b = scens$true_b,
                         c = scens$true_c,  d = scens$true_d,
                         effect_type = "lnOR",
                         SAFE_distribution = "2_multinomial_as_normal",
                         SAFE = FALSE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first"),
           c("true_y_plugin_1st", "true_v_plugin_1st"))

  # This will take a while (not parallelizing SAFE because of cluster conflicts...)
  out <- eff_size(a = sim_dat$sim_a, b = sim_dat$sim_b,
                  c = sim_dat$sim_c,  d = sim_dat$sim_d,
                  effect_type = "lnOR",
                  SAFE_distribution = "2_multinomial_as_normal",
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
                         SAFE = FALSE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first"),
           c("true_y_plugin_1st", "true_v_plugin_1st"))
  
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

#' lnRR_normal <- function(scens){
#'   # Add 0.5 just to affected group and 1 to n, unlike with OR (because a, b, c, d require symmetry but lnRR doens't)
#'   #' [Simulate data:]
#'   sim_dat <- lapply(1:nrow(scens),
#'                     function(x){
#'                       
#'                       sig <- diag(c((scens$true_p_a[x] * (1 - scens$true_p_a[x])) / scens$n1[x],
#'                                     (scens$true_p_c[x] * (1 - scens$true_p_c[x])) / scens$n2[x]))
#'                       if(nrow(sig) > 2) print("what the absolute hell")
#'                       
#'                       means <- c(p1 = scens$true_p_a[x],
#'                                  p2 = scens$true_p_c[x])
#'                       y <- rtmvnorm(n = max(c(scens$n1[x], scens$n2[x])),
#'                                     mean = means,
#'                                     sigma = sig,
#'                                     lower = rep(0, length(means)),
#'                                     upper = rep(1, length(means)),
#'                                     algorithm = "gibbs") |>
#'                         as.data.frame() |>
#'                         setDT()
#'                       names(y) <- c("p1", "p2")
#'                       
#'                       sim_dat <- list(p1 = y[1:scens$n1[x], ]$p1,
#'                                       p2 = y[1:scens$n2[x], ]$p2)
#'                       
#'                       sim_dat <- data.table(sim_p1 = mean(sim_dat$p1),
#'                                             sim_p2 = mean(sim_dat$p2),
#'                                             sim_n1 = length(sim_dat$p1),
#'                                             sim_n2 = length(sim_dat$p2))
#'                       sim_dat
#'                       
#'                       #' [need to draw from binomial with the new p...]
#'                       
#' 
#'                     }) |> rbindlist()
#'   
#'   sim_dat[, `:=` (sim_a = round(sim_p1 * sim_n1) |> as.double(),
#'                   sim_c = round(sim_p2 * sim_n2) |> as.double())]
#'   
#'   sim_dat[sim_a == 0, `:=` (sim_a = sim_a + 0.5, 
#'                             sim_n1 = sim_n1 + 1)]
#'   sim_dat[sim_c == 0, `:=` (sim_c = sim_c + 0.5, 
#'                             sim_n2 = sim_n2 + 1)]
#'   
#'   true_point <- eff_size(a = scens$true_a, c = scens$true_c,
#'                          n1 = scens$n1,  n2 = scens$n2,
#'                          effect_type = "lnRR",
#'                          SAFE_distribution = "2_multinomial_as_normal",
#'                          SAFE = FALSE,
#'                          verbose = FALSE,
#'                         SAFE_boots = unique(scens$boots))
#'   setnames(true_point,
#'            c("yi_first", "vi_first"),
#'            c("true_y_plugin_1st", "true_v_plugin_1st"))
#'   
#'   # a = sim_dat$sim_a; c = sim_dat$sim_c
#'   # n1 = sim_dat$n1;  n2 = sim_dat$n2
#'   
#'   out <- eff_size(a = sim_dat$sim_a, c = sim_dat$sim_c,
#'                   n1 = sim_dat$sim_n1,  n2 = sim_dat$sim_n2,
#'                   effect_type = "lnRR",
#'                   SAFE_distribution = "2_multinomial_as_normal",
#'                   SAFE = TRUE,
#'                   verbose = FALSE,
#'                  SAFE_boots = unique(scens$boots))
#'   
#'   setnames(out,
#'            c("yi_first", "vi_first"),
#'            c("sim_y_plugin_1st", "sim_v_plugin_1st"))
#'   
#'   # Store results:
#'   results <- data.table(scens,
#'                         sim_dat,
#'                         true_point,
#'                         out)
#'   
#'   return(results)
#' 
#' }

lnCVR <- function(scens){
  
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
  
  #' [true plugins:]
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "lnCVR",
                         SAFE_distribution = "4_multivariate_normal",
                         SAFE = FALSE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_y_plugin_2nd", "true_v_plugin_2nd"))
  
  #' [Now sim:]
  out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n1 = scens$sample_size1, n2 = scens$sample_size2, 
                  effect_type = "lnCVR",
                  SAFE_distribution = "4_multivariate_normal",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  # # for TESTING:
  if(any(is.na(out$yi_safe))){
    print(data.table(sim_dat, out))
  }
  
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
  
  #' [true plugins:]
  true_point <- eff_size(x1 = scens$true_mean1, x2 = scens$true_mean2,
                         sd1 = scens$true_sd1,  sd2 = scens$true_sd2,
                         n1 = scens$sample_size1, n2 = scens$sample_size2, 
                         effect_type = "lnCVR",
                         SAFE_distribution = "4_multivariate_normal_wishart",
                         SAFE = FALSE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("true_y_plugin_1st", "true_v_plugin_1st",
             "true_y_plugin_2nd", "true_v_plugin_2nd"))
  
  #' [Now sim:]
  out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n1 = scens$sample_size1, n2 = scens$sample_size2, 
                  effect_type = "lnCVR",
                  SAFE_distribution = "4_multivariate_normal_wishart",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  # for TESTING:
  if(any(is.na(out$yi_safe))){
    print(data.table(sim_dat, out))
  }
  
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
  # scens <- copy(guide)
  
  sim_dat <- lapply(1:nrow(scens),
                    function(x){
                      
                      # y <- data.table()
                      
                      #' [can't accept 0s...]
                      # while(nrow(sim_dat) < 3){
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
                        #' [Make sure 0.5 is added to all of them]
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
  
  #' [true plugins:]
  true_point <- eff_size(n_AA = scens$true_n_AA, 
                         n_Aa = scens$true_n_Aa,
                         n_aa = scens$true_n_aa,  
                         effect_type = "lnHWE_A",
                         SAFE = FALSE,
                         verbose = FALSE,
                        SAFE_boots = unique(scens$boots))
  setnames(true_point,
           c("yi_first", "vi_first"),
           c("true_y_plugin_1st", "true_v_plugin_1st"))
  
  
  #' [Now sim:]
  out <- eff_size(n_AA = sim_dat$sim_n_AA, 
                  n_Aa = sim_dat$sim_n_Aa,
                  n_aa = sim_dat$sim_n_aa,  
                  effect_type = "lnHWE_A",
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = unique(scens$boots))
  
  # for TESTING:
  # if(is.na(out$yi_safe)){
  #   print(sim_dat)
  # }
  
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

# Testing -----------------------------------------------------------------

if(local){
  prepare_cluster <- function(n){
    require("parallel")
    require("foreach")
    require("doSNOW")
    
    nCores <- parallel::detectCores() -1 
    cl <- makeCluster(nCores)
    registerDoSNOW(cl)
    
    # Progress bar
    pb <- txtProgressBar(max = n, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    ret <- list(opts, pb, cl)
    names(ret) <- c("options", "progress", "cluster")
    return(ret)
    
    cat("Pass 'x$options' to .opts in foreach;
      'x$progress' to setTxtProgressBar(x$progress, i);
      'x$cluster' to stopCluster(x$cluster) after foreach")
  }
  
  
  scenarios <- readRDS("data/scenarios.Rds")
  source('remote_universal_SAFE.R')
  
  unique(scenarios$effect_type)
  
  sub_scenarios <- scenarios[effect_type == "SMD_Wishart", .SD[1], by = .(boots)]
  
  sub_scenarios[boots == 1e8, boots := 1e2]
  sub_scenarios

  runs <- unique(sub_scenarios$run_ID)
  
  i <- 1
  k <- 1
  timings <- c()
  res <- list()
  inside.res <- list()
  
  N <- 1000
  clust_out <- prepare_cluster(n = N)
  
  res <- foreach(i = 1:N, 
                           .options.snow = clust_out$options,
                           .errorhandling = "stop",
                           .packages = c("data.table", "MASS",
                                         "crayon", "tmvtnorm")) %dopar% {
    
    for(k in 1:length(runs)){
      guide <- sub_scenarios[run_ID == runs[k], ]
      type <- unique(guide$effect_type)
      type
      
      if(length(type) > 1){
        stop("More than one effect type in guide")
      }
      
      s <- Sys.time()
      if(type == "reciprocal") inside.res[[k]] <- reciprocal(scens = guide)
      if(type == "lnRoM") inside.res[[k]] <- lnRoM(scens = guide)
      if(type == "SMD_normal") inside.res[[k]] <- SMD(scens = guide)
      if(type == "lnOR") inside.res[[k]] <- lnOR(scens = guide)
      if(type == "lnOR_normal") inside.res[[k]] <- lnOR_normal(scens = guide) 
      if(type == "lnRR") inside.res[[k]] <- lnRR(scens = guide)
      if(type == "lnRR_normal") inside.res[[k]] <- lnRR_normal(scens = guide) 
      if(type == "lnCVR_normal") inside.res[[k]] <- lnCVR(scens = guide)
      if(type == "lnHWE_A") inside.res[[k]] <- lnHWE(scens = guide)
      if(type == "SMD_Wishart") inside.res[[k]] <- SMD_Wishart(scens = guide)
      if(type == "lnCVR_Wishart") inside.res[[k]] <- lnCVR_Wishart(scens = guide)
      inside.res[[k]]$timing <- Sys.time() - s
      
      if(nrow(inside.res[[k]][is.na(yi_safe) | is.na(vi_safe), ]) > 0){
        stop("NAs in SAFE results")
      }
    }
  
   setTxtProgressBar(pb = clust_out$progress, value = i)
   return(rbindlist(inside.res))
  }
  #
  #
  #
  #
  #
  stopCluster(clust_out$cluster)
  
  res.bind <- rbindlist(res)
  
  setorder(res.bind, boots)
  res.bind[, .(sd_yi_safe = sd(yi_safe),
               sd_vi_safe = sd(vi_safe),
               mean_timing = mean(timing),
               sd_timing = sd(timing)), 
      by = .(boots)]
  #
  #
  #
  #
  #
  #
  #
  # names(timings) <- effs
  # timings
  # 
  # sub_scenarios[effect_type == "lnRR_normal"]
  # 
  # guide <- scenarios[run_ID == "lnRR_normal_run1"]
  # scens <- copy(guide) # for inside functions
  # length(unique(scens$scenario_id))
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# >>> Prepare loop --------------------------------------------------
res <- list()
i <- 1
start <- 1
end <- unique(guide$iterations_per_core) # 1e5/200 # This is based on duplication in working guide to increase parallelization
# 500 * 200
# check_point_length <- round(unique(guide$iterations_per_core) * .01) # Save every 1 percent?
check_point_length <- 15

if(local){
  end <- 20
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# >>> Load checkpoint files... --------------------------------------------
if(file.exists(unique(guide$checkpoint_path))){
  
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
  #' [Seed would lead to exact same results otherwise...]
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

  if(i %% check_point_length == 0){
    saveRDS(res,
            unique(guide$checkpoint_path))
    print("Saving checkpoint file")
  }
  
}
#
results.dat <- rbindlist(res, fill = TRUE)
print(nrow(results.dat))

results.dat[is.na(yi_safe), ]
results.dat[is.na(vi_safe), ] # Hopefully these are NAs.
results.dat[is.na(true_y_plugin_1st), ] # Hopefully these are NAs.
results.dat[is.na(true_v_plugin_1st), ] # Hopefully these are NAs.

results.dat[is.na(sim_y_plugin_1st), ] # Hopefully these are NAs.
results.dat[is.na(sim_v_plugin_1st), ] # Hopefully these are NAs.

saveRDS(results.dat, unique(guide$file_path))

print("Saved successfully")
