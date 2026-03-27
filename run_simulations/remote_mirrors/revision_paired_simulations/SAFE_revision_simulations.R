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
  setwd("run_simulations/remote_mirrors/revision_paired_simulations/")
  
  index <- 2 # This is the chunk number
  
  # setwd("/Users/ejlundgren/GenomeDK/meta_megafauna/meta_simulations/")
  source('remote_universal_SAFE.R')
  
  scenarios <- readRDS("data/scenarios.Rds")

  guide <- scenarios[chunk == index, ]
  scens <- copy(guide)
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

data_generation <- function(scens){
  lapply(1:nrow(scens), function(x){
    
    # Simulate data for scens
    # sig <- diag(c(scens$true_sd1[x]^2,
    #               scens$true_sd2[x]^2))
    sig <- matrix(
      c(scens$true_sd1[x]^2,
        scens$r[x] * scens$true_sd1[x] * scens$true_sd2[x],
        scens$r[x] * scens$true_sd1[x] * scens$true_sd2[x],
        scens$true_sd2[x]^2),
      nrow = 2, ncol = 2, byrow = TRUE
    )
    
    means <- c(m1 = scens$true_mean1[x],
               m2 = scens$true_mean2[x])
    
    out <- rtmvnorm(n = scens$n[x],
                    mean = means,
                    sigma = sig,
                    lower = rep(0, length(means)),
                    upper = rep(Inf, length(means)),
                    algorithm = "gibbs") |>
      as.data.frame() |>
      setDT()
    names(out) <- c("m1", "m2")
    
    out <- data.table(sim_mean1 = mean(out$m1),
                      sim_mean2 = mean(out$m2),
                      sim_sd1 = sd(out$m1),
                      sim_sd2 = sd(out$m2))
    return(out)
  }) |> rbindlist()
}


lnRoM <- function(scens){
  
  sim_dat <- data_generation(scens)
  
  # Calculate simulated effect sizes
  effs <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n = scens$n, r = scens$r, 
                  effect_type = "lnRoM_paired",
                  SAFE = TRUE,
                  parallelize = FALSE,
                  verbose = FALSE,
                  SAFE_boots = 1e6)
  setnames(effs,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st",
             "sim_y_plugin_2nd", "sim_v_plugin_2nd"),
           skip_absent=TRUE)
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        effs)
  
  return(results)
  
}

SMD_Wishart <- function(scens){
  
  # Simulate data for scen
  sim_dat <- data_generation(scens)
  
  # Calculate simulated effect sizes
  effs <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n = scens$n, r = scens$r, 
                  effect_type = "SMD_paired",
                  SAFE_distribution = "4_multivariate_normal_wishart_paired",
                  parallelize = FALSE,
                  SAFE = TRUE,
                  verbose = FALSE,
                 SAFE_boots = 1e6)
  setnames(effs,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st",
             "sim_y_plugin_2nd", "sim_v_plugin_2nd"),
           skip_absent=TRUE)
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        effs)
  
  return(results)
}

lnCVR_Wishart <- function(scens){
  
  # Simulate data:
  sim_dat <- data_generation(scens)
  
  
  # calculate simulated effect sizes
  effs <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                  sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                  n = scens$n, r = scens$r, 
                  effect_type = "lnCVR_paired",
                  SAFE_distribution = "4_multivariate_normal_wishart_paired",
                  SAFE = TRUE,
                  parallelize = FALSE,
                  verbose = FALSE,
                  SAFE_boots = 1e6)
  
  setnames(effs,
           c("yi_first", "vi_first", 
             "yi_second", "vi_second"),
           c("sim_y_plugin_1st", "sim_v_plugin_1st",
             "sim_y_plugin_2nd", "sim_v_plugin_2nd"),
           skip_absent=TRUE)
  
  # Store results:
  results <- data.table(scens,
                        sim_dat,
                        effs)
  
  return(results)

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# >>> Prepare loop --------------------------------------------------

res <- list()
i <- 1
start <- 1

end <- 100

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

if(length(res) == end){
  saveRDS(rbindlist(res, fill = T), unique(guide$file_path))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# >>> Loop --------------------------------------------------

print(paste("Beginning simulation for", unique(guide$batch_id)))
i <- start

for(i in start:end){ 
  type <- unique(guide$effect_type)
  if(length(type) > 1){
    stop("More than one effect type in loop")
  }
  
  if(type == "lnRoM") res[[i]] <- lnRoM(scens = guide)
  if(type == "SMD") res[[i]] <- SMD_Wishart(scens = guide)
  if(type == "lnCVR") res[[i]] <- lnCVR_Wishart(scens = guide)
  if(type == "lnOR") res[[i]] <- lnOR(scens = guide)
  if(type == "lnRR") res[[i]] <- lnRR(scens = guide)
  
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
