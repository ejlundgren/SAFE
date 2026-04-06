# July 17th 2025
#
#
# Remote simulations of paired designs
#
#
#
# 0. Prepare environment --------------------------------------------------

rm(list = ls())

library("data.table")
library("MASS")
library("tmvtnorm")
library("crayon")
library("parallel")
# library("pbapply")

local <- FALSE

if(local){
  setwd("run_simulations/remote_mirrors/revision_paired_simulations/")
  
  index <- 298 # This is the chunk number
  
  # setwd("/Users/ejlundgren/GenomeDK/meta_megafauna/meta_simulations/")
  source('remote_universal_SAFE.R')

  scenarios <- readRDS("data/scenarios.Rds")
  # scenarios[which(effect_type == "lnCVR"), ]
  
  guide <- scenarios[chunk == index, ]
  # scens <- copy(guide)
  
  }else{
  
  scenarios <- readRDS("data/working_scenarios.Rds")
  
  # get model number for this iteration
  args <- commandArgs()
  print(args)
  
  index <- as.numeric(args[6]) # get index value from bash script
  
  source('remote_universal_SAFE.R')

  if(!file.exists("outputs")) dir.create("outputs")
  if(!file.exists("checkpoints")) dir.create("checkpoints")
  
  # Chunk should be same for an entire effect size / runID combination
  guide <- scenarios[chunk == index, ]
}

print(paste(nrow(guide), "scenarios to run"))

guide

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Add some constants to guide
guide[, lower_filter := ifelse(effect_type == "SMD", -Inf, 0)]
guide[, effect_type := paste0(effect_type, "_paired")]
guide

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# >>> Encapsulate data generation ----------------------------------------------------

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# >>> Prepare loop --------------------------------------------------

res <- list()
i <- 1
start <- 1

end <- 1000

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
  
  sim_dat <- data_generation(guide, 
                             lower_filter = unique(guide$lower_filter))
  
  
  # Calculate simulated effect sizes
  effs <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                   sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                   n = guide$n, r = sim_dat$sim_r, 
                   effect_type = type,
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
  res[[i]] <- data.table(guide,
                        sim_dat,
                        effs)
  
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
