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
  
  index <- 3 # This is the chunk number
  
  # setwd("/Users/ejlundgren/GenomeDK/meta_megafauna/meta_simulations/")
  source('remote_universal_SAFE.R')
  source('remote_universal_SAFE_TEST.R')
  
  scenarios <- readRDS("data/scenarios.Rds")
  # scenarios[which(effect_type == "lnCVR"), ]
  
  guide <- scenarios[chunk == index, ]
  scens <- copy(guide)
  
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


# stopifnot(nrow(guide) == 1)
#' @Shinichi- here are all the formulas"
#' formulas <- fread("data/effect_size_formulas.csv")
#' #' [SMD point estimates:]
#' unique(formulas[name == "SMD_paired" ]$sim_family)
#' formulas[name == "SMD_paired" & sim_family == "4_multivariate_normal_wishart_paired"]$formula
#' # First:
#' "(x1 - x2) / sqrt( ((n - 1) * sd1^2 + (n - 1) * sd2^2) / (2 * n - 2) )"
#' # Second:
#' "(ifelse((2 * n - 2) <= 1, NA_real_, exp(lgamma((2 * n - 2)/2) - log(sqrt((2 * n - 2)/2)) - lgamma(((2 * n - 2) - 1)/2)))) * 
#'                   ((x1 - x2) / sqrt( ((n - 1) * sd1^2 + (n - 1) * sd2^2) / (2 * n - 2) ) )"
#' 
#' #' [SMD variance:]
#' # First:
#' "2 * (1 - r) / n + ((x1 - x2) / sqrt( ((n - 1) * sd1^2 + (n - 1) * sd2^2) / (2 * n - 2) ))^2 / (2 * n )"
#' 
#' # Second:
#' "(ifelse((2 * n - 2) <= 1, NA_real_, exp(lgamma((2 * n - 2)/2) - log(sqrt((2 * n - 2)/2)) - 
#'             lgamma(((2 * n - 2) - 1)/2))))^2 * 
#'             (2 * (1 - r) / n + ((x1 - x2) / sqrt( ((n - 1) * sd1^2 + (n - 1) * sd2^2) / 
#'             (2 * n - 2) ))^2 / (2 * n ))"
#' 
#' #' [lnRoM point estimates:]
#' formulas[name == "lnRoM_paired", ]$formula
#' # First:
#' "log(x1 / x2)"
#' 
#' #' [lnRoM variance:]
#' # First:
#' "vi_first <- (sd1^2 / (n1 * x1^2)) + (sd2^2 / (n2 * x2^2)) - ((2 * r * sd1 * sd2) / (x1 * x2 * sqrt(n1 * n2)))"
#' 
#' 
#' #' [lnCVR point estimate:]
#' formulas[name == "lnCVR_paired" & sim_family == "4_multivariate_normal_wishart_paired", ]$formula
#' # First:
#' "log(sd1 / x1) - log(sd2 / x2)"
#' 
#' # Second:
#' "log((sd1 / x1) / (sd2 / x2)) + 1/2 * (1 / (n - 1) - 1 / (n - 1)) + 1/2 * ((sd2^2/(n * x2^2)) - (sd1^2 / (n * x1^2)))"
#' 
#' #' [lnCVR variance:]
#' # First:
#' "sd1^2/(n * x1^2) + sd2^2/(n * x2^2) - 2*r*sd1*sd2/(n * x1 * x2) + 1/(n - 1) - r^2/(n - 1)"
#' 
#' # Second:
#' "sd1^2/(n * x1^2) + sd1^4/(2 * n^2 * x1^4) + sd2^2/(n * x2^2) + sd2^4/(2 * n^2 * x2^4) - 
#'           2*r*sd1*sd2/(n * x1 * x2) + r^2 * sd1^2 * sd2^2 * (x1^4 + x2^4) / (2 * n^2 * x1^4 * x2^4) + 
#'           n/(n - 1)^2 - r^2/(n - 1) + r^4 * (sd1^8 + sd2^8) / (2 * (n - 1)^2 * sd1^4 * sd2^4)"
#' 
#' #' [It is working, i was afriad that maybe I was setting 'r' to 0 inside the function...]
#' eff_size(x1 = 15, x2 = 11.5, sd1 = 1.5, sd2 = 1.3,
#'          n = 15, 
#'          r = 0.5,
#'          verbose = F,
#'          effect_type = "SMD_paired")
#' 
#' eff_size(x1 = 15, x2 = 11.5, sd1 = 1.5, sd2 = 1.3,
#'          n = 15, 
#'          r = 0.8,
#'          verbose = F,
#'          effect_type = "SMD_paired")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Add some constants to guide
guide[, lower_filter := ifelse(effect_type == "SMD", -Inf, 0)]
guide[, effect_type := paste0(effect_type, "_paired")]
guide

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# >>> Encapsulate data generation ----------------------------------------------------

data_generation <- function(scens, lower_filter){
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
    
    out <- rtmvnorm(n = scens$n[x],
                    mean = means,
                    sigma = sig,
                    lower = rep(lower_filter, length(means)),
                    upper = rep(Inf, length(means)),
                    algorithm = "gibbs") |>
      as.data.frame() |>
      setDT()
    names(out) <- c("m1", "m2")
    
    cor <- cor.test(out$m1, out$m2)
    
    out <- data.table(sim_mean1 = mean(out$m1),
                      sim_mean2 = mean(out$m2),
                      sim_sd1 = sd(out$m1),
                      sim_sd2 = sd(out$m2),
                      sim_r = cor$estimate)
    return(out)
  }) |> rbindlist()
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
  
  sim_dat <- data_generation(scens, 
                             lower_filter = unique(guide$lower_filter))
  
  # Calculate simulated effect sizes
  effs <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                   sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                   n = scens$n, r = sim_dat$sim_r, 
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
  
  # Now calculate test alternative, where 'r' was calculated between safe point clouds:
  test <- eff_size2(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                    sd1 = sim_dat$sim_sd1,  sd2 = sim_dat$sim_sd2,
                    n = scens$n, r = sim_dat$sim_r, 
                    effect_type = type,
                    SAFE = TRUE,
                    parallelize = FALSE,
                    verbose = FALSE,
                    SAFE_boots = 1e6)
  test <- test[, .(yi_safe, vi_safe, test_safe_r)]
  setnames(test, 
           c("yi_safe", "vi_safe"),
           c("yi_safe_r_test", "vi_safe_r_test"))
  
  # Store results:
  res[[i]] <- data.table(scens,
                        sim_dat,
                        effs,
                        test)
  
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
