#
#
# Load raw cluster outputs, calculate bias and MC standard error of bias and bind
#
#

rm(list = ls())

library("data.table")
library("crayon")
library("MASS")
library("stringr")
library("parallel")
library("foreach")
library("doSNOW")

# >>> Helper functions ----------------------------------------------------
bias <- function(estimates,
                 estimand){
  ( mean(estimates) - estimand )
}

bias2 <- function(estimates,
                  estimand){
  ( mean(estimates) - estimand ) ^ 2
}

relative_bias <- function(estimates,
                          estimand){
  ((mean(estimates) - estimand ) / estimand) * 100
}

mean_square_error <- function(estimates,
                              estimand){
  mean( (estimates - estimand)^2 )
}

MC_standard_error_of_bias <- function(estimates){
  var_estimates <- var(estimates, na.rm = TRUE)
  return( sqrt(var_estimates / length(estimates)) )
}

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

# >>> Load scenarios  -----------------------------------------------------
scenarios <- readRDS("remote_mirrors/final_simulations/data/scenarios.Rds")

unique(scenarios[effect_type == "lnHWE_A", .(scenario_id, n)])

# This includes paths to the chunks that were run (this will change in next run)
# We will filter to a single row per scenario based on these id variables:
# id_vars <- names(scenarios)[!names(scenarios) %in% c("key", "run_ID", "chunk",
#                                                      "file_path")]
# id_vars

# Get the names that aren't related to the separate cluster runs
id_names <- names(scenarios)[!names(scenarios) %in% c("key", "run_ID", "chunk",
                                                      "file_path", "checkpoint_path",
                                                      "seed", "batch_ID", "total_iterations_needed",
                                                      "iterations_per_core", "number_of_cores_needed")]
id_names

# Load file names ---------------------------------------------------------
files <- data.table(path = list.files("run_simulations/remote_mirrors/outputs/", full.names = T))

files
nrow(files)

files[, run_ID := word(path, -1, sep = "/")]
files 

files[, run_ID := gsub(".Rds", "", run_ID)]
files

scenarios$run_ID
setdiff(scenarios$run_ID, files$run_ID)
#' [Must be length 0]

setdiff(files$run_ID, scenarios$run_ID)
#' [MUST BE LENGTH 0]
files <- files[!grepl("lnOR_normal", path), ]
files <- files[!grepl("lnRR_normal", path), ]
files

unique(scenarios$effect_type)

files.mrg <- merge(files,
                   unique(scenarios[, .(run_ID, effect_type)]))
files.mrg
nrow(files.mrg) == nrow(files)
#' [Must be TRUE]

unique(files.mrg$effect_type)

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------
# Process ----------------------------------------------------------

unique(files.mrg$effect_type)
effects <- unique(files.mrg$effect_type)
sim <- c()
effects
i <- 7

results.clust <- list()

clust_out <- prepare_cluster(n = length(effects))

results.clust <- foreach(i = 1:length(effects), 
                         .options.snow = clust_out$options,
                         .errorhandling = "stop",
                         .packages = c("data.table")) %dopar% {
                           
                           # >>> Load files & bind ---------------------------------------------------
                           sim <- lapply(files.mrg[effect_type == effects[i], ]$path,
                                         readRDS) |> 
                             rbindlist(fill = TRUE)
                           
                           sim[, .(n = .N), by = .(scenario_id)]
                           
                           sim
                           
                           #' [Column names of dataset:]
                           #' *true == true values*
                           #' *sim == simulated values*
                           #' *y == point *
                           #' *v == variance *
                           #' 
                           if(!"sim_y_plugin_2nd" %in% names(sim)){
                             #' [If there's only 1 plugin:]
                             # Calculate variance estimands:
                             sim[, `:=` (var_estimand_SAFE = var(yi_safe),
                                         var_estimand_1st = var(sim_y_plugin_1st)),
                                 by = .(effect_type, scenario_id, boots)]
                             
                             
                             # Now summarize:
                             sim_summary <- sim[, .(
                               # MCSE:
                               MCSE.plugin_1st.NA.point = MC_standard_error_of_bias(sim_y_plugin_1st),
                               MCSE.safe.NA.point = MC_standard_error_of_bias(yi_safe),
                               MCSE.plugin_1st.NA.variance = MC_standard_error_of_bias(sim_v_plugin_1st),
                               MCSE.safe.NA.variance = MC_standard_error_of_bias(vi_safe),
                               
                               # SD of SAFE (to evaluate bootstrapping) on true values
                               SD.safe.NA.point = sd(true_yi_safe),
                               SD.safe.NA.variance = sd(true_vi_safe),
                               # Bias of point estimates:
                               bias.plugin_1st.plugin_1st.point = bias(sim_y_plugin_1st,
                                                                       true_y_plugin_1st),
                               bias.safe.plugin_1st.point = bias(yi_safe,
                                                                 true_y_plugin_1st),
                               # Variance relative bias:
                               relative_bias.safe.plugin_1st.variance = relative_bias(vi_safe, var_estimand_1st),
                               relative_bias.safe.safe.variance = relative_bias(vi_safe, var_estimand_SAFE),
                               relative_bias.plugin_1st.plugin_1st.variance = relative_bias(sim_v_plugin_1st, var_estimand_1st),
                               relative_bias.plugin_1st.safe.variance = relative_bias(sim_v_plugin_1st, var_estimand_SAFE)),
                               by = .(scenario_id, effect_type, boots)] |> unique()
                             
                           }else{
                             #' [If there's a second order plugin formula:]
                             
                             sim[, `:=` (var_estimand_SAFE = var(yi_safe),
                                         var_estimand_1st = var(sim_y_plugin_1st),
                                         var_estimand_2nd = var(sim_y_plugin_2nd)),
                                 by = .(effect_type, scenario_id, boots)]
                             
                             sim_summary <- sim[, .(
                               # MCSE:
                               MCSE.plugin_1st.NA.point = MC_standard_error_of_bias(sim_y_plugin_1st),
                               MCSE.safe.NA.point = MC_standard_error_of_bias(yi_safe),
                               MCSE.plugin_1st.NA.variance = MC_standard_error_of_bias(sim_v_plugin_1st),
                               MCSE.safe.NA.variance = MC_standard_error_of_bias(vi_safe),
                               MCSE.plugin_2nd.NA.point = MC_standard_error_of_bias(sim_y_plugin_2nd),
                               MCSE.plugin_2nd.NA.variance = MC_standard_error_of_bias(sim_v_plugin_2nd),
                               
                               # SD of SAFE (to evaluate bootstrapping) on true values
                               SD.safe.NA.point = sd(true_yi_safe),
                               SD.safe.NA.variance = sd(true_vi_safe),
                               # Bias of point estimates:
                               bias.plugin_1st.plugin_1st.point = bias(sim_y_plugin_1st,
                                                                       true_y_plugin_1st),
                               bias.plugin_2nd.plugin_1st.point = bias(sim_y_plugin_2nd,
                                                                       true_y_plugin_1st),
                               bias.safe.plugin_1st.point = bias(yi_safe,
                                                                 true_y_plugin_1st),
                               # Variance relative bias:
                               relative_bias.safe.plugin_1st.variance = relative_bias(vi_safe, var_estimand_1st),
                               relative_bias.safe.plugin_2nd.variance = relative_bias(vi_safe, var_estimand_2nd),
                               relative_bias.safe.safe.variance = relative_bias(vi_safe, var_estimand_SAFE),
                               relative_bias.plugin_1st.plugin_1st.variance = relative_bias(sim_v_plugin_1st, var_estimand_1st),
                               relative_bias.plugin_1st.plugin_2nd.variance = relative_bias(sim_v_plugin_1st, var_estimand_2nd),
                               relative_bias.plugin_1st.safe.variance = relative_bias(sim_v_plugin_1st, var_estimand_SAFE),
                               relative_bias.plugin_2nd.plugin_1st.variance = relative_bias(sim_v_plugin_2nd, var_estimand_1st),
                               relative_bias.plugin_2nd.plugin_2nd.variance = relative_bias(sim_v_plugin_2nd, var_estimand_2nd),
                               relative_bias.plugin_2nd.safe.variance = relative_bias(sim_v_plugin_2nd, var_estimand_SAFE)),
                               by = .(scenario_id, effect_type, boots)] |> unique()
                             
                           }
                           
                           # >>> Melt and split names -------------------------------------------
                           sim_summary.mlt <- melt(sim_summary,
                                                   id.vars = c("scenario_id", 
                                                               "boots",
                                                               "effect_type"))
                           sim_summary.mlt
                           
                           sim_summary.mlt[, c("calculation", "estimator", 
                                               "estimand", "estimate_of") := tstrsplit(variable, 
                                                                                       ".", 
                                                                                       fixed = TRUE)]
                           sim_summary.mlt
                           unique(sim_summary.mlt[, .(variable, calculation, estimator, estimand, estimate_of)])
                           
                           sim_summary.mlt$variable <- NULL
                           
                           sim_summary.mlt
                           
                           setcolorder(sim_summary.mlt, 
                                       c("effect_type", "scenario_id", "boots",
                                         "calculation", "estimator", "estimand",
                                         "estimate_of", "value"),
                                       skip_absent = TRUE)
                           
                           sim_summary.mlt
                           
                           unique(sim_summary.mlt[, .(estimand, calculation, estimator)])
                           
                           # >>> Merge in scenario details -------------------------------------------
                           sim_bias_final <- merge(unique(sim[, id_names, with = F]),
                                                   sim_summary.mlt[, !c("effect_type", "boots"), with = F],
                                                   by = "scenario_id")
                           
                           return(sim_bias_final)
                           
                         }

stopCluster(clust_out$cl)

results.clust.dat <- rbindlist(results.clust, fill = TRUE, use.names = TRUE) #, fill = TRUE
results.clust.dat

list.files("run_simulations/remote_mirrors/summaries/")
results.clust.dat
unique(results.clust.dat$scenario_id)

saveRDS(results.clust.dat, "run_simulations/remote_mirrors/summaries/all_scenarios_summarized.Rds")

# results.clust.dat readRDS("remote_mirrors/all_effect_sizes_august_2025/summaries/all_scenarios_summarized.Rds")
