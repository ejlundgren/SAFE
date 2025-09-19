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
files <- data.table(path = list.files("remote_mirrors/final_simulations/outputs/", full.names = T))

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


# >>> TESTING -------------------------------------------------------------
# 
# OR_binomial <- lapply(files.mrg[effect_type == "lnOR" & grepl("1e[+]06", path)]$path,
#             readRDS)
# OR_binomial <- rbindlist(OR_binomial)
# OR_binomial[, .(n = .N), by = .(scenario_id)]
# 
# files.mrg[effect_type == "lnOR_normal" & grepl("1e[+]06", path)]
# OR_normal <- lapply(files.mrg[effect_type == "lnOR_normal" & grepl("1e[+]06", path)]$path,
#              readRDS)
# OR_normal <- rbindlist(OR_normal)
# OR_normal[, .(n = .N), by = .(scenario_id)]
# 
# OR_binomial[scenario_id == "lnOR_scenario_18"]
# OR_binomial[, .(method = "binomial",
#                 mean_yi_safe = mean(yi_safe),
#                 mean_vi_safe = mean(vi_safe),
#                 var_yi_safe = var(yi_safe),
#                 var_yi_plugin = var(sim_y_plugin_1st)),
#             by = .(n1)]
# 
# OR_normal[, .(method = "normal",
#               mean_yi_safe = mean(yi_safe),
#               mean_vi_safe = mean(vi_safe),
#                 var_yi_safe = var(yi_safe),
#                 var_yi_plugin = var(sim_y_plugin_1st)),
#             by = .(n1)]
# 
# effect_formulas <- fread("data/effect_size_formulas.csv")
# effect_formulas[name == "lnOR", .(sim_family, label, formula, lower_filter, upper_filter)]
# length(unique(effect_formulas[name == "lnOR",]$formula))
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------
# Process ----------------------------------------------------------

unique(files.mrg$effect_type)
effects <- unique(files.mrg$effect_type)
sim <- c()
effects
i <- 7

results.clust <- list()
# length(ids)
# which(ids == "lnRR_scenario_1")
# i <- 436
clust_out <- prepare_cluster(n = length(effects))

results.clust <- foreach(i = 1:length(effects), 
                         .options.snow = clust_out$options,
                         .errorhandling = "stop",
                         .packages = c("data.table")) %dopar% {
                           
# >>> Load files & bind ---------------------------------------------------

  sim <- lapply(files.mrg[effect_type == effects[i], ]$path,
                readRDS) |> 
    rbindlist(fill = TRUE)

  # sim[, .(n = .N), by = .(scenario_id)][n != 100000]
  sim
  
  # if(nrow(sim[, .(n = .N), by = .(scenario_id)][n != 100000]) > 0){
  #   sim <- sim[, .SD[1:1e5], by = .(scenario_id)]
  # }
  
  #' [Column names of dataset:]
  #' *true == true values*
  #' *sim == simulated values*
  #' *y == point *
  #' *v == variance *
  #' 

# >>> Point bias, var_mc, & MCSE error ------------------------------- --------
  
  #' [Naming conventions in these columns are as follows:]
  # "calculation", (e.g., bias, MCSE, relative_bias)
  # "estimator", (the estimator used, plugin_1st, plugin_2nd, safe)
  # "estimand", (the estimand used, true_1st, true_2nd, safe)
  # "estimate_of" (point or variance)
  # All separated by '.'
  
  if(!"sim_y_plugin_2nd" %in% names(sim)){

    sim_summary <- sim[, .( bias.plugin_1st.true_1st.point = bias(sim_y_plugin_1st,
                                                                 true_y_plugin_1st),
                           bias.safe.true_1st.point = bias(yi_safe,
                                                           true_y_plugin_1st),
                           
                           # Now MC error:
                           MCSE.plugin_1st.NA.point = MC_standard_error_of_bias(sim_y_plugin_1st),
                           MCSE.safe.NA.point = MC_standard_error_of_bias(yi_safe),
                           MCSE.plugin_1st.NA.variance = MC_standard_error_of_bias(sim_v_plugin_1st),
                           MCSE.safe.NA.variance = MC_standard_error_of_bias(vi_safe),
                           # SD of SAFE (to evaluate bootstrapping)
                           SD.safe.NA.point = sd(yi_safe),
                           SD.safe.NA.variance = sd(vi_safe)),
                       by = .(scenario_id, effect_type, boots)] |> 
      unique()
    sim_summary
    
  }else{

    sim_summary <- sim[, .(bias.plugin_1st.true_1st.point = bias(sim_y_plugin_1st,
                                                                 true_y_plugin_1st),
                           bias.safe.true_1st.point = bias(yi_safe,
                                                           true_y_plugin_1st),
                           
                           # Now MC error:
                           MCSE.plugin_1st.NA.point = MC_standard_error_of_bias(sim_y_plugin_1st),
                           MCSE.safe.NA.point = MC_standard_error_of_bias(yi_safe),
                           MCSE.plugin_1st.NA.variance = MC_standard_error_of_bias(sim_v_plugin_1st),
                           MCSE.safe.NA.variance = MC_standard_error_of_bias(vi_safe),
                           
                           bias.plugin_2nd.true_1st.point = bias(sim_y_plugin_2nd,
                                                                  true_y_plugin_1st),
                          bias.safe.true_1st.point = bias(yi_safe,
                                                          true_y_plugin_1st),
                          # Now MC error:
                          MCSE.plugin_2nd.NA.point = MC_standard_error_of_bias(sim_y_plugin_2nd),
                          MCSE.plugin_2nd.NA.variance = MC_standard_error_of_bias(sim_v_plugin_2nd),
                          # SD of SAFE (to evaluate bootstrapping)
                          SD.safe.NA.point = sd(yi_safe),
                          SD.safe.NA.variance = sd(vi_safe)),
                        by = .(scenario_id, effect_type, boots)] |> 
      unique()
  
  }
  
  
  # >>> Melt point and MCSE stats -------------------------------------------
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
  
  sim_summary.mlt$variable <- NULL
  
  sim_summary.mlt
  
  setcolorder(sim_summary.mlt, 
              c("effect_type", "scenario_id", "boots",
                "calculation", "estimator", "estimand",
                "estimate_of", "value"),
              skip_absent = TRUE)
  
  sim_summary.mlt
  
  unique(sim_summary.mlt[, .(estimand, calculation, estimator)])
  
  # >>> Variance bias -------------------------------------------------------
  
  # Calculate mc_var as our estimand
  if(!"sim_y_plugin_2nd" %in% names(sim)){
    sim[, `:=` (var_estimand_SAFE = var(yi_safe),
                var_estimand_1st = var(sim_y_plugin_1st)),
        by = .(effect_type, scenario_id, boots)]
    
    var.bias <- sim[, .(SAFE_estimate.1st_estimand = relative_bias(vi_safe, var_estimand_1st),
                        SAFE_estimate.SAFE_estimand = relative_bias(vi_safe, var_estimand_SAFE),
                        plugin_1st_estimate.1st_estimand = relative_bias(sim_v_plugin_1st, var_estimand_1st),
                        plugin_1st_estimate.SAFE_estimand = relative_bias(sim_v_plugin_1st, var_estimand_SAFE)),
                    by = .(scenario_id, effect_type, boots)] |> unique()
    
  }else{
    sim[, `:=` (var_estimand_SAFE = var(yi_safe),
                var_estimand_1st = var(sim_y_plugin_1st),
                var_estimand_2nd = var(sim_y_plugin_2nd)),
        by = .(effect_type, scenario_id, boots)]
    
    var.bias <- sim[, .(SAFE_estimate.1st_estimand = relative_bias(vi_safe, var_estimand_1st),
                        SAFE_estimate.2nd_estimand = relative_bias(vi_safe, var_estimand_2nd),
                        SAFE_estimate.SAFE_estimand = relative_bias(vi_safe, var_estimand_SAFE),
                        plugin_1st_estimate.1st_estimand = relative_bias(sim_v_plugin_1st, var_estimand_1st),
                        plugin_1st_estimate.2nd_estimand = relative_bias(sim_v_plugin_1st, var_estimand_2nd),
                        plugin_1st_estimate.SAFE_estimand = relative_bias(sim_v_plugin_1st, var_estimand_SAFE),
                        plugin_2nd_estimate.1st_estimand = relative_bias(sim_v_plugin_2nd, var_estimand_1st),
                        plugin_2nd_estimate.2nd_estimand = relative_bias(sim_v_plugin_2nd, var_estimand_2nd),
                        plugin_2nd_estimate.SAFE_estimand = relative_bias(sim_v_plugin_2nd, var_estimand_SAFE)),
                    by = .(scenario_id, effect_type, boots)] |> unique()
    
  }

  sim
 
  # Now melt:
  var_rel_bias <- melt(var.bias,
                        id.vars = c("scenario_id", "effect_type", "boots"))
  var_rel_bias
  
  # Split the 'variable' into estimator and estimand for plotting
  var_rel_bias[, c("estimator", "estimand") := tstrsplit(variable, ".", fixed = TRUE)]
  var_rel_bias
  
  # 
  # >>> Combine point and variance bias ----------------------------------------------------
  var_rel_bias
  sim_summary.mlt
  
  unique(sim_summary.mlt$estimate_of)
  var_rel_bias[, `:=` (calculation = "relative_bias",
                       estimate_of = "variance")]
  # setnames(var_rel_bias, "relative_bias", "value")
  
  unique(sim_summary.mlt$estimator)
  unique(var_rel_bias$estimator)
  var_rel_bias[, estimator := fcase(estimator == "plugin_1st_estimate", "plugin_1st",
                                    estimator == "plugin_2nd_estimate", "plugin_2nd",
                                    estimator == "SAFE_estimate", "safe")]
  
  unique(sim_summary.mlt$estimand)
  unique(var_rel_bias$estimand)
  
  var_rel_bias[, estimand := fcase(estimand == "1st_estimand", "plugin_1st_mc",
                                   estimand == "2nd_estimand", "plugin_2nd_mc",
                                   estimand == "SAFE_estimand", "safe_mc")]
  var_rel_bias[, variable := NULL]
  
  setdiff(names(var_rel_bias), names(sim_summary.mlt))
  setdiff( names(sim_summary.mlt), names(var_rel_bias))
  
  sim_bias_comb <- rbind(var_rel_bias, sim_summary.mlt)
  sim_bias_comb
  
  # >>> Merge in scenario details -------------------------------------------
  sim_bias_final <- merge(unique(sim[, id_names, with = F]),
                          sim_bias_comb[, !c("effect_type", "boots"), with = F],
                          by = "scenario_id")
  # names(sim_bias_final)[grepl(".x", names(sim_bias_final))]
  
  return(sim_bias_final)
  
}

stopCluster(clust_out$cl)
# setdiff(names(results.clust[[1]]), names(results.clust[[5]]))
# setdiff(names(results.clust[[5]]), names(results.clust[[1]]))

results.clust.dat <- rbindlist(results.clust, fill = TRUE, use.names = TRUE) #, fill = TRUE
results.clust.dat

list.files("remote_mirrors/final_simulations/summaries/")
results.clust.dat
unique(results.clust.dat$scenario_id)

saveRDS(results.clust.dat, "remote_mirrors/final_simulations/summaries/all_scenarios_summarized.Rds")

# results.clust.dat readRDS("remote_mirrors/all_effect_sizes_august_2025/summaries/all_scenarios_summarized.Rds")
