# March 23rd, 2026
#
#
# Calculate bias and relative bias
#
#
#

rm(list = ls())

# Now load packages:
library("pacman")
p_load(data.table, MASS, crayon, 
       tmvtnorm, metafor,
       parallel, foreach, doSNOW,
       ggplot2, patchwork, scico,
       stringr)


res <- list.files("builds/r_simulations_raw/", full.names = T)
res <- lapply(res, readRDS) |> rbindlist()
res

# Calculate average
bias_df <- res[, .(mean_yi_first = mean(yi_first), # mean MC yi estimates
                    mean_yi_second = mean(yi_second),
                    mean_yi_safe = mean(yi_safe),
                   
                    mean_vi_first = mean(vi_first), # mean MC vi estimates
                    mean_vi_second = mean(vi_second),
                    mean_vi_safe = mean(vi_safe),
                   
                    vi_first_estimand = var(yi_first), # vi estimands
                    vi_second_estimand = var(yi_second),
                    vi_safe_estimand = var(yi_safe)),
               by = .(effect_type, true_mean1, true_mean2, 
                      true_sd1, true_sd2, r, n, scenario_id)]




# Now calculate bias:





# OLD:



# Calculate bias:
bias <- function(estimates, estimand){
  return(mean(estimates) - unique(estimand))
}

point.bias <- res[, .(plugin_bias = bias(yi_plugin, true_yi),
                      bias_corrected_safe_bias = bias(yi_safe, true_yi),
                      uncorrected_safe_bias = bias(mean_safe, true_yi)),
                  by = .(scenario_id, sample_n)] |> unique()
head(point.bias)

# Melt this to make it plottable:
point.bias.long <- melt(point.bias,
                        id.vars = c("scenario_id", "sample_n"),
                        value.name = "bias",
                        variable.name = "estimator")

head(point.bias.long)

# Sort by sample size 
setorder(point.bias.long, sample_n)

# Relative bias
res[, var_estimand_plugin := var(yi_plugin), 
    by = .(scenario_id, sample_n)]
res[, var_estimand_SAFE := var(yi_safe), 
    by = .(scenario_id, sample_n)]

# To make this easier to read, we'll encapsulate the relative bias in a function:
relative_bias <- function(estimates,
                          estimand){
  ((mean(estimates) - estimand ) / estimand) * 100
}

# Now summarize by calculating relative bias per scenario ID and sample size
var.bias <- res[, .(SAFE_estimate.plugin_estimand = relative_bias(vi_safe, unique(var_estimand_plugin)),
                    SAFE_estimate.SAFE_estimand = relative_bias(vi_safe, unique(var_estimand_SAFE)),
                    plugin_estimate.plugin_estimand = relative_bias(vi_plugin, unique(var_estimand_plugin)),
                    plugin_estimate.SAFE_estimand = relative_bias(vi_plugin, unique(var_estimand_SAFE))),
                by = .(scenario_id, sample_n)] |> unique()

# Now melt:
var.bias.long <- melt(var.bias,
                      id.vars = c("scenario_id", "sample_n"))
head(var.bias.long)

# Split the 'variable' into estimator and estimand for plotting
var.bias.long[, c("Estimator", "Estimand") := tstrsplit(variable, ".", fixed = TRUE)]

# Sort the dataset by sample size
setorder(var.bias.long, sample_n)
