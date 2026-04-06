# Summarize the revision simulations
#
#
#
#

rm(list = ls())
library("data.table")

local <- FALSE
if(local){
  setwd("run_simulations/remote_mirrors/revision_paired_simulations")
}

files <- data.table(checkpoint = list.files("checkpoints/",
                    full.names = T))

#' [Somehow some of the files are corrupted. But I don't think the checkpoints are.]

# readRDS(files[i, ]$checkpoint)
dat <- lapply(files$checkpoint, function(x) y <- readRDS(x) |> rbindlist() )
test <- lapply(dat, function(x) max(x$iter)) |> unlist() |> unique()
test

dat <- rbindlist(dat, fill = TRUE)

# Add scenario id ---------------------------------------------------------
dat[, scenario_id := paste0("scenario_", effect_type, "_", .GRP),
    by = .(effect_type, true_mean1, true_mean2, true_sd1, true_sd2, r, n)]
dat

#
rs <- dat[, .(min_sim_r = min(sim_r), 
              max_sim_r  = max(sim_r), 
              mean_sim_r = mean(sim_r),
              sd_sim_r = sd(sim_r)),
          by = .(r, n, effect_type)]
rs[, mean_sim_r := round(mean_sim_r, 3)]

# Calculate mean and var --------------------------------------------------
dat
dat.estimates <- dat[, .(yi_first_estimate = mean(sim_y_plugin_1st),
                         yi_second_estimate = mean(sim_y_plugin_2nd),
                         yi_safe_estimate = mean(yi_safe),
                         
                         vi_first_estimate = mean(sim_v_plugin_1st),
                         vi_second_estimate = mean(sim_v_plugin_2nd),
                         vi_safe_estimate = mean(vi_safe),
                         
                         var_first_estimand = var(sim_y_plugin_1st),
                         var_second_estimand = var(sim_y_plugin_2nd),
                         var_safe_estimand = var(yi_safe)),
                     by = .(effect_type, true_mean1, true_mean2, 
                            true_sd1, true_sd2, r, n,
                            scenario_id, yi_first_true, yi_second_true)]
dat.estimates

# drop 2nd point estimand. It's not real. 
setnames(dat.estimates, 
         c("yi_first_true", "yi_second_true"), 
          c("yi_first_estimand", "yi_second_estimand"))

# Calculate bias and relative bias ----------------------------------------
# Point estimate bias:
dat.estimates[, point.first_estimate.first_estimand.bias := yi_first_estimate - yi_first_estimand]
dat.estimates[,  point.second_estimate.first_estimand.bias := yi_second_estimate - yi_first_estimand]
dat.estimates[,  point.second_estimate.second_estimand.bias := yi_second_estimate - yi_second_estimand]
dat.estimates[,  point.safe_estimate.first_estimand.bias := yi_safe_estimate - yi_first_estimand]
dat.estimates[,  point.safe_estimate.second_estimand.bias := yi_safe_estimate - yi_second_estimand]

dat.estimates

# Variance estimate bias:
dat.estimates[, var.first_estimate.first_estimand.rel_bias := ((vi_first_estimate - var_first_estimand) / var_first_estimand) * 100]
dat.estimates[, var.second_estimate.first_estimand.rel_bias := ((vi_second_estimate - var_first_estimand) / var_first_estimand) * 100]
dat.estimates[, var.safe_estimate.first_estimand.rel_bias := ((vi_safe_estimate - var_first_estimand) / var_first_estimand) * 100]

dat.estimates[, var.first_estimate.second_estimand.rel_bias := ((vi_first_estimate - var_second_estimand) / var_second_estimand) * 100]
dat.estimates[, var.second_estimate.second_estimand.rel_bias := ((vi_second_estimate - var_second_estimand) / var_second_estimand) * 100]
dat.estimates[, var.safe_estimate.second_estimand.rel_bias := ((vi_safe_estimate - var_second_estimand) / var_second_estimand) * 100]

dat.estimates[, var.first_estimate.safe_estimand.rel_bias := ((vi_first_estimate - var_safe_estimand) / var_safe_estimand) * 100]
dat.estimates[, var.second_estimate.safe_estimand.rel_bias := ((vi_second_estimate - var_safe_estimand) / var_safe_estimand) * 100]
dat.estimates[, var.safe_estimate.safe_estimand.rel_bias := ((vi_safe_estimate - var_safe_estimand) / var_safe_estimand) * 100]

nms <- names(dat.estimates)
nms <- nms[grepl("bias", names(dat.estimates))]
nms

dat.mlt <- melt(dat.estimates,
                measure.vars = nms,
                id.vars = c("scenario_id", "effect_type", "true_mean1", "true_mean2",
                            "true_sd1", "true_sd2", "r", "n"))
dat.mlt[, c("type", "estimate", "estimand", "measure") := tstrsplit(variable, "[.]")]
dat.mlt

if(!file.exists("summaries")) dir.create("summaries")
saveRDS(dat.mlt, "summaries/paired_scenarios_bias.Rds")

