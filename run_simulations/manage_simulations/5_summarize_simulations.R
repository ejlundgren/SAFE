# Summarize the revision simulations
#
#
#
#
#
rm(list = ls())
library("data.table")
source('run_simulations/remote_mirrors/revision_paired_simulations/remote_universal_SAFE.R')

files <- data.table(path = list.files("run_simulations/remote_mirrors/revision_paired_simulations/outputs/",
                    full.names = T))
files <- files[!grepl("lnOR", path)]
files <- files[!grepl("lnRR", path)]
# For some mysterious reason some of the files aren't openable but are in the checkpoints (which should now be the full dataset length)
files[, checkpoint := gsub("outputs", "checkpoints", path)]
files[!file.exists(checkpoint)]

# dat <- lapply(files$path, readRDS)
# Uh oh
# dat <- list()

errors <- c()

for(i in 1:nrow(files)){
  tryCatch({
    dat[[i]] <- readRDS(files$path[i])
  },
  error = function(e){
    errors <- i
  })
  if(is.null(dat[[i]])){
    
    dat[[i]] <- readRDS(files$checkpoint[i]) |> rbindlist()
    
  }
} 

# dat
# errors
any(sapply(dat, is.null))
# Must be FALSE
is_null_vector <- sapply(dat, is.null)
# which(is_null_vector)
# readRDS(files[2181])# dat[[4061]]

# Weird.

dat <- rbindlist(dat, fill = T)
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
                            scenario_id)]
dat.estimates

# Calculate true values ---------------------------------------------------
lncvr <- eff_size(n = dat.estimates[effect_type == "lnCVR"]$n, 
                r = dat.estimates[effect_type == "lnCVR"]$r,
                x1 = dat.estimates[effect_type == "lnCVR"]$true_mean1, 
                x2 = dat.estimates[effect_type == "lnCVR"]$true_mean2,
                sd1 = dat.estimates[effect_type == "lnCVR"]$true_sd1,
                sd2 = dat.estimates[effect_type == "lnCVR"]$true_sd2,
                SAFE = FALSE,
                effect_type = "lnCVR_paired")
lncvr <- lncvr[, names(lncvr)[!grepl("vi", names(lncvr))], with = F]
setnames(lncvr, names(lncvr), paste0(names(lncvr), "_estimand"))
lncvr$scenario_id <- dat.estimates[effect_type == "lnCVR"]$scenario_id

lnrom <- eff_size(n = dat.estimates[effect_type == "lnRoM"]$n, 
                  r = dat.estimates[effect_type == "lnRoM"]$r,
                  x1 = dat.estimates[effect_type == "lnRoM"]$true_mean1, 
                  x2 = dat.estimates[effect_type == "lnRoM"]$true_mean2,
                  sd1 = dat.estimates[effect_type == "lnRoM"]$true_sd1,
                  sd2 = dat.estimates[effect_type == "lnRoM"]$true_sd2,
                  SAFE = FALSE,
                  effect_type = "lnRoM_paired")
lnrom
lnrom <- lnrom[, names(lnrom)[!grepl("vi", names(lnrom))], with = F]
setnames(lnrom, names(lnrom), paste0(names(lnrom), "_estimand"))
lnrom$scenario_id <- dat.estimates[effect_type == "lnRoM"]$scenario_id


smd <- eff_size(n = dat.estimates[effect_type == "SMD"]$n, 
                  r = dat.estimates[effect_type == "SMD"]$r,
                  x1 = dat.estimates[effect_type == "SMD"]$true_mean1, 
                  x2 = dat.estimates[effect_type == "SMD"]$true_mean2,
                  sd1 = dat.estimates[effect_type == "SMD"]$true_sd1,
                  sd2 = dat.estimates[effect_type == "SMD"]$true_sd2,
                  SAFE = FALSE,
                  effect_type = "SMD_paired")
smd
smd <- smd[, names(smd)[!grepl("vi", names(smd))], with = F]
setnames(smd, names(smd), paste0(names(smd), "_estimand"))
smd$scenario_id <- dat.estimates[effect_type == "SMD"]$scenario_id
smd

out <- rbind(lncvr, lnrom, smd, fill = TRUE)
out

final.dat <- merge(dat.estimates,
                   out,
                   by = "scenario_id")
final.dat

# Calculate bias and relative bias ----------------------------------------
# Point estimate bias:
final.dat[, point.first_estimate.first_estimand.bias := yi_first_estimate - yi_first_estimand]
final.dat[,  point.second_estimate.first_estimand.bias := yi_second_estimate - yi_first_estimand]
final.dat[,  point.second_estimate.second_estimand.bias := yi_second_estimate - yi_second_estimand]
final.dat[,  point.safe_estimate.first_estimand.bias := yi_safe_estimate - yi_first_estimand]
final.dat[,  point.safe_estimate.second_estimand.bias := yi_safe_estimate - yi_second_estimand]

final.dat

# Variance estimate bias:
final.dat[, var.first_estimate.first_estimand.rel_bias := ((vi_first_estimate - var_first_estimand) / var_first_estimand) * 100]
final.dat[, var.second_estimate.first_estimand.rel_bias := ((vi_second_estimate - var_first_estimand) / var_first_estimand) * 100]
final.dat[, var.safe_estimate.first_estimand.rel_bias := ((vi_safe_estimate - var_first_estimand) / var_first_estimand) * 100]

final.dat[, var.first_estimate.second_estimand.rel_bias := ((vi_first_estimate - var_second_estimand) / var_second_estimand) * 100]
final.dat[, var.second_estimate.second_estimand.rel_bias := ((vi_second_estimate - var_second_estimand) / var_second_estimand) * 100]
final.dat[, var.safe_estimate.second_estimand.rel_bias := ((vi_safe_estimate - var_second_estimand) / var_second_estimand) * 100]

final.dat[, var.first_estimate.safe_estimand.rel_bias := ((vi_first_estimate - var_safe_estimand) / var_safe_estimand) * 100]
final.dat[, var.second_estimate.safe_estimand.rel_bias := ((vi_second_estimate - var_safe_estimand) / var_safe_estimand) * 100]
final.dat[, var.safe_estimate.safe_estimand.rel_bias := ((vi_safe_estimate - var_safe_estimand) / var_safe_estimand) * 100]

nms <- names(final.dat)
nms <- nms[grepl("bias", names(final.dat))]
nms

dat.mlt <- melt(final.dat,
                measure.vars = nms,
                id.vars = c("scenario_id", "effect_type", "true_mean1", "true_mean2",
                            "true_sd1", "true_sd2", "r", "n"))
dat.mlt[, c("type", "estimate", "estimand", "measure") := tstrsplit(variable, "[.]")]


saveRDS(dat.mlt, "builds/paired_scenarios_bias.Rds")

