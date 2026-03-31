
# March 22nd, 2026
#
# AIM: Simulate the influence of different choices of 'r' on bias and relative bias
#
#
#
#
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

# Prepare workspace -------------------------------------------------------

# Copy functions to new remote mirror
source("scripts/SAFE_function.R")
file.copy("scripts/SAFE_function.R",
          "run_simulations/remote_mirrors/revision_paired_simulations/remote_universal_SAFE.R",
          overwrite = T)
file.copy("data/effect_size_formulas.csv",
          "run_simulations/remote_mirrors/revision_paired_simulations/data/effect_size_formulas.csv",
          overwrite = T)

#
sim_results <- readRDS("builds/all_scenarios_summarized.Rds")

monte_carlo_N <- 1e5
#
# Set up scenarios
# 'type' will be for the 2 data generating processes
guide <- CJ(true_mean1 = 13.4, true_mean2 = 16.1, 
            true_sd1 = 4.6, true_sd2 = 3.9,
            r = c(0, 0.5, 0.8),
            n = c(5, 15, 100),
            effect_type = c("SMD", "lnRoM", "lnCVR"))

# For continuity correction.
# Check the simulated data

final_guide <- copy(guide)
final_guide

final_guide[, scenario_id := paste("scenario", effect_type, 1:.N, sep = '_')]
final_guide

# Calculate true values --------------------------------------------------------------------
temp <- guide[effect_type == "lnRoM", ]
lnrom <- eff_size(effect_type = "lnRoM_paired",
                  n = temp$n, r = temp$r,
                  x1 = temp$true_mean1, x2 = temp$true_mean2,
                  sd1 = temp$true_sd1, sd2 = temp$true_sd2,
                  SAFE = FALSE)
lnrom <- cbind(temp, lnrom)
lnrom

temp <- guide[effect_type == "SMD", ]
smd <- eff_size(effect_type = "SMD_paired",
                  n = temp$n, r = temp$r,
                  x1 = temp$true_mean1, x2 = temp$true_mean2,
                  sd1 = temp$true_sd1, sd2 = temp$true_sd2,
                  SAFE = FALSE)
smd <- cbind(temp, smd)
smd

temp <- guide[effect_type == "lnCVR", ]
lncvr <- eff_size(effect_type = "lnCVR_paired",
                n = temp$n, r = temp$r,
                x1 = temp$true_mean1, x2 = temp$true_mean2,
                sd1 = temp$true_sd1, sd2 = temp$true_sd2,
                SAFE = FALSE)
lncvr <- cbind(temp, lncvr)
lncvr

final_guide <- rbind(lnrom, smd, lncvr, fill = TRUE)
setnames(final_guide, c("yi_first", "vi_first", "yi_second", "vi_second"),
         c("yi_first_true", "vi_first_true", "yi_second_true", "vi_second_true"))
final_guide

# Expand guide for 100 core per effect size --------------------------------------------

# Not sure why I'm having such a hard time with this...
expansion <- data.table(effect_type = unique(final_guide$effect_type))
# expansion[, num := 1:.N]

expanded.guide <- expansion[rep(seq(1, nrow(expansion)), 100)]
expanded.guide
expanded.guide[, batch_id := paste0(effect_type, "_batch_", 1:.N)]
expanded.guide[, chunk := 1:.N]
expanded.guide[, seed := chunk]
expanded.guide
expanded.guide[, .(min(chunk)),  by = effect_type]

# Now a cartesian join:
expanded.guide <- merge(expanded.guide,
                        final_guide,
                        by = "effect_type",
                        allow.cartesian = TRUE)
expanded.guide[batch_id == "SMD_batch_1"]
expanded.guide[, .(n = uniqueN(effect_type)), by = .(chunk)][n > 1]
# Must be 0 rows


1e5 / 100
length(unique(expanded.guide$batch_id))
# So 5000 cores and 100 iterations per core.
# That seems reasonable.

expanded.guide[, checkpoint_path := paste0("checkpoints/", batch_id, ".Rds")]
expanded.guide[, file_path := paste0("outputs/", batch_id, ".Rds")]

# Save --------------------------------------------------------------------

saveRDS(expanded.guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/scenarios.Rds")
saveRDS(expanded.guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")
