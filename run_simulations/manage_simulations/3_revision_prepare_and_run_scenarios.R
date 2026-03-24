
# March 22nd, 2026
#
# AIM: Simulate the influence of different choices of 'r' on bias and relative bias
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

source("scripts/SAFE_function.R")
file.copy("scripts/SAFE_function.R",
          "run_simulations/remote_mirrors/revision_paired_simulations/remote_universal_SAFE.R",
          overwrite = T)
file.copy("data/effect_size_formulas.csv",
          "run_simulations/remote_mirrors/revision_paired_simulations/data/effect_size_formulas.csv",
          overwrite = T)
#
sim_results <- readRDS("builds/all_scenarios_summarized.Rds")

rerun <- F

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
guide2 <- unique(sim_results[effect_type %in% c("lnOR", "lnRR"), .(scenario_id, effect_type, true_p_a, true_p_c, 
                                                                   n1, n2, true_a, true_c,
                                                                    true_b, true_d)])

guide2

guide2[is.na(true_b), true_b := n1 - true_a]
guide2[is.na(true_d), true_d := n2 - true_c]
nrow(guide2)
nrow(unique(guide2[, .(true_p_a, true_p_c, n1, n2, true_a, true_c,
                       true_b, true_d)]))
guide2 <- unique(guide2[, .(effect_type, true_p_a, true_p_c, n1, n2, true_a, true_c,
                     true_b, true_d)])

final_guide <- rbind(guide, guide2,
                     fill = TRUE)
final_guide

final_guide[, scenario_id := paste("scenario", effect_type, 1:.N, sep = '_')]
final_guide

# Expand guide for 1k per core --------------------------------------------

# expanded.guide <- final_guide[rep(seq(1, nrow(final_guide)), 1000)]
# expanded.guide

# Add a batch ID:
# expanded.guide[, batch_id := seq(1:), by = .()]
# 
# expansion <- rbind(data.table(type = c("normal"),
#                               chunk = 1:100),
#                    data.table(type = c("binomial"),
#                               chunk = 101:200))
# expansion

# Not sure why I'm having such a hard time with this...
expansion <- data.table(effect_type = unique(final_guide$effect_type))
# expansion[, num := 1:.N]

expanded.guide <- expansion[rep(seq(1, nrow(expansion)), 1000)]
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


1e5 / 1000
length(unique(expanded.guide$batch_id))
# So 5000 cores and 100 iterations per core.
# That seems reasonable.

expanded.guide[, checkpoint_path := paste0("checkpoints/", batch_id, ".Rds")]
expanded.guide[, file_path := paste0("outputs/", batch_id, ".Rds")]

# Save --------------------------------------------------------------------

saveRDS(expanded.guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/scenarios.Rds")


# Manage ------------------------------------------------------------------

saveRDS(expanded.guide, "run_simulations/remote_mirrors/revision_paired_simulations/data/working_scenarios.Rds")


# OLD ---------------------------------------------------------------------

# 
# #
# clust_out <- prepare_cluster(n = monte_carlo_N)
# 
# foreach(i = start:monte_carlo_N, 
#         .options.snow = clust_out$options,
#         .errorhandling = "pass",
#         .packages = c("data.table", "MASS", "tmvtnorm")) %dopar% {
#           
#           # Created simulated data for each scenario:
#           sim_dat  <- lapply(1:nrow(guide), function(x){
#             sub_guide <- guide[x, ]
#             sigma_matrix <- matrix(data = c(sub_guide$true_sd1^2,
#                                             (sub_guide$r*sub_guide$true_sd1*sub_guide$true_sd2), #  
#                                             (sub_guide$r*sub_guide$true_sd1*sub_guide$true_sd2),      
#                                             (sub_guide$true_sd2^2)), #  
#                                    nrow = 2, ncol = 2)
#             
#             # Now let's draw our simulated data:
#             out <- MASS::mvrnorm(mu = c(sub_guide$true_mean1, sub_guide$true_mean2),
#                                  Sigma = sigma_matrix,
#                                  n = sub_guide$n) |> 
#               as.data.frame() |>
#               setDT()
#             
#             out <- out[, .(sim_mean1 = mean(V1),
#                            sim_sd1 = sd(V1),
#                            sim_mean2 = mean(V2),
#                            sim_sd2 = sd(V2))]
#             
#             return(cbind(out, sub_guide))
#           }) |> rbindlist()
#           head(sim_dat)
#           
#           # Now, calculate plugin and SAFE estimates:
#           lnrom_out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
#                                 sd1 = sim_dat$sim_sd1, sd2 = sim_dat$sim_sd2,
#                                 n = sim_dat$n, r = sim_dat$r,
#                                 effect_type = "lnRoM_paired",
#                                 SAFE_distribution = "2_multivariate_normal",
#                                 parallelize = FALSE,
#                                 SAFE = TRUE,
#                                 verbose = FALSE,
#                                 SAFE_boots = 1e6) 
#           lnrom_out[, effect_type := "lnRoM_paired"]
#           
#           smd_out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
#                               sd1 = sim_dat$sim_sd1, sd2 = sim_dat$sim_sd2,
#                               n = sim_dat$n, r = sim_dat$r,
#                               effect_type = "SMD_paired",
#                               SAFE_distribution = "4_multivariate_normal_wishart",
#                               parallelize = FALSE,
#                               SAFE = TRUE,
#                               verbose = FALSE,
#                               SAFE_boots = 1e6) 
#           smd_out[, effect_type := "SMD_paired"]
#           
#           res <- rbind(data.table(lnrom_out, guide),
#                        data.table(smd_out, guide),
#                        fill = TRUE)
#           res[, iter := i]
#           
#           saveRDS(res, file.path("builds", "r_simulations_raw", paste0("all_scenarios_i.", i, ".Rds")))
#           setTxtProgressBar(clust_out$progress, i)
#           
#         }
# stopCluster(clust_out$cluster)
# 
# #
# #
# #
# #
# #
# #
# 
