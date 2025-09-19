# September 2025
# Written by E.J. Lundgren
# Reviewed by XYZ
#
# AIM: Prepare scenarios for Monte Carlo simulations to test bias between SAFE and other methods and the influence of bootstrap length.
#
# The Monte Carlo simulations were run on a remote server (Canada Alliance). The code used on the server, for each chunk
# is in `remote_mirrors/SAFE_simulations.R`
#
#

rm(list = ls())

library("groundhog")
groundhog.library(pkg = c("data.table", 
                          "crayon", "MASS"),
                  date = "2025-04-15")
source('run_simulations/remote_mirrors/remote_universal_SAFE.R')

n_reps <- 1e5

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
guide <- list()
i <- 1
B <- c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8)

# >>> Reciprocal -----------------------
guide[[i]] <- CJ(true_mean = 10.5,
                 true_sd = 1.2,
                 sample_size = c(5, 10, 30, 100, 500),
                 boots = B)
guide[[i]][, scenario_id := paste0("scenario_", seq(1:.N))]

guide[[i]]
guide[[i]] <- guide[[i]][!(sample_size %in% c(10, 100) & B != 1e6)]
guide[[i]][, .(n_boots = uniqueN(boots)), by = sample_size]

# Repeat each scenario 1e5 times
# guide[[i]] <- guide[[i]][rep(seq(.N), n_reps), ]
guide[[i]][, effect_type := "reciprocal"]
# guide[[i]][, iter := seq(1:.N)]

i <- i + 1

# >>> lnRoM ---------------------------------------------------------------

guide[[i]] <- CJ(true_mean1 = 13.4, true_mean2 = 16.1, 
                 true_sd1 = 4.6, true_sd2 = 3.9, 
                 sample_size1 = c(5, 10, 50, 100, 150),
                 sample_size_ratio = 1,
                 boots = B)
guide[[i]][, scenario_id := paste0("scenario_", seq(1:.N))]

guide[[i]][, sample_size2 := round(sample_size1 * sample_size_ratio)]
guide[[i]] <- guide[[i]][sample_size2 >= 3, ]

guide[[i]] <- guide[[i]][!(sample_size1 %in% c(10, 100) & B != 1e6)]
guide[[i]][, .(n_boots = uniqueN(boots)), by = sample_size1]

guide[[i]] 

guide[[i]][, effect_type := "lnRoM"]

i <- i + 1

# >>> SMD 4-normal---------------------------------------------------------------

guide[[i]] <-CJ(true_mean1 = 13.4, true_mean2 = 16.1, 
                true_sd1 = 4.6, true_sd2 = 3.9, 
                sample_size1 = c(5, 10, 50, 100, 150),
                sample_size_ratio = 1,
                boots = B)
guide[[i]][, scenario_id := paste0("scenario_", seq(1:.N))]

guide[[i]][, sample_size2 := round(sample_size1 * sample_size_ratio)]
guide[[i]] <- guide[[i]][sample_size2 >= 3, ]


guide[[i]] <- guide[[i]][!(sample_size1 %in% c(10, 100) & B != 1e6)]
guide[[i]][, .(n_boots = uniqueN(boots)), by = sample_size1]

# Repeat each scenario 1e5 times
# guide[[i]] <- guide[[i]][rep(seq(.N), n_reps), ]
guide[[i]][, effect_type := "SMD_normal"]
# guide[[i]][, iter := seq(1:.N)]
guide[[i]]

i <- i + 1

# >>> SMD Wishart---------------------------------------------------------------

guide[[i]] <- CJ(true_mean1 = 13.4, true_mean2 = 16.1, 
                 true_sd1 = 4.6, true_sd2 = 3.9, 
                 sample_size1 = c(5, 10, 50, 100, 150),
                 sample_size_ratio = 1,
                 boots = B)
guide[[i]][, scenario_id := paste0("scenario_", seq(1:.N))]

guide[[i]][, sample_size2 := round(sample_size1 * sample_size_ratio)]
guide[[i]] <- guide[[i]][sample_size2 >= 3, ]

guide[[i]] <- guide[[i]][!(sample_size1 %in% c(10, 100) & B != 1e6)]
guide[[i]][, .(n_boots = uniqueN(boots)), by = sample_size1]

# Repeat each scenario 1e5 times
# guide[[i]] <- guide[[i]][rep(seq(.N), n_reps), ]
guide[[i]][, effect_type := "SMD_Wishart"]
# guide[[i]][, iter := seq(1:.N)]

i <- i + 1

# >>> lnOR ----------------------------------------------------------------

guide[[i]]  <- CJ(pr_a = c(0.3), 
                  pr_c = c(0.8), 
                  n1 = c(10, 20, 50, 100, 500),
                  n_ratio = c(1),
                  boots = B) 
guide[[i]] [, n2 := round(n1 * n_ratio)]
guide[[i]] [, `:=` (true_a = round(n1*pr_a),
                    true_c = round(n2*pr_c))]
guide[[i]] [, `:=` (true_b = n1-true_a,
                    true_d = n2-true_c)]
guide[[i]][true_a + true_b != n1, ]
guide[[i]][true_a + true_b != n2, ]
#' [Must be 0 rows]

guide[[i]] <- guide[[i]][!(n1 %in% c(20, 100) & B != 1e6)]
guide[[i]][, .(n_boots = uniqueN(boots)), by = n1]


guide[[i]] [, scenario_id := paste0("scenario_", seq(1:.N))]
guide[[i]] 

guide[[i]] [true_a == 0 | true_c == 0 | true_b == 0 | true_d == 0, ]
guide[[i]] <- guide[[i]] [!(true_a == 0 | true_c == 0 | true_b == 0 | true_d == 0), ]

guide[[i]][, effect_type := "lnOR"]
# guide[[i]][, iter := seq(1:.N)]

length(unique(guide[[i]]$scenario_id))
# Lots of scenarios

i <- i + 1

# >>> lnRR ----------------------------------------------------------------
guide[[i]] <- CJ(pr_a = c(0.3), 
                 pr_c = c(0.8),
                 n1 = c(10, 20, 50, 100, 500),
                 n_ratio = c(1),
                 boots = B) 

guide[[i]][, n2 := n1 * n_ratio]

guide[[i]][, `:=` (true_a = round(n1*pr_a),
                   true_c = round(n2*pr_c))]

guide[[i]] <- guide[[i]][!(true_a == 0 | true_c == 0), ]
guide[[i]]

guide[[i]] <- guide[[i]][true_a > 0 & true_c > 0, ]
# Hmmm. 
guide[[i]] <- guide[[i]][!(n1 %in% c(20, 100) & B != 1e6)]
guide[[i]][, .(n_boots = uniqueN(boots)), by = n1]


guide[[i]][, scenario_id := paste0("scenario_", seq(1:.N))]
length(unique(guide[[i]]$scenario_id))
# 100

guide[[i]][, effect_type := "lnRR"]

i <- i + 1

# >>> lnCVR 4-normal ----------------------------------------------------------------

guide[[i]] <- CJ(true_mean1 = 13.4, true_mean2 = 16.1, 
                 true_sd1 = 4.6, true_sd2 = 3.9, 
                 sample_size1 = c(5, 10, 50, 100, 150),
                 sample_size_ratio = 1,
                 boots = B)
guide[[i]][, scenario_id := paste0("scenario_", seq(1:.N))]

guide[[i]][, sample_size2 := round(sample_size1 * sample_size_ratio)]
guide[[i]] <- guide[[i]][sample_size2 >= 3, ]


guide[[i]] <- guide[[i]][!(sample_size1 %in% c(10, 100) & B != 1e6)]
guide[[i]][, .(n_boots = uniqueN(boots)), by = sample_size1]

guide[[i]]
guide[[i]][, effect_type := "lnCVR_normal"]

guide[[i]]

i <- i + 1

# >>> lnCVR 4-wishart ----------------------------------------------------------------

guide[[i]] <- CJ(true_mean1 = 13.4, true_mean2 = 16.1, 
                 true_sd1 = 4.6, true_sd2 = 3.9, 
                 sample_size1 = c(5, 10, 50, 100, 150),
                 sample_size_ratio = 1,
                 boots = B)

guide[[i]] <- guide[[i]][!(sample_size1 %in% c(10, 100) & B != 1e6)]
guide[[i]][, .(n_boots = uniqueN(boots)), by = sample_size1]


guide[[i]][, scenario_id := paste0("scenario_", seq(1:.N))]

guide[[i]][, sample_size2 := round(sample_size1 * sample_size_ratio)]
guide[[i]] <- guide[[i]][sample_size2 >= 3, ]

guide[[i]]
guide[[i]][, effect_type := "lnCVR_Wishart"]

i <- i + 1

# >>> HWE ----------------------------------------------------------------

guide[[i]] <- CJ(p_AA = c(.25),#seq(from = 0.1, to = 1, by = 0.1),
                 p_Aa = c(.5), #seq(from = 0.1, to = 1, by = 0.1),
                 p_aa = c(.25),#seq(from = 0.1, to = 1, by = 0.1),
                 n = c(10, 50, 100, 300, 500),
                 boots = B)
guide[[i]] <- guide[[i]][p_AA + p_Aa + p_aa == 1.0, ]

guide[[i]] <- guide[[i]][p_AA + p_Aa + p_aa == 1.0, ]
guide[[i]]

guide[[i]] <- guide[[i]][!(n %in% c(50, 300) & B != 1e6)]
guide[[i]][, .(n_boots = uniqueN(boots)), by = n]

guide[[i]][, scenario_id := paste0("scenario_", seq(1:.N))]

guide[[i]]

guide[[i]][, `:=` (true_n_AA = p_AA * n,
                   true_n_Aa = p_Aa * n,
                   true_n_aa = p_aa * n)]

guide[[i]][, effect_type := "lnHWE_A"]
guide[[i]][true_n_AA == 0 | true_n_Aa == 0 | true_n_aa == 0, ]
# hopefully 0 rows

length(unique(guide[[i]]$scenario_id))
# 12. Much more reasonable.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------
# COMBINE guide -----------------------------------------------------------

master_guide <- rbindlist(guide, fill = TRUE)
# master_guide

master_guide

master_guide[, scenario_id := paste0(effect_type, "_", scenario_id)]


master_guide[, .(n_boots = uniqueN(boots)), by = .(scenario_id)][n_boots > 1, ]
# Must be 0 rows.
# >>> Replicate into chunks, with each chunk being a separate cluster node --------------------------------------

# These have to be run separately:
master_guide[, batch_ID := paste0(effect_type, "_", boots)]
master_guide
#
length(unique(master_guide$batch_ID)) # 66 batches that must be run on independent clusters

master_guide[, total_iterations_needed := 1e5]

master_guide[boots == 1e+01, iterations_per_core := 1e5/2]
master_guide[boots == 1e+02, iterations_per_core := 1e5/2]
master_guide[boots == 1e+03, iterations_per_core := 1e5/2]
master_guide[boots == 1e+04, iterations_per_core := 1e5/2]
master_guide[boots == 1e+05, iterations_per_core := 1e5/10]
master_guide[boots == 1e+06, iterations_per_core := 1e5/20]
master_guide[boots == 1e+07, iterations_per_core := 1e5/50]
master_guide[is.na(iterations_per_core), ]
# Must be 0 rows

master_guide[, number_of_cores_needed := round(total_iterations_needed/iterations_per_core)]
master_guide

expansion_guide <- unique(master_guide[, .(batch_ID, number_of_cores_needed)])
sum(expansion_guide$number_of_cores_needed)
# ~3000 cores seems to be the limit in a batch call to the Fir cluster.

expansion_guide

rep(seq(1, nrow(expansion_guide)), expansion_guide$number_of_cores_needed)

# Next time just use this method in the master_guide.
expanded.guide <- expansion_guide[rep(seq(1, nrow(expansion_guide)), expansion_guide$number_of_cores_needed)]
expanded.guide[, run_ID := seq(1:.N)]
expanded.guide

expanded.guide.mrg <- merge(expanded.guide,
                            master_guide,
                            by = "batch_ID",
                            all.x = T,
                            all.y = T,
                            allow.cartesian = T)

expanded.guide.mrg[number_of_cores_needed.x != number_of_cores_needed.y]
# Must be 0 rows
expanded.guide.mrg$number_of_cores_needed.x <- NULL
setnames(expanded.guide.mrg, "number_of_cores_needed.y", "number_of_cores_needed")

expanded.guide.mrg[, seed := run_ID]
expanded.guide.mrg[, run_ID := paste0(batch_ID, "_run", run_ID)]

expanded.guide.mrg


expanded.guide.mrg[, .(n_uniq_boots = uniqueN(boots),
                  n_effects = uniqueN(effect_type),
                  total_iterations_provided = sum(iterations_per_core)),
              by = .(scenario_id)][total_iterations_provided != 1e+05 | n_uniq_boots > 1 | n_effects > 1]
# Must be 0 rows

expanded.guide.mrg

setnames(expanded.guide.mrg, c("pr_a", "pr_c"), c("true_p_a", "true_p_c"))

#
expanded.guide.mrg[, file_path := paste0("outputs/", run_ID, ".Rds")]
expanded.guide.mrg

expanded.guide.mrg

expanded.guide.mrg[, checkpoint_path := paste0("checkpoints/", run_ID, "_checkpoint.Rds")]
expanded.guide.mrg
# run_guide.mrg[, checkpoint_path := paste0("checkpoints/", scenario_id, 
#                                           "_run", run_ID, "_checkpoint.Rds")]
unique(expanded.guide.mrg$effect_type)

# >>> Add chunk -----------------------------------------------------------
expanded.guide.mrg[, chunk := .GRP, by = .(run_ID)]
expanded.guide.mrg

# >>> Save ----------------------------------------------------------------
saveRDS(expanded.guide.mrg, "remote_mirrors/final_simulations/data/scenarios.Rds")

expanded.guide.mrg
