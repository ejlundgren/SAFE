
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

# Also, we'll create a theme object for ggplots to make code more readable:
theme_SAFE <- theme_bw()+
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        strip.background = element_blank())


source("scripts/SAFE_function.R")

rerun <- F

monte_carlo_N <- 1e5
files <- list.files("builds/r_simulations_raw/")

if(length(files) == 0){
  start <- 1
}else{
  iters_complete <- word(files, -2, sep = "[.]") |> as.numeric() |> max()
  start <- iters_complete+1
}

# Set up scenarios:
guide <- CJ(true_mean1 = 10.5,
            true_mean2 = 13,
            true_sd1 = 1.2,
            true_sd2 = 1.5,
            r = c(0, 0.5, 0.8),
            n = c(5, 15, 100))
guide[, scenario_id := paste0("scenario_", 1:.N)]

out <- c()
sim_dat <- c()
lnrom_out <- c()
smd_out <- c()
res <- c()

clust_out <- prepare_cluster(n = monte_carlo_N)

foreach(i = start:monte_carlo_N, 
        .options.snow = clust_out$options,
        .errorhandling = "pass",
        .packages = c("data.table", "MASS", "tmvtnorm")) %dopar% {
          
          # Created simulated data for each scenario:
          sim_dat  <- lapply(1:nrow(guide), function(x){
            sub_guide <- guide[x, ]
            sigma_matrix <- matrix(data = c(sub_guide$true_sd1^2,
                                            (sub_guide$r*sub_guide$true_sd1*sub_guide$true_sd2), #  
                                            (sub_guide$r*sub_guide$true_sd1*sub_guide$true_sd2),      
                                            (sub_guide$true_sd2^2)), #  
                                   nrow = 2, ncol = 2)
            
            # Now let's draw our simulated data:
            out <- MASS::mvrnorm(mu = c(sub_guide$true_mean1, sub_guide$true_mean2),
                                 Sigma = sigma_matrix,
                                 n = sub_guide$n) |> 
              as.data.frame() |>
              setDT()
            
            out <- out[, .(sim_mean1 = mean(V1),
                           sim_sd1 = sd(V1),
                           sim_mean2 = mean(V2),
                           sim_sd2 = sd(V2))]
            
            return(cbind(out, sub_guide))
          }) |> rbindlist()
          head(sim_dat)
          
          # Now, calculate plugin and SAFE estimates:
          lnrom_out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                                sd1 = sim_dat$sim_sd1, sd2 = sim_dat$sim_sd2,
                                n = sim_dat$n, r = sim_dat$r,
                                effect_type = "lnRoM_paired",
                                SAFE_distribution = "2_multivariate_normal",
                                parallelize = FALSE,
                                SAFE = TRUE,
                                verbose = FALSE,
                                SAFE_boots = 1e6) 
          lnrom_out[, effect_type := "lnOR"]
          
          smd_out <- eff_size(x1 = sim_dat$sim_mean1, x2 = sim_dat$sim_mean2,
                              sd1 = sim_dat$sim_sd1, sd2 = sim_dat$sim_sd2,
                              n = sim_dat$n, r = sim_dat$r,
                              effect_type = "SMD_paired",
                              SAFE_distribution = "4_multivariate_normal_wishart",
                              parallelize = FALSE,
                              SAFE = TRUE,
                              verbose = FALSE,
                              SAFE_boots = 1e6) 
          smd_out[, effect_type := "lnRR"]
          
          res <- rbind(data.table(lnrom_out, guide),
                       data.table(smd_out, guide),
                       fill = TRUE)
          res[, iter := i]
          
          saveRDS(res, file.path("builds", "continuity_correction_simulations", paste0("all_scenarios_i.", i, ".Rds")))
          setTxtProgressBar(clust_out$progress, i)
          
        }
stopCluster(clust_out$cluster)

#
#
#
#
#
#

