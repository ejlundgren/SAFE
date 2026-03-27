
rm(list = ls())
library("data.table")
source('scripts/SAFE_function.R')
library("metafor")


full_guide <- readRDS("run_simulations/remote_mirrors/revision_paired_simulations/data/scenarios.Rds")
unique(full_guide$effect_type)
full_guide <- full_guide[!effect_type %in% c("lnOR", "lnRR")]
full_guide


formulas <- fread("run_simulations/remote_mirrors/revision_paired_simulations/data/effect_size_formulas.csv")

# March 27 ----------------------------------------------------------------
# First, make sure ROM paired returns the same result as escalc:

test <- unique(full_guide[effect_type == "lnRoM", .(scenario_id,
                                                    r, n, true_mean1, true_mean2,
                                                    true_sd1, true_sd2)])


escalc_out <- escalc(measure = "ROMC", 
                     m1i = true_mean1, m2i = true_mean2,
                     sd1i = true_sd1, sd2i = true_sd2,
                     ni = n, ri = r,
                     var.names = c("yi_escalc", "vi_escalc"),
                     data = test)

out <- eff_size(x1 = test$true_mean1, x2 = test$true_mean2,
                sd1 = test$true_sd1, sd2 = test$true_sd2,
                n = test$n, r = test$r,
                effect_type = "lnRoM_paired")
wtf <- cbind(escalc_out, out) |> setDT()
wtf
wtf[yi_escalc != yi_first]
wtf[vi_escalc != vi_first]
# Good news!
# That looks fine....

# SMD paired

escalc_out <- escalc(measure = "SMCRP", 
                     m1i = true_mean1, m2i = true_mean2,
                     sd1i = true_sd1, sd2i = true_sd2,
                     ni = n, ri = r,
                     var.names = c("yi_escalc", "vi_escalc"),
                     data = test)
#
out <- eff_size(x1 = test$true_mean1, x2 = test$true_mean2,
                sd1 = test$true_sd1, sd2 = test$true_sd2,
                n = test$n, r = test$r,
                effect_type = "SMD_paired")
wtf <- cbind(escalc_out, out) |> setDT()
wtf # Why doesn't the yi_first change with n?
wtf[, .(yi_escalc, yi_second, yi_safe)]
wtf[, .(n, r, vi_escalc, vi_first, vi_second, vi_safe)]


# Let's look at unpaired 

escalc_out <- escalc(measure = "SMD", 
                     m1i = true_mean1, m2i = true_mean2,
                     sd1i = true_sd1, sd2i = true_sd2,
                     n1i = n, n2i = n, #ri = r,
                     var.names = c("yi_escalc", "vi_escalc"),
                     data = test)
#
out <- eff_size(x1 = test$true_mean1, x2 = test$true_mean2,
                sd1 = test$true_sd1, sd2 = test$true_sd2,
                n1 = test$n, n2 = test$n,
                effect_type = "SMD")
wtf <- cbind(escalc_out, out) |> setDT()
wtf # Why doesn't the yi_first change with n?
wtf[, .(yi_escalc, yi_second, yi_safe)]
wtf[, .(n, vi_escalc, vi_first, vi_second, vi_safe)]


# lnCVR
escalc_out <- escalc(measure = "CVR", 
                     m1i = true_mean1, m2i = true_mean2,
                     sd1i = true_sd1, sd2i = true_sd2,
                     n1i = n, n2i = n, #ri = r,
                     var.names = c("yi_escalc", "vi_escalc"),
                     data = test)
#
out <- eff_size(x1 = test$true_mean1, x2 = test$true_mean2,
                sd1 = test$true_sd1, sd2 = test$true_sd2,
                n1 = test$n, n2 = test$n,
                effect_type = "lnCVR")
wtf <- cbind(escalc_out, out) |> setDT()
wtf # Why doesn't the yi_first change with n?
wtf[, .(yi_escalc, yi_second, yi_safe)]
wtf[, .(n, vi_escalc, vi_first, vi_second, vi_safe)]

#
escalc_out <- escalc(measure = "CVRC", 
                     m1i = true_mean1, m2i = true_mean2,
                     sd1i = true_sd1, sd2i = true_sd2,
                     ni = n, ri = r, #ri = r,
                     var.names = c("yi_escalc", "vi_escalc"),
                     data = test)
#
out <- eff_size(x1 = test$true_mean1, x2 = test$true_mean2,
                sd1 = test$true_sd1, sd2 = test$true_sd2,
                n = test$n, r = test$r,
                effect_type = "lnCVR_paired")
wtf <- cbind(escalc_out, out) |> setDT()
wtf # Why doesn't the yi_first change with n?
wtf[, .(yi_escalc, yi_second, yi_safe)]
wtf[, .(n, vi_escalc, vi_first, vi_second, vi_safe)]

CVRC

# March 25th -----------------------------------------------------------------

formulas <- fread("run_simulations/remote_mirrors/revision_paired_simulations/data/effect_size_formulas.csv")
formulas[name == "lnRoM", ]

formulas[name == "lnRoM_paired", ]

"(sd1^2 / (n * x1^2)) + (sd2^2 / (n * x2^2)) - ((2 * r * sd1 * sd2) / (x1 * x2 * sqrt(n^2)))"
#

formulas[name == "SMD_paired" & sim_family == "4_multivariate_normal_wishart", ]

full_guide <- readRDS("run_simulations/remote_mirrors/revision_paired_simulations/data/scenarios.Rds")
unique(full_guide$effect_type)
full_guide <- full_guide[!effect_type %in% c("lnOR", "lnRR")]
full_guide

#
test <- unique(full_guide[effect_type == "lnRoM", .(scenario_id,
                                                    r, n, true_mean1, true_mean2,
                                                    true_sd1, true_sd2)])

#
escalc_out <- escalc(measure = "ROMC", 
       m1i = true_mean1, m2i = true_mean2,
       sd1i = true_sd1, sd2i = true_sd2,
       ni = n, ri = r,
       var.names = c("yi_escalc", "vi_escalc"),
       data = test)

out <- eff_size(x1 = test$true_mean1, x2 = test$true_mean2,
         sd1 = test$true_sd1, sd2 = test$true_sd2,
         n = test$n, r = test$r,
         effect_type = "lnRoM_paired")
wtf <- cbind(escalc_out, out)
wtf

wtf[yi_escalc == yi_first, ]
nrow(wtf)
# That's good

wtf[vi_escalc == vi_first, ]
