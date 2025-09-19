#
#
# Write and test all effect size formulas
#
#

# 0. Prepare environment --------------------------------------------------

rm(list = ls())

library("groundhog")
groundhog.library(pkg = c("data.table", 
                          "crayon", #"pryr",
                          "MASS", "metafor"),
                  date = "2025-04-15")


# 1. Create effect size table ---------------------------------------------


effect_formulas <- data.table(name = c("reciprocal",
                                       "lnRoM",
                                       "lnRoM",
                                       #' [Need to change Cohens d and Hedges g to have matching names...]
                                       "SMD", # Cohen's d
                                       "SMD",# Hedges' g
                                       "SMD_paired", # Cohen's d
                                       "SMD_paired", # Hedges' g
                                       #' [Wishart SMD:]
                                       "SMD", # Cohen's d
                                       "SMD",# Hedges' g
                                       "SMD_paired", # Cohen's d
                                       "SMD_paired", # Hedges' g
                                       # 
                                       "lnRoM_paired",
                                       "lnOR",
                                       "lnRR",
                                       "lnCVR",
                                       "lnCVR",
                                       "lnCVR_paired",
                                       "lnCVR_paired",
                                       #' [Wishart lnCVR:]
                                        "lnCVR",
                                       "lnCVR",
                                       "lnCVR_paired",
                                       "lnCVR_paired",
                                       #
                                       "lnHWE_A",
                                       "lnM",
                                       "lnM_paired"
                                       ),
                              derivative = c("first", # reciprocal
                                        "first", #lnRoM first
                                        "second",# lnRoM second
                                        "first",# Cohen's d
                                        "second", # Hedgse' g
                                        "first",# Cohen's d paired
                                        "second", # Hedgse' g paired
                                        
                                        "first",# Cohen's d WISHART
                                        "second", # Hedgse' g WISHART
                                        "first",# Cohen's d paired WISHART
                                        "second", # Hedgse' g paired WISHART
                                        
                                        "first", # lnRoM_paired
                                        "first", # 'lnOR
                                        "first", #lnRR
                                        "first", # lnCVR first order
                                        "second", # lnCVR second order
                                        "first", # lnCVR paired first order
                                        "second", # lnCVR paired second order
                                        
                                        "first", # lnCVR first order WISHART
                                        "second", # lnCVR second order WISHART
                                        "first", # lnCVR paired first order WISHART
                                        "second", # lnCVR paired second order WISHART
                                        
                                        "first", # lnHWE_A
                                        "first", # lnM
                                        "first" # lnM_paired
                              ),
                              effect_size = c("1 / x",# reciprocal
                                              "log(x1 / x2)",#lnRox first
                                              "log(x1 / x2) + 0.5 * (sd1^2/(n1 * x1^2) - sd2^2/(n2 * x2^2))", # lnRox second
                                              "(x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) )", # Cohen's d
                                              "(1 - 3 / (4 * (n1 + n2 - 2) - 1) ) * ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ) )", # Hedges' g
                                              "(x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) )", # Cohen's d PAIRED
                                              "(1 - 3 / (4 * (n1 + n2 - 2) - 1) ) * ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ) )", # Hedges' g PAIRED
                                              #' [WISHART:]
                                              "(x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) )", # Cohen's d
                                              "(1 - 3 / (4 * (n1 + n2 - 2) - 1) ) * ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ) )", # Hedges' g
                                              "(x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) )", # Cohen's d PAIRED
                                              "(1 - 3 / (4 * (n1 + n2 - 2) - 1) ) * ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ) )", # Hedges' g PAIRED
                                              #
                                              "log(x1 / x2)", # lnRox_paired
                                              "log(((a) * (d)) / ((b) * (c)))", # lnOR
                                              "log( ((a) / n1) / ((c)/ n2) )", # lnRR
                                              "log(sd1 / x1) - log(sd2 / x2)", # lnCVR first order
                                              "log((sd1 / x1) / (sd2 / x2)) + 1/2 * (1 / (n1 - 1) - 1 / (n2 - 1)) + 1/2 * ((sd2^2/(n2 * x2^2)) - (sd1^2 / (n1 * x1^2)))", # lnCVR second order
                                              "log(sd1 / x1) - log(sd2 / x2)", # lnCVR paired first order
                                              "log((sd1 / x1) / (sd2 / x2)) + 1/2 * (1 / (n1 - 1) - 1 / (n2 - 1)) + 1/2 * ((sd2^2/(n2 * x2^2)) - (sd1^2 / (n1 * x1^2)))", #lnCVR paired second order
                                              # WISHART:
                                              "log(sd1 / x1) - log(sd2 / x2)", # lnCVR first order
                                              "log((sd1 / x1) / (sd2 / x2)) + 1/2 * (1 / (n1 - 1) - 1 / (n2 - 1)) + 1/2 * ((sd2^2/(n2 * x2^2)) - (sd1^2 / (n1 * x1^2)))", # lnCVR second order
                                              "log(sd1 / x1) - log(sd2 / x2)", # lnCVR paired first order
                                              "log((sd1 / x1) / (sd2 / x2)) + 1/2 * (1 / (n1 - 1) - 1 / (n2 - 1)) + 1/2 * ((sd2^2/(n2 * x2^2)) - (sd1^2 / (n1 * x1^2)))", #lnCVR paired second order
                                              #
                                              "log((n_Aa/(n_AA + n_Aa + n_aa)) / (2*sqrt((n_AA/(n_AA + n_Aa + n_aa)) * (n_aa/(n_AA + n_Aa + n_aa)))))", # lnHWE_A
                                              "log(sqrt(((((n1 * n2) / (n1 + n2)) * (x1 - x2)^2) - (((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))) / ((2 * n1 * n2) / (n1 + n2)))) - log(sqrt((((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))))", #lnM
                                              "log(sqrt(((((n1 * n2) / (n1 + n2)) * (x1 - x2)^2) - (((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))) / ((2 * n1 * n2) / (n1 + n2)))) - log(sqrt((((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))))" #lnM_paired
                              ),
                              sampling_variance = c("(sd^2 / n) * (-1 * x ^ -2)^2", # reciprocal 
                                                    "sd1^2 / (n1 * x1^2) + sd2^2 / (n2 * x2^2)", #lnRox first
                                                    "sd1^2 / (n1 * x1^2) + sd2^2 / (n2 * x2^2) + 0.5 * ( (sd1^4 / (n1^2 * x1^4)) + (sd2^4 / (n2^2 * x2^4)))", # lnRoM second
                                                    "((n1 + n2) / (n1 * n2)) + ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ))^2 / (2 * (n1 + n2 - 2))", # Cohen's d
                                                    "(1 - 3 / (4 * (n1 + n2 - 2) - 1) )^2 * ((n1 + n2) / (n1 * n2)) + ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ))^2 / (2 * (n1 + n2 - 2))", # Hedges' g
                                                    "((n1 + n2) / (n1 * n2)) + ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ))^2 / (2 * (n1 + n2 - 2))", # Cohen's d paired
                                                    "(1 - 3 / (4 * (n1 + n2 - 2) - 1) )^2 * ((n1 + n2) / (n1 * n2)) + ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ))^2 / (2 * (n1 + n2 - 2))", # Hedges' g paired
                                                    #' *WISHART:*
                                                    "((n1 + n2) / (n1 * n2)) + ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ))^2 / (2 * (n1 + n2 - 2))", # Cohen's d
                                                    "(1 - 3 / (4 * (n1 + n2 - 2) - 1) )^2 * ((n1 + n2) / (n1 * n2)) + ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ))^2 / (2 * (n1 + n2 - 2))", # Hedges' g
                                                    "((n1 + n2) / (n1 * n2)) + ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ))^2 / (2 * (n1 + n2 - 2))", # Cohen's d paired
                                                    "(1 - 3 / (4 * (n1 + n2 - 2) - 1) )^2 * ((n1 + n2) / (n1 * n2)) + ((x1 - x2) / sqrt( ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2) ))^2 / (2 * (n1 + n2 - 2))", # Hedges' g paired
                                                    #
                                                    "(sd1^2 / (n1 * x1^2)) + (sd2^2 / (n2 * x2^2)) - ((2 * r * sd1 * sd2) / (x1 * x2 * sqrt(n1 * n2)))", # lnRox_paired
                                                    "(1 / (a)) + (1 / (b)) + (1 / (c)) + (1 / (d))", # lnOR
                                                    "(1 - (a / n1)) / a + (1 - (c / n2)) / c",# lnRR
                                                    "sd1^2/(n1 * x1^2) + sd2^2/(n2 * x2^2) + 1/(2*(n1 - 1)) + 1/(2*(n2 - 1))", # lnCVR first order 
                                                    "sd1^2/(n1 * x1^2) + sd1^4/(2 * n1^2 * x1^4) + n1/(2*(n1 - 1)^2) + sd2^2/(n2 * x2^2) + sd2^4/(2 * n2^2 * x2^4) + n2/(2*(n2 - 1)^2)", # lnCVR second order
                                                    "sd1^2/(n1 * x1^2) + sd2^2/(n1 * x2^2) - 2*r*sd1*sd2/(n1 * x1 * x2) + 1/(n1 - 1) - r^2/(n1 - 1)", # lnCVR_paired first order
                                                    "sd1^2/(n1 * x1^2) + sd1^4/(2 * n1^2 * x1^4) + sd2^2/(n1 * x2^2) + sd2^4/(2 * n1^2 * x2^4) - 2*r*sd1*sd2/(n1 * x1 * x2) + r^2 * sd1^2 * sd2^2 * (x1^4 + x2^4) / (2 * n1^2 * x1^4 * x2^4) + n1/(n1 - 1)^2 - r^2/(n1 - 1) + r^4 * (sd1^8 + sd2^8) / (2 * (n1 - 1)^2 * sd1^4 * sd2^4)", # lnCVR paired second order
                                                    # WISHART:
                                                    "sd1^2/(n1 * x1^2) + sd2^2/(n2 * x2^2) + 1/(2*(n1 - 1)) + 1/(2*(n2 - 1))", # lnCVR first order 
                                                    "sd1^2/(n1 * x1^2) + sd1^4/(2 * n1^2 * x1^4) + n1/(2*(n1 - 1)^2) + sd2^2/(n2 * x2^2) + sd2^4/(2 * n2^2 * x2^4) + n2/(2*(n2 - 1)^2)", # lnCVR second order
                                                    "sd1^2/(n1 * x1^2) + sd2^2/(n1 * x2^2) - 2*r*sd1*sd2/(n1 * x1 * x2) + 1/(n1 - 1) - r^2/(n1 - 1)", # lnCVR_paired first order
                                                    "sd1^2/(n1 * x1^2) + sd1^4/(2 * n1^2 * x1^4) + sd2^2/(n1 * x2^2) + sd2^4/(2 * n1^2 * x2^4) - 2*r*sd1*sd2/(n1 * x1 * x2) + r^2 * sd1^2 * sd2^2 * (x1^4 + x2^4) / (2 * n1^2 * x1^4 * x2^4) + n1/(n1 - 1)^2 - r^2/(n1 - 1) + r^4 * (sd1^8 + sd2^8) / (2 * (n1 - 1)^2 * sd1^4 * sd2^4)", # lnCVR paired second order
                                                    #
                                                    "(1/(n_AA + n_Aa + n_aa)) * ((1/(n_Aa / (n_AA + n_Aa + n_aa))) + (1-(n_Aa / (n_AA + n_Aa + n_aa)))/(4*(n_AA / (n_AA + n_Aa + n_aa))*(n_aa / (n_AA + n_Aa + n_aa))))", # lnHWE_A
                                                    "(1 / 4 * ((((n1 * n2) / (n1 + n2)) * (x1 - x2)^2) - (((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2)))^2) * ( (n0/2)^2 * (2 * (sd1^2 / n1 + sd2^2 / n2)^4 + 4 * (sd1^2 / n1 + sd2^2 / n2)^2 * (x1 - x2)^2) + ((((n1 * n2) / (n1 + n2)) * (x1 - x2)^2)^2 / (((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))^2) * ((2 * (((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))^2) / n1 + n2 - 2) )", #lnM
                                                    "(1 / 4 * ((((n1 * n2) / (n1 + n2)) * (x1 - x2)^2) - (((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2)))^2) * ( (n1/2)^2 * ( ((2 * (sd1^2 / n1 + sd2^2 / n2)^4)/n1^2) + ((4 * (sd1^2 / n1 + sd2^2 / n2)^2 * (x1 - x2)^2)/n1) ) ) + ((((n1 * n2) / (n1 + n2)) * (x1 - x2)^2)^2 / (((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))^2) * ((sd1^4 + sd2^4 + 2*r^2*sd1^2*sd2^2) / (2 * (n-1)))" # lnM_paired
                                                    ),
                              sim_family = c( # for now, number of simulated terms + distribution type
                                "1_normal",# "reciprocal",
                                "2_multivariate_normal",# "lnRoM",
                                "2_multivariate_normal",# "lnRoM",
                                "4_multivariate_normal",# "Cohens_d",
                                "4_multivariate_normal",# "Hedges_g",
                                "4_multivariate_normal",# "Cohens_d",
                                "4_multivariate_normal",# "Hedges_g",
                                # WISHART:
                                "4_multivariate_normal_wishart",# "Cohens_d",
                                "4_multivariate_normal_wishart",# "Hedges_g",
                                "4_multivariate_normal_wishart",# "Cohens_d",
                                "4_multivariate_normal_wishart",# "Hedges_g",
                                #
                                "2_multivariate_normal",# "lnRoM_paired",
                                "4_binomial",# "lnOR",
                                "2_binomial",# "lnRR",
                                #' [lnCVR is different from SMD in manuscript but Shinichi says it can be the same. ]
                                "4_multivariate_normal",# "lnCVR",
                                "4_multivariate_normal",# "lnCVR",
                                "4_multivariate_normal",# "lnCVR_paired",
                                "4_multivariate_normal",# "lnCVR_paired",
                                #  WISHART:
                                "4_multivariate_normal_wishart",# "lnCVR",
                                "4_multivariate_normal_wishart",# "lnCVR",
                                "4_multivariate_normal_wishart",# "lnCVR_paired",
                                "4_multivariate_normal_wishart",# "lnCVR_paired",
                                #
                                "3_multinomial",# "lnHWE_A"
                                "4_multivariate_normal", # lnM
                                "4_multivariate_normal" # lnM_paired
                              ),
                              vars_required = c(
                                "n, x, sd",# "reciprocal",
                                "x1, x2, sd1, sd2, n1, n2",# "lnRoM",
                                "x1, x2, sd1, sd2, n1, n2",# "lnRoM",
                                "x1, x2, sd1, sd2, n1, n2",# "Cohens_d",
                                "x1, x2, sd1, sd2, n1, n2",# "Hedges_g", 
                                "x1, x2, sd1, sd2, r, n1, n2",# "Cohens_d_paired",
                                "x1, x2, sd1, sd2, r, n1, n2",# "Hedges_g_paired", 
                                # WISHART:
                                "x1, x2, sd1, sd2, n1, n2",# "Cohens_d",
                                "x1, x2, sd1, sd2, n1, n2",# "Hedges_g", 
                                "x1, x2, sd1, sd2, r, n1, n2",# "Cohens_d_paired",
                                "x1, x2, sd1, sd2, r, n1, n2",# "Hedges_g_paired", 
                                #
                                "x1, x2, sd1, sd2, r, n1, n2",# "lnRoM_paired",
                                "a, b, c, d",# "lnOR",
                                "a, c, n1, n2",# "lnRR",
                                "x1, x2, sd1, sd2, n1, n2",# "lnCVR",
                                "x1, x2, sd1, sd2, n1, n2",# "lnCVR",
                                "x1, x2, sd1, sd2, r, n1, n2",# "lnCVR_paired",
                                "x1, x2, sd1, sd2, r, n1, n2",# "lnCVR_paired",
                                # WISHART:
                                "x1, x2, sd1, sd2, n1, n2",# "lnCVR",
                                "x1, x2, sd1, sd2, n1, n2",# "lnCVR",
                                "x1, x2, sd1, sd2, r, n1, n2",# "lnCVR_paired",
                                "x1, x2, sd1, sd2, r, n1, n2",# "lnCVR_paired",
                                #
                                "n_AA, n_Aa, n_aa",# "lnHWE_A")
                                "x1, x2, sd1, sd2, n1, n2", # lnM
                                "x1, x2, sd1, sd2, n1, n2, r" # lnM_paired
                              ),
                              cloud_filtering_rules = c(
                                NA,# "reciprocal",
                                "x1 > 0 & x2 > 0",# "lnRoM",
                                "x1 > 0 & x2 > 0",# "lnRoM",
                                
                                "v1 > 0 & v2 > 0",# "Cohens_d",
                                "v1 > 0 & v2 > 0",# "Hedges_g",
                                "v1 > 0 & v2 > 0",# "Cohens_d_paired",
                                "v1 > 0 & v2 > 0",# "Hedges_g_paired",
                                
                                # Wishart:
                                NA, #"v1 > 0 & v2 > 0",# "Cohens_d",
                                NA, #"v1 > 0 & v2 > 0",# "Hedges_g",
                                NA, #"v1 > 0 & v2 > 0",# "Cohens_d_paired",
                                NA, #"v1 > 0 & v2 > 0",# "Hedges_g_paired",
                                #
                                "x1 > 0 & x2 > 0",# "lnRoM_paired",
                                NA,# "lnOR",
                                NA,# "lnRR",
                                "x1 > 0 & x2 > 0 & v1 > 0 & v2 > 0",# "lnCVR",
                                "x1 > 0 & x2 > 0 & v1 > 0 & v2 > 0",# "lnCVR",
                                "x1 > 0 & x2 > 0 & v1 > 0 & v2 > 0",# "lnCVR_paired",
                                "x1 > 0 & x2 > 0 & v1 > 0 & v2 > 0",# "lnCVR_paired",
                                # Wishart:
                                "x1 > 0 & x2 > 0", #"v1 > 0 & v2 > 0",# "lnCVR",
                                "x1 > 0 & x2 > 0", #"v1 > 0 & v2 > 0",# "lnCVR",
                                "x1 > 0 & x2 > 0", #"v1 > 0 & v2 > 0",# "lnCVR_paired",
                                "x1 > 0 & x2 > 0", #"v1 > 0 & v2 > 0",# "lnCVR_paired",
                                #
                                NA, # "lnHWE_A"
                                "v1 > 0 & v2 > 0", # lnM
                                "v1 > 0 & v2 > 0" # lnM
                                
                              ),
                              special_warnings = c(
                                                NA, # "reciprocal",
                                                "lnRoM cannot accept x1 or x2 ≤ 0.", # "lnRoM",
                                                "lnRoM cannot accept x1 or x2 ≤ 0.", # "lnRoM",
                                                #
                                                "SMD can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "SMD", # Cohen's d
                                                "SMD can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "SMD",# Hedges' g
                                                "SMD can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "SMD_paired", # Cohen's d
                                                "SMD can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "SMD_paired", # Hedges' g
                                                # Wishart:
                                                "SMD can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "SMD", # Cohen's d
                                                "SMD can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "SMD",# Hedges' g
                                                "SMD can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "SMD_paired", # Cohen's d
                                                "SMD can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "SMD_paired", # Hedges' g
                                                #
                                                "lnRoM cannot accept x1 or x2 ≤ 0.", # "lnRoM_paired",
                                                "If any group (a, b, c, d) is 0, lnOR will be incalculable. If this is the case with your data, add 0.5 to all groups and rerun.", # "lnOR",
                                                "If any group (a, c) is 0, lnRR will be incalculable. If this is the case with your data, add 0.5 to all groups and rerun.",# "lnRR",
                                                #
                                                "lnCVR can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "lnCVR",
                                                "lnCVR can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "lnCVR",
                                                "lnCVR can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "lnCVR_paired",
                                                "lnCVR can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "lnCVR_paired",
                                                # Wishart:
                                                "lnCVR can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "lnCVR",
                                                "lnCVR can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "lnCVR",
                                                "lnCVR can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "lnCVR_paired",
                                                "lnCVR can use normal or Wishart distribution to model variances. If unspecified with 'SAFE_distribution' argument, defaulting to Wishart", # "lnCVR_paired",
                                                #
                                                "Cannot accept values of 0 for n_AA, n_Aa, or n_aa", # "lnHWE_A"
                                                "Results are only reliable with SAFE method", #'lnM [Is this true?]
                                                "Results are only reliable with SAFE method" #'lnM [Is this true?]
                                                
                              ),
                              default_safe_family = c(
                                          NA,#' "reciprocal",
                                          NA,#' "lnRoM",
                                          NA, #' "lnRoM",
                                          "no",#' "SMD", # Cohen's d
                                          "no",#' "SMD",# Hedges' g
                                          "no",#' "SMD_paired", # Cohen's d
                                          "no",#' "SMD_paired", # Hedges' g
                                          #' #' [Wishart SMD:]
                                          "yes",#' "SMD", # Cohen's d
                                          "yes",#' "SMD",# Hedges' g
                                          "yes",#' "SMD_paired", # Cohen's d
                                          "yes",#' "SMD_paired", # Hedges' g
                                          #' # 
                                         NA, #' "lnRoM_paired",
                                         NA,#' "lnOR",
                                         NA, #' "lnRR",
                                         #
                                         "no", #' "lnCVR",
                                         "no", #' "lnCVR",
                                         "no",#' "lnCVR_paired",
                                         "no",#' "lnCVR_paired",
                                          #' #' [Wishart lnCVR:]
                                          "yes",#' "lnCVR",
                                          "yes",#' "lnCVR",
                                          "yes",#' "lnCVR_paired",
                                          "yes",#' "lnCVR_paired",
                                          #' #
                                          NA,#' "lnHWE_A",
                                          NA,#' "lnM",
                                          NA#' "lnM_paired"
                                          )
                            )

effect_formulas

effect_formulas <- melt(effect_formulas,
                        id.vars = c("name", "derivative", "sim_family", "vars_required",
                                    "cloud_filtering_rules", "special_warnings",
                                    "default_safe_family"),
                        variable.name = "calc_type",
                        value.name = "formula")
effect_formulas <- effect_formulas[!is.na(formula)]

# Give a label:
unique(effect_formulas$derivative)
unique(effect_formulas$calc_type)

effect_formulas[, label := paste(ifelse(calc_type == "effect_size", "yi", "vi"), 
                                 ifelse(derivative == "first", "first", "second"), sep = "_")]
effect_formulas

effect_formulas[name == "SMD", ]
effect_formulas[name == "lnCVR", ]

# >>> Add new families of already transcribed formulas --------------------
unique(effect_formulas$sim_family)

addendum <- copy(effect_formulas[name %in% c("lnOR", "lnRR")])
#' [Can potentially treat these as same sim_family lnROM but for now going to treat separately]
addendum[name == "lnOR", sim_family := "2_multinomial_as_normal"]
addendum[name == "lnRR", sim_family := "2_multinomial_as_normal"]
addendum$default_safe_family
addendum[, default_safe_family := "no"]
effect_formulas[name %in% c("lnOR", "lnRR"), default_safe_family := "yes"]
addendum[name %in% c("lnOR", "lnRR"), cloud_filtering_rules := "p1 > 0 & p2 > 0 & p1 < 1 & p2 < 1"]

effect_formulas <- rbind(effect_formulas,
                         addendum)

# >>> Add an execution string with explicit environment/object control ----------------
i <- 1
k <- 1
effect_formulas[, exec_formula := formula]

for(i in 1:nrow(effect_formulas)){
  vars <- strsplit(unique(effect_formulas[i, ]$vars_required), split = ", ") |> unlist()
  to <- paste0("input$", vars)
  vars <- paste0("\\b", vars, "\\b")
  
  for(k in 1:length(vars)){
    effect_formulas[i, exec_formula := gsub(vars[k], to[k], exec_formula)]
  }
  
}

effect_formulas

effect_formulas[grepl("paired", name) & grepl("sqrt", exec_formula)]


# >>> Add an alternative to cloud filtering rules -------------------------

unique(effect_formulas[, .(sim_family, cloud_filtering_rules)])

effect_formulas[cloud_filtering_rules == "x1 > 0 & x2 > 0" &
                  sim_family == "2_multivariate_normal",
                `:=` (lower_filter = "x1=0, x2=0",
                      upper_filter = "x1=Inf, x2=Inf")]

effect_formulas[cloud_filtering_rules == "v1 > 0 & v2 > 0" &
                  sim_family == "4_multivariate_normal",
                `:=` (lower_filter = "x1=-Inf, x2=-Inf, v1=0, v2=0",
                      upper_filter = "x1=Inf, x2=Inf, v1=Inf, v2=Inf")]

effect_formulas[cloud_filtering_rules == "x1 > 0 & x2 > 0 & v1 > 0 & v2 > 0", ]$name
effect_formulas[cloud_filtering_rules == "x1 > 0 & x2 > 0 & v1 > 0 & v2 > 0", 
                `:=` (lower_filter = "x1=0, x2=0, v1=0, v2=0",
                      upper_filter = "x1=Inf, x2=Inf, v1=Inf, v2=Inf")]

# effect_formulas[cloud_filtering_rules %in% c("x1 > 0 & x2 > 0", 
#                                              "v1 > 0 & v2 > 0",
#                                              "x1 > 0 & x2 > 0 & v1 > 0 & v2 > 0"),
#                 `:=` (lower_filter = 0, upper_filter = Inf)]

effect_formulas[cloud_filtering_rules %in% c("p1 > 0 & p2 > 0 & p1 < 1 & p2 < 1"),
                `:=` (lower_filter = "p1=0, p2=0", 
                      upper_filter = "p1=1, p2=1")]

effect_formulas
effect_formulas[is.na(cloud_filtering_rules) &
                  !name %in% c("lnOR", "lnRR", "lnHWE_A"),]

effect_formulas[sim_family == "4_multivariate_normal_wishart" &
                  name == "lnCVR",
                `:=` (lower_filter = "x1=0, x2=0",
                      upper_filter= "x1=Inf, x2=Inf")]

effect_formulas[sim_family == "4_multivariate_normal_wishart" &
                  name == "SMD",
                `:=` (lower_filter = "x1=-Inf, x2=-Inf",
                      upper_filter= "x1=Inf, x2=Inf")]

effect_formulas
effect_formulas[name == "lnCVR"]


# >>> Save table ----------------------------------------------------------

fwrite(effect_formulas, "data/effect_size_formulas.csv", na = "NA")
fwrite(effect_formulas, "scripts/run_simulations/remote_mirrors/final_simulations/data/effect_size_formulas.csv", na = "NA")
