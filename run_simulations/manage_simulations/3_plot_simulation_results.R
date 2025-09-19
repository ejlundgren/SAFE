# July 17th 2025
#
#
# Plot simulation results
#
#
#
# 0. Prepare environment --------------------------------------------------

rm(list = ls())
gc()

library("data.table")
library("ggplot2")
library("patchwork")
library("scico")

# >>> Load scenarios ------------------------------------------------------

dat <- readRDS("remote_mirrors/final_simulations/summaries/all_scenarios_summarized.Rds")

dat

dat[effect_type == "SMD_normal" &
      estimator == "plugin_2nd" &
      estimate_of == "point" &
      calculation == "bias"]


dat[(estimate_of == "point" & 
               estimand == "true_2nd")]
#' [Should be 0 rows]

# >>> Plotting defaults ---------------------------------------------------
theme_SAFE <- theme_bw()+
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        strip.background = element_blank())
theme_SAFE


pal = c("plugin_1st" = "#A8DADC",
           "plugin_2nd" = "#1D3557",
           "safe" = "#E63946")

labs = c("plugin_1st" = "1st order plugin",
           "plugin_2nd" = "2nd order plugin",
           "safe" = "SAFE")

facet_labs <- as_labeller(c("plugin_1st_mc" = "Monte Carlo 1st order plugin estimand",
                            "plugin_2nd_mc" = "Monte Carlo 2nd order plugin estimand",
                            "safe_mc" = "Monte Carlo SAFE variance estimand",
                            "true_1st" = "True 1st order point estimand",
                            "true_2nd" = "True 2nd order point estimand"))


# >>> Standardize some dataset names for easier plotting ------------------
unique(dat$effect_type)
names(dat)
dat[, Sample_Size := fcase(effect_type %in% c("reciprocal"), sample_size,
                           effect_type %in% c("SMD_Wishart", "SMD_normal",
                                              "lnRoM", "lnCVR_Wishart",
                                              "lnCVR_normal"), sample_size1,
                           effect_type %in% c("lnHWE_A"), n,
                           effect_type %in% c("lnOR", "lnOR_normal",
                                              "lnRR", "lnRR_normal"), n1)]

dat[is.na(Sample_Size), ]
#' [Must be 0 rows]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------

# BOOTS == 1e+06 -------------------------------------------------------------------
unique(dat$boots)

dat1e6 <- dat[boots == 1e+06, ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 1. Reciprocal --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
sub_dat <-dat1e6[effect_type == "reciprocal", ]
sub_dat
setorder(sub_dat, Sample_Size)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
            aes(x = Sample_Size, y = value, color = estimator,
                group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("Reciprocal (1 / x)")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
# p.point

p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
             aes(x = Sample_Size, y = value, color = estimator,
                 group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs,
             nrow = 2)+
  theme_SAFE+
  theme(legend.position = "bottom")
# p.variance

test1e6 <- p.point + p.variance + plot_layout(ncol = 1, heights = c(1/3, 2/3))

ggsave("figures/initial_submission_figures/reciprocal_bias.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 2. lnRoM, equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "lnRoM", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
setorder(sub_dat, sample_size1)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias") & 
                                   sample_size_ratio == 1, ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  coord_cartesian(ylim = c(-0.004, 0.004))+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnRoM")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             nrow = 3,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnRoM_bias.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 3. SMD NORMAL for all 4, equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "SMD_normal", ]
sub_dat
setorder(sub_dat, Sample_Size)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  coord_cartesian(ylim = c(-0.07, 0.07))+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("SMD",
          subtitle = "estimated with 4-multivariate-normal distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = c(plugin_1st = "Cohen's d", 
                                plugin_2nd = "Hedges' g", 
                                safe = "SAFE"))+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = as_labeller(c("plugin_1st_mc" = "Monte Carlo Cohen's d estimand",
                                      "plugin_2nd_mc" = "Monte Carlo Hedges' g estimand",
                                      "safe_mc" = "Monte Carlo SAFE variance estimand",
                                      "true_1st" = "True Cohen's d point estimand",
                                      "true_2nd" = "True Hedges' g point estimand")))+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/SMD_normal.pdf", width = 7, height = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 4. SMD WISHART, equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "SMD_Wishart", ]
sub_dat
setorder(sub_dat, Sample_Size)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("SMD",
          subtitle = "estimated with 2-multivariate normal and 2-multivariate Wishart")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point
#

p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = c(plugin_1st = "Cohen's d", 
                                plugin_2nd = "Hedges' g", 
                                safe = "SAFE"))+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = as_labeller(c("plugin_1st_mc" = "Monte Carlo Cohen's d estimand",
                                      "plugin_2nd_mc" = "Monte Carlo Hedges' g estimand",
                                      "safe_mc" = "Monte Carlo SAFE variance estimand",
                                      "true_1st" = "True Cohen's d point estimand",
                                      "true_2nd" = "True Hedges' g point estimand")))+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/SMD_Wishart.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 5. lnCVR NORMAL equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "lnCVR_normal", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  coord_cartesian(ylim = c(-0.04, 0.04))+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnCVR",
          subtitle = "estimated with 4-multivariate-normal distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnCVR_normal.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 6. lnCVR WISHART equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "lnCVR_Wishart", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnCVR",
          subtitle = "estimated with 2-multivariate normal and 2-multivariate Wishart")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point


#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnCVR_Wishart.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 7. lnOR --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "lnOR", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnOR",
          subtitle = "estimated with binomial distribution")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnOR_binomial.pdf", width = 7, height = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 8. lnOR Normal --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "lnOR_normal", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnOR",
          subtitle = "estimated with normal distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnOR_normal.pdf", width = 7, height = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 9. lnRR --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "lnRR", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias") , ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnRR",
          subtitle = "estimated with binomial distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnRR_binomial.pdf", width = 7, height = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 10. lnRR Normal --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "lnRR_normal", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnRR",
          subtitle = "estimated with normal distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

 ggsave("figures/initial_submission_figures/lnRR_normal.pdf", width = 7, height = 10)

 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 11. HWE --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat1e6$effect_type)
sub_dat <-dat1e6[effect_type == "lnHWE_A", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

sub_dat
# range(sub_dat[true_n_AA == 2.5, ]$n)
sub_dat[calculation %in% c("bias"), ]


# SOME RANDOM Sample_Size# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = estimator,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("HWD")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = estimator,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnHWE.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# ALL BOOTS, SAFE only. SD or Bias? -------------------------------------------------------------------

dat.boots <- dat[estimator == "safe" &
                   calculation == "SD", ]

dat.boots

# Filter out the sample sizes we didn't do. 
dat.boots[, key := paste(effect_type, Sample_Size)]
keep <- unique(dat.boots[boots != 1e+06, ]$key)
keep

dat.boots <- dat.boots[key %in% keep, ]
dat.boots

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 1. Reciprocal --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
sub_dat <- dat.boots[effect_type == "reciprocal", ]
sub_dat
setorder(sub_dat, boots, Sample_Size)
sub_dat

p.point <- ggplot(data = sub_dat[estimate_of == "point"], 
                  aes(x = boots, y = value, color = Sample_Size,
                      group = Sample_Size))+
  # geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  geom_point(size = 3)+
  ylab("SD of point estimate")+
  xlab("Number of SAFE bootstraps")+
  scale_color_scico(name = "Scenario sample size", 
                     palette = "hawaii")+
  scale_x_log10(breaks = c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8))+
  ggtitle("Reciprocal",
          subtitle = "SD of point estimate estimate")+
  theme_SAFE+
  theme(legend.position = "bottom") #, legend.position.inside = c(.85, .85)
p.point

p.variance <- ggplot(data = sub_dat[estimate_of == "variance", ], 
                     aes(x = boots, y = value, color = Sample_Size,
                         group = Sample_Size))+
  # geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  geom_point(size = 3)+
  ylab("SD of variance estimate")+
  xlab("Number of SAFE bootstraps")+
  scale_color_scico(name = "Scenario sample size", 
                    palette = "hawaii")+
  scale_x_log10(breaks = c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8))+
  ggtitle(NULL, subtitle = "SD of variance estimate estimate")+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + p.variance + plot_layout(ncol = 2)

ggsave("figures/initial_submission_figures/reciprocal_SD.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 2. lnRoM, equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "lnRoM", ]
sub_dat
setorder(sub_dat, boots)

p.point <- ggplot(data = sub_dat[estimate_of == "point"], 
                  aes(x = boots, y = value, color = Sample_Size,
                      group = Sample_Size))+
  # geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  geom_point(size = 3)+
  ylab("SD of point estimate")+
  xlab("Number of SAFE bootstraps")+
  scale_color_scico(name = "Scenario sample size", 
                    palette = "hawaii")+
  scale_x_log10(breaks = c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8))+
  ggtitle("lnRoM",
          subtitle = "SD of point estimate")+
  theme_SAFE+
  theme(legend.position = "bottom") #, legend.position.inside = c(.85, .85)
p.point

p.variance <- ggplot(data = sub_dat[estimate_of == "variance", ], 
                     aes(x = boots, y = value, color = Sample_Size,
                         group = Sample_Size))+
  # geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  geom_point(size = 3)+
  ylab("SD of variance estimate")+
  xlab("Number of SAFE bootstraps")+
  scale_color_scico(name = "Scenario sample size", 
                    palette = "hawaii")+
  scale_x_log10(breaks = c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8))+
  ggtitle(NULL, 
          subtitle = "SD of variance estimate")+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + p.variance + plot_layout(ncol = 2)

ggsave("figures/initial_submission_figures/lnRoM_SD.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 3. SMD NORMAL for all 4, equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "SMD_normal", ]
sub_dat
setorder(sub_dat, Sample_Size)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = boots,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  coord_cartesian(ylim = c(-0.07, 0.07))+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("SMD",
          subtitle = "estimated with 4-multivariate-normal distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = boots,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = c(plugin_1st = "Cohen's d", 
                                plugin_2nd = "Hedges' g", 
                                safe = "SAFE"))+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = as_labeller(c("plugin_1st_mc" = "Monte Carlo Cohen's d estimand",
                                      "plugin_2nd_mc" = "Monte Carlo Hedges' g estimand",
                                      "safe_mc" = "Monte Carlo SAFE variance estimand",
                                      "true_1st" = "True Cohen's d point estimand",
                                      "true_2nd" = "True Hedges' g point estimand")))+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/SMD_normal.pdf", width = 7, height = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 4. SMD WISHART, equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "SMD_Wishart", ]
sub_dat
setorder(sub_dat, Sample_Size)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = boots,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("SMD",
          subtitle = "estimated with 2-multivariate normal and 2-multivariate Wishart")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point
#

p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = boots,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = c(plugin_1st = "Cohen's d", 
                                plugin_2nd = "Hedges' g", 
                                safe = "SAFE"))+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = as_labeller(c("plugin_1st_mc" = "Monte Carlo Cohen's d estimand",
                                      "plugin_2nd_mc" = "Monte Carlo Hedges' g estimand",
                                      "safe_mc" = "Monte Carlo SAFE variance estimand",
                                      "true_1st" = "True Cohen's d point estimand",
                                      "true_2nd" = "True Hedges' g point estimand")))+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/SMD_Wishart.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 5. lnCVR NORMAL equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "lnCVR_normal", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = boots,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  coord_cartesian(ylim = c(-0.04, 0.04))+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnCVR",
          subtitle = "estimated with 4-multivariate-normal distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = boots,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnCVR_normal.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 6. lnCVR WISHART equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "lnCVR_Wishart", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = boots,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnCVR",
          subtitle = "estimated with 2-multivariate normal and 2-multivariate Wishart")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point


#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = boots,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnCVR_Wishart.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 7. lnOR --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "lnOR", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = boots,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnOR",
          subtitle = "estimated with binomial distribution")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = boots,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnOR_binomial.pdf", width = 7, height = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 8. lnOR Normal --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "lnOR_normal", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = boots,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnOR",
          subtitle = "estimated with normal distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = boots,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnOR_normal.pdf", width = 7, height = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 9. lnRR --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "lnRR", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias") , ], 
                  aes(x = Sample_Size, y = value, color = boots,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnRR",
          subtitle = "estimated with binomial distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = boots,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnRR_binomial.pdf", width = 7, height = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 10. lnRR Normal --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "lnRR_normal", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = boots,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("lnRR",
          subtitle = "estimated with normal distributions")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = boots,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnRR_normal.pdf", width = 7, height = 10)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 11. HWE --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "lnHWE_A", ]
sub_dat
sub_dat[calculation %in% c("bias"), ]$estimand
sub_dat[calculation %in% c("bias"), ]
setorder(sub_dat, Sample_Size)

sub_dat
# range(sub_dat[true_n_AA == 2.5, ]$n)
sub_dat[calculation %in% c("bias"), ]


# SOME RANDOM Sample_Size# SOME RANDOM pr selections
p.point <- ggplot(data = sub_dat[calculation %in% c("bias"), ], 
                  aes(x = Sample_Size, y = value, color = boots,
                      group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Bias of point estimates\nMean estimate - estimand")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             labeller = facet_labs)+
  ggtitle("HWD")+
  theme_SAFE+
  theme(legend.position = "none") #, legend.position.inside = c(.85, .85)
p.point

#
p.variance <- ggplot(data = sub_dat[calculation %in% c("relative_bias"), ], 
                     aes(x = Sample_Size, y = value, color = boots,
                         group = estimator))+
  geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  ylab("Relative bias of variance estimates\n(Mean estimate - estimand) / estimand * 100")+
  xlab("Sample size")+
  scale_color_manual(name = "Estimator", 
                     values = pal,
                     labels = labs)+
  facet_wrap(~estimand, 
             scales = "free_y",
             ncol = 1,
             labeller = facet_labs)+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + xlab(NULL) + p.variance + plot_layout(ncol = 1, heights = c(1/4, 3/4))

ggsave("figures/initial_submission_figures/lnHWE.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# ALL BOOTS, SAFE only. MCSE -------------------------------------------------------------------

dat.boots <- dat[estimator == "safe" &
                   calculation == "MCSE", ]

dat.boots

# Filter out the sample sizes we didn't do. 
dat.boots[, key := paste(effect_type, Sample_Size)]
keep <- unique(dat.boots[boots != 1e+06, ]$key)
keep

dat.boots <- dat.boots[key %in% boot.table, ]
dat.boots

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 1. Reciprocal --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
sub_dat <- dat.boots[effect_type == "reciprocal", ]
sub_dat
setorder(sub_dat, boots, Sample_Size)
sub_dat

p.point <- ggplot(data = sub_dat[estimate_of == "point"], 
                  aes(x = boots, y = value, color = Sample_Size,
                      group = Sample_Size))+
  # geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  geom_point(size = 3)+
  ylab("MCSE")+
  xlab("Number of SAFE bootstraps")+
  scale_color_scico(name = "Scenario sample size", 
                    palette = "hawaii")+
  scale_x_log10(breaks = c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8))+
  ggtitle("Reciprocal",
          subtitle = "SD of point estimate estimate")+
  theme_SAFE+
  theme(legend.position = "bottom") #, legend.position.inside = c(.85, .85)
p.point

p.variance <- ggplot(data = sub_dat[estimate_of == "variance", ], 
                     aes(x = boots, y = value, color = Sample_Size,
                         group = Sample_Size))+
  # geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  geom_point(size = 3)+
  ylab("SD of variance estimate")+
  xlab("Number of SAFE bootstraps")+
  scale_color_scico(name = "Scenario sample size", 
                    palette = "hawaii")+
  scale_x_log10(breaks = c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8))+
  ggtitle(NULL, subtitle = "SD of variance estimate estimate")+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + p.variance + plot_layout(ncol = 2)

ggsave("figures/initial_submission_figures/reciprocal_SD.pdf", width = 7, height = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# >>> 2. lnRoM, equal sample sizes --------------------------------------------------------------
# object.size(scenarios[effect_type == "reciprocal", ]$file_path)
unique(dat.boots$effect_type)
sub_dat <-dat.boots[effect_type == "lnRoM", ]
sub_dat
setorder(sub_dat, boots)

p.point <- ggplot(data = sub_dat[estimate_of == "point"], 
                  aes(x = boots, y = value, color = Sample_Size,
                      group = Sample_Size))+
  # geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  geom_point(size = 3)+
  ylab("SD of point estimate")+
  xlab("Number of SAFE bootstraps")+
  scale_color_scico(name = "Scenario sample size", 
                    palette = "hawaii")+
  scale_x_log10(breaks = c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8))+
  ggtitle("lnRoM",
          subtitle = "SD of point estimate")+
  theme_SAFE+
  theme(legend.position = "bottom") #, legend.position.inside = c(.85, .85)
p.point

p.variance <- ggplot(data = sub_dat[estimate_of == "variance", ], 
                     aes(x = boots, y = value, color = Sample_Size,
                         group = Sample_Size))+
  # geom_hline(yintercept = 0)+
  geom_path(lwd = 1)+
  geom_point(size = 3)+
  ylab("SD of variance estimate")+
  xlab("Number of SAFE bootstraps")+
  scale_color_scico(name = "Scenario sample size", 
                    palette = "hawaii")+
  scale_x_log10(breaks = c(1e3, 1e4, 1e5, 1e6, 1e7, 1e8))+
  ggtitle(NULL, 
          subtitle = "SD of variance estimate")+
  theme_SAFE+
  theme(legend.position = "bottom")
p.variance

p.point + p.variance + plot_layout(ncol = 2)

ggsave("figures/initial_submission_figures/lnRoM_SD.pdf", width = 7, height = 10)
