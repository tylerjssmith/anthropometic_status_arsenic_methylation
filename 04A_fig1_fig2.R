################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Figures 1-2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Figure 1: Linear Regression - Methylation Percentages ####################
(fig1 <- df_lm %>%
  filter(!y %in% c("ln_PMI","ln_SMI")) %>%
  ggplot(aes(x = adj, y = estimate, ymin = conf.low , ymax = conf.high, group = adj)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0.05) +
  geom_point() +
  scale_y_continuous(breaks = seq(-100,100,1)) +
  facet_grid(x ~ y, labeller = labeller(x = x_labs)) +
  labs(
    x = NULL,
    y = "Mean Difference in Arsenic Methylation Percentage
    per IQR-unit Difference in Anthropometric Measure
    (95% Confidence Interval)") +
  th)

##### Figure 2: Linear Regression - Methylation Indices ########################
(fig2 <- df_lm %>%
  filter(y %in% c("ln_PMI","ln_SMI")) %>%
  ggplot(aes(x = adj, y = estimate, ymin = conf.low, ymax = conf.high, group = adj)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0.05) +
  geom_point() +
  scale_y_continuous(breaks = seq(-100,100,10)) +
  facet_grid(x ~ y, labeller = labeller(x = x_labs, y = index_labs)) +
  labs(
    x = NULL,
    y = "Mean Percent Difference in Arsenic Methylation Index
    per IQR-unit Difference in Anthropometric Measure
    (95% Confidence Interval)") +
  th)

##### Export Figures ###########################################################
# Figure 1: Methylation Percentages
ggsave(
  plot = fig1,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_fig1.jpg",
  device = "jpeg",
  width = 9,
  height = 9,
  units = "in",
  dpi = 400
)

# Figure 2: Methylation Indices
ggsave(
  plot = fig2,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_fig2.jpg",
  device = "jpeg",
  width = 9,
  height = 9,
  units = "in",
  dpi = 400
)
