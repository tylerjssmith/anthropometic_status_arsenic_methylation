################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Figure S7

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Figure S6: Dirichlet Regression Models - Methylation Proportions #########
(figS7 <- estimates_dr_per %>%
  ggplot(aes(x = y, y = Estimate, ymin = lb, ymax = ub, color = y)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0) +
  geom_point() +
  scale_y_continuous(breaks = seq(-0.5,0.5,0.05)) +
  expand_limits(y = c(-0.10,0.15)) +
  labs(
    x = NULL,
    y = "Difference in Log Odds of Arsenic Methylation Proportion\nper IQR-unit Difference in Anthropometric Measure\n(95% Confidence Interval)",
    color = "Arsenic\nMethylation\nMeasure") +
  facet_grid(term ~ set, labeller = labeller(term = x_labs_dr)) +
  th)

##### Export Figures ###########################################################
# Figure S6
ggsave(
  plot = figS7,
  filename = "~/Desktop/research/manuscripts/smith_etal_side_pair_bodycomp/tables_figures/pair_bodycomp_figS7.jpg",
  device = "jpeg",
  width = 9,
  height = 9,
  units = "in",
  dpi = 400
)




