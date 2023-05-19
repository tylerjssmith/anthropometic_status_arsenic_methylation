################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Figure S9

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Figure S9: Dirichlet Regression Models - Methylation Proportions #########
(figS9 <- df_dr %>%
  ggplot(aes(x = y, y = estimate, ymin = conf.low, ymax = conf.high, color = y)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0) +
  geom_point() +
  scale_y_continuous(breaks = seq(-0.5,0.5,0.05)) +
  expand_limits(y = c(-0.10,0.15)) +
  facet_grid(x ~ adj, labeller = labeller(term = x_labs)) +
  labs(
    x = NULL,
    y = "Difference in Log Odds of Arsenic Methylation Proportion\nper IQR-unit Difference in Anthropometric Measure\n(95% Confidence Interval)",
    color = "Arsenic\nMethylation\nMeasure") +
  th)

##### Export Figures ###########################################################
# Figure S9
ggsave(
  plot = figS9,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_figS9.jpg",
  device = "jpeg",
  width = 9,
  height = 9,
  units = "in",
  dpi = 400
)




