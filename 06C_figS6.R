################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Figure S6

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Figure S5: Beta Regression Models - Methylation Proportions ##############
(figS6 <- estimates_bt_per %>%
  ggplot(aes(x = adj, y = estimate, ymin = conf.low , ymax = conf.high, group = adj)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = 0.05) +
  geom_point() +
  scale_y_continuous(breaks = seq(-100,100,0.05)) +
  facet_grid(x ~ y, labeller = labeller(x = x_labs)) +
  labs(
    x = NULL,
    y = "Difference in Log Odds of Arsenic Methylation Proportion\nper IQR-unit Difference in Anthropometric Measure\n(95% Confidence Interval)") +
  th)

##### Export Figures ###########################################################
# Figure S5
ggsave(
  plot = figS6,
  filename = "~/Desktop/research/manuscripts/smith_etal_side_pair_bodycomp/tables_figures/pair_bodycomp_figS6.jpg",
  device = "jpeg",
  width = 9,
  height = 9,
  units = "in",
  dpi = 400
)
