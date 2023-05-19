################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table S2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table 5 #########################################################
# Combine Estimates; Pivot Wider
tblS2 <- df_lm %>%
  pivot_wider(id_cols = c(y, x), names_from = adj, values_from = c(estimate, conf.low, conf.high)) %>%
  mutate(across(-c(x, y), ~ format(round(.x, 2), nsmall = 2)))

tblS2 %>% head()

# Format Table
tblS2 <- tblS2 %>%
  arrange(y, x) %>%
  mutate(unaj = paste0( estimate_Unadjusted,  " (",  conf.low_Unadjusted,  ", ",  conf.high_Unadjusted,  ")")) %>%
  mutate(adj1 = paste0(`estimate_Adjusted 1`, " (", `conf.low_Adjusted 1`, ", ", `conf.high_Adjusted 1`, ")")) %>%
  mutate(adj2 = paste0(`estimate_Adjusted 2`, " (", `conf.low_Adjusted 2`, ", ", `conf.high_Adjusted 2`, ")")) %>%
  select(y, x, unaj, adj1, adj2)

tblS2 %>% head()

##### Export Table #############################################################
write_csv(
  x = tblS2,
  file = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_tblS2.csv",
  col_names = TRUE
)

