################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table S1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table 5 #########################################################
# Combine Estimates; Pivot Wider
tblS1_per <- estimates_lm_per %>%
  pivot_wider(id_cols = c(y, x), names_from = adj, values_from = c(estimate, conf.low, conf.high)) %>%
  mutate(across(-c(x, y), ~ round(.x, 2)))

tblS1_ind <- estimates_lm_indices %>%
  pivot_wider(id_cols = c(y, x), names_from = adj, values_from = c(estimate, conf.low, conf.high)) %>%
  mutate(across(-c(x, y), ~ round(.x, 2)))

tblS1 <- rbind(tblS1_per, tblS1_ind)

tblS1 %>% head()

# Format Table
tblS1 <- tblS1 %>%
  arrange(y, x) %>%
  mutate(unaj = paste0( estimate_Unadjusted,  " (",  conf.low_Unadjusted,  ", ",  conf.high_Unadjusted,  ")")) %>%
  mutate(adj1 = paste0(`estimate_Adjusted 1`, " (", `conf.low_Adjusted 1`, ", ", `conf.high_Adjusted 1`, ")")) %>%
  mutate(adj2 = paste0(`estimate_Adjusted 2`, " (", `conf.low_Adjusted 2`, ", ", `conf.high_Adjusted 2`, ")")) %>%
  select(y, x, unaj, adj1, adj2)

tblS1 %>% head()

##### Export Table #############################################################
write_csv(
  x = tblS1,
  file = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_tblS1.csv",
  col_names = TRUE
)

