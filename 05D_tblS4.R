################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table S4

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table S2 ########################################################
tblS4 <- df_dr %>%
  pivot_wider(id_cols = c(y, x), names_from = adj, 
    values_from = c(estimate, conf.low, conf.high)) %>%
  mutate(across(-c(y, x), ~ round(.x, 2)))

tblS4 %>% head()

tblS4 <- tblS4 %>%
  arrange(x, y) %>%
  mutate(unaj = paste0( estimate_Unadjusted,  " (",  conf.low_Unadjusted,  ", ",  conf.high_Unadjusted,  ")")) %>%
  mutate(adj1 = paste0(`estimate_Adjusted 1`, " (", `conf.low_Adjusted 1`, ", ", `conf.high_Adjusted 1`, ")")) %>%
  mutate(adj2 = paste0(`estimate_Adjusted 2`, " (", `conf.low_Adjusted 2`, ", ", `conf.high_Adjusted 2`, ")")) %>%
  select(x, y, unaj, adj1, adj2)

tblS4 %>% head(n = 6)

df_dr

##### Export Table #############################################################
write_csv(
  x = tblS4,
  file = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_tblS4.csv",
  col_names = TRUE
)

