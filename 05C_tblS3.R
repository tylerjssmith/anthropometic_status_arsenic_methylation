################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table S3

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table S1 ########################################################
# Combine Estimates; Pivot Wider
tblS3 <- df_bt %>%
  pivot_wider(id_cols = c(y, x), names_from = adj, values_from = c(estimate, conf.low, conf.high)) %>%
  mutate(across(-c(x, y), ~ round(.x, 2)))

tblS3 %>% head()

# Format Table
tblS3 <- tblS3 %>%
  arrange(y, x) %>%
  mutate(unaj = paste0( estimate_Unadjusted,  " (",  conf.low_Unadjusted,  ", ",  conf.high_Unadjusted,  ")")) %>%
  mutate(adj1 = paste0(`estimate_Adjusted 1`, " (", `conf.low_Adjusted 1`, ", ", `conf.high_Adjusted 1`, ")")) %>%
  mutate(adj2 = paste0(`estimate_Adjusted 2`, " (", `conf.low_Adjusted 2`, ", ", `conf.high_Adjusted 2`, ")")) %>%
  select(y, x, unaj, adj1, adj2)

tblS3 %>% head(n = 9)

##### Export Table #############################################################
write_csv(
  x = tblS3,
  file = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_tblS3.csv",
  col_names = TRUE
)
