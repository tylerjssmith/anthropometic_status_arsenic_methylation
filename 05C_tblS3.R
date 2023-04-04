################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table S3

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table S2 ########################################################
tblS3 <- estimates_dr_per %>%
  pivot_wider(id_cols = c(y, term), names_from = set, values_from = c(Estimate, lb, ub)) %>%
  mutate(across(-c(y, term), ~ round(.x, 2)))

tblS3 %>% head()

tblS3 <- tblS3 %>%
  arrange(term, y) %>%
  mutate(unaj = paste0( Estimate_Unadjusted,  " (",  lb_Unadjusted,  ", ",  ub_Unadjusted,  ")")) %>%
  mutate(adj1 = paste0(`Estimate_Adjusted 1`, " (", `lb_Adjusted 1`, ", ", `ub_Adjusted 1`, ")")) %>%
  mutate(adj2 = paste0(`Estimate_Adjusted 2`, " (", `lb_Adjusted 2`, ", ", `ub_Adjusted 2`, ")")) %>%
  select(term, y, unaj, adj1, adj2)

tblS3 %>% head(n = 6)

##### Export Table #############################################################
write_csv(
  x = tblS3,
  file = "~/Desktop/research/manuscripts/smith_etal_side_pair_bodycomp/tables_figures/pair_bodycomp_tblS3.csv",
  col_names = TRUE
)

