################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Tables S2A, S3A, S4A

# Tyler Smith
# May 19, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Tables ##########################################################
# Table S2A: Linear Regression
df_lm_mua %>% head()

tblS2A <- df_lm_mua %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ format(round(.x, 2), nsmall = 2))) %>%
  mutate(value = paste0(estimate, " (", conf.low, ", ", conf.high, ")")) %>%
  select(y, x, value)

tblS2A %>% head()

# Table S3A: Beta Regression
df_bt_mua %>% head()

tblS3A <- df_bt_mua %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ format(round(.x, 2), nsmall = 2))) %>%
  mutate(value = paste0(estimate, " (", conf.low, ", ", conf.high, ")")) %>%
  select(y, x, value)

tblS3A %>% head()

# Table S4: Dirichlet Regression
df_dr_mua %>% head()

tblS4A <- df_dr_mua %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ format(round(.x, 2), nsmall = 2))) %>%
  mutate(value = paste0(estimate, " (", conf.low, ", ", conf.high, ")")) %>%
  select(y, x, value)

tblS4A %>% head()

##### Export Tables ############################################################
# Table S2A
write_csv(
  x = tblS2A,
  file = "../../tables_figures/pair_bodycomp_tblS2a.csv",
  col_names = TRUE
)

# Table S3A
write_csv(
  x = tblS3A,
  file = "../../tables_figures/pair_bodycomp_tblS3a.csv",
  col_names = TRUE
)

# Table S4A
write_csv(
  x = tblS4A,
  file = "../../tables_figures/pair_bodycomp_tblS4a.csv",
  col_names = TRUE
)




