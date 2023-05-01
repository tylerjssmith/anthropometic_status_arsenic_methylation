################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table 1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
(tbl1 <- rbind(
  df %>% tbl1_cont(x = AGE, 
    xlab = "Age"),
  df %>% tbl1_cont(x = SEGSTAGE, 
    xlab = "Gestational Age"),
  df %>% tbl1_disc(x = PARITY,
    xlab = "Parity"),
  df %>% tbl1_disc(x = EDUCATION, 
    xlab = "Education"),
  df %>% tbl1_cont(x = wAs, 
    xlab = "wAs"),
  df %>% tbl1_cont(x = uSum, 
    xlab = "∑uAs"),
  df %>% tbl1_disc(x = SEBMILT185,
    xlab = "BMI <18.5"),
  df %>% tbl1_disc(x = SEBMIGT25,
    xlab = "BMI > 25"),
  df %>% tbl1_disc(x = medSEHEIGHTLT145,
    xlab = "Height <145 cm"),
  df %>% tbl1_cont(x = SEFOL,
    xlab = "Plasma Folate"),
  df %>% tbl1_disc(x = SEFOLCUT, 
    xlab = "Folate Deficiency"),
  df %>% tbl1_cont(x = SEB12, 
    xlab = "Plasma Vitamin B12"),
  df %>% tbl1_disc(x = SEB12CUT, 
    xlab = "Vitamin B12 Deficiency"),
  df %>% tbl1_cont(x = SEHCY, 
    xlab = "Plasma Homocysteine"),
  df %>% tbl1_disc(x = SEHCYCUT, 
    xlab = "Elevated Homocysteine")
))

##### Export Table #############################################################
write_csv(
  x = tbl1, 
  file = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_tbl1.csv",
  col_names = TRUE
)





