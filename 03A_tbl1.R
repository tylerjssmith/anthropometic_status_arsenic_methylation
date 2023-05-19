################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table 1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Overall
tbl1_overall <- rbind(
  tibble(xlab = "Overall", xval = "NA", value = "NA"),
  df %>% tbl1_cont(x = AGE, 
    xlab = "Age", group = NULL),
  df %>% tbl1_cont(x = SEGSTAGE, 
    xlab = "Gestational Age", group = NULL),
  df %>% tbl1_disc(x = PARITY,
    xlab = "Parity", group = NULL),
  df %>% tbl1_disc(x = EDUCATION, 
    xlab = "Education", group = NULL),
  df %>% tbl1_cont(x = wAs, 
    xlab = "wAs", group = NULL),
  df %>% tbl1_cont(x = uSum, 
    xlab = "∑uAs", group = NULL),
  df %>% tbl1_cont(x = SEFOL,
    xlab = "Plasma Folate", group = NULL),
  df %>% tbl1_disc(x = SEFOLCUT, 
    xlab = "Folate Deficiency", group = NULL),
  df %>% tbl1_cont(x = SEB12, 
    xlab = "Plasma Vitamin B12", group = NULL),
  df %>% tbl1_disc(x = SEB12CUT, 
    xlab = "Vitamin B12 Deficiency", group = NULL),
  df %>% tbl1_cont(x = SEHCY, 
    xlab = "Plasma Homocysteine", group = NULL),
  df %>% tbl1_disc(x = SEHCYCUT, 
    xlab = "Elevated Homocysteine", group = NULL)
)

tbl1_overall

# BMI Categories
(tbl1_bmi <- rbind(
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

(tbl1_bmi <- tbl1_bmi %>%
  pivot_wider(
    id_cols = c(xlab, xval),
    names_from = SEBMICUT,
    values_from = value
  ))

(tbl1_bmi_totals <- df %>%
  count(SEBMICUT) %>%
  mutate(pr = n / sum(n) * 100) %>%
  mutate(across(everything(), ~ round(.x, 1))) %>%
  mutate(value = paste0(n, " (", pr, ")")) %>%
  select(SEBMICUT, value) %>%
  pivot_wider(names_from = SEBMICUT, values_from = value) %>%
  tibble(xlab = "Overall", xval = "NA", .))

(tbl1_bmi <- rbind(
  tbl1_bmi_totals, 
  tbl1_bmi
))

# Join Overall and BMI Categories
tbl1 <- left_join(tbl1_overall, tbl1_bmi, by = c("xlab","xval"))

rm(list = c("tbl1_bmi","tbl1_bmi_totals","tbl1_overall"))

##### Export Table #############################################################
write_csv(
  x = tbl1, 
  file = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_tbl1.csv",
  col_names = TRUE
)





