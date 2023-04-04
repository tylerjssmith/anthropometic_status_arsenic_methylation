################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table 2

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Generate Table ###########################################################
# Select Variables
tbl2 <- df %>%
  select(
    # Adiposity
    SEBMI, medSESUBSC, medSETRICEP, medSEMUAC, 
    # Arsenic Methylation
    piAs, pMMA, pDMA, PMI, SMI
  )

tbl2 %>% head()

# Summarize Measures
tbl2 <- tbl2 %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(
    mean = mean(value),
    sd   = sd(value),
    med  = median(value),
    q1   = quantile(value, 0.25),
    q3   = quantile(value, 0.75),
    min  = min(value),
    max  = max(value)
  )

tbl2 %>% head(n = 9)

##### Format Table #############################################################
# Order and Label Rows
tbl2 <- tbl2 %>%
  mutate(name = factor(name,
    levels = c("SEBMI","medSESUBSC","medSETRICEP","medSEMUAC","piAs","pMMA","pDMA","PMI","SMI"),
    labels = c("Body Mass Index (kg/m2)","Subscapular Skinfold (mm)","Triceps Skinfold (mm)","Mid-upper Arm Circumference (cm)","iAs%","MMA%","DMA%","Primary Methylation Index","Secondary Methylation Index"))) %>%
  arrange(name)

tbl2 %>% head(n = 9)

# Round Digits; Combine Mean and SD
tbl2 <- tbl2 %>%
  mutate(across(-name, ~ ifelse(name == "Primary Methylation Index",
    format(round(.x, 2), nsmall = 2), format(round(.x, 1), nsmall = 1)))) %>%
  mutate(mean_sd = paste0(mean, " (", sd, ")")) %>%
  select(name, mean_sd, min, q1, med, q3, max)

tbl2 %>% head(n = 9)

##### Export Table #############################################################
write_csv(
  x = tbl2,
  file = "~/Desktop/research/manuscripts/smith_etal_side_pair_bodycomp/tables_figures/pair_bodycomp_tbl2.csv",
  col_names = TRUE
)



