################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table 3

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(ggcorrplot)
library(correlation)

##### Generate Table 4 #########################################################
# Generate Table
(tbl3 <- df %>% 
  select(piAs, pMMA, pDMA, PMI, SMI, SEBMI, medSESUBSC, medSETRICEP, medSEMUAC, SEFOL, SEB12, SEHCY) %>% 
  cor(., method = "spearman") %>%
  round(., digits = 2) %>%
  as_tibble(rownames = "Variable"))

# Calculate p Values
tbl3_pval <- df %>% 
  select(piAs, pMMA, pDMA, PMI, SMI, SEBMI, medSESUBSC, medSETRICEP, medSEMUAC, SEFOL, SEB12, SEHCY) %>% 
  cor_pmat(method = "spearman")

# Assess p<0.05
(tbl3_pval < 0.05)

##### Export Table #############################################################
write_csv(
  x = tbl3,
  file = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_tbl3.csv",
  col_names = TRUE
)
