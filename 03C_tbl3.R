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
library(corrplot)

##### Generate Table 3 #########################################################
# Extract Data
df_tbl3 <- df %>% 
  select(
    # Arsenic Methylation
    piAs,pMMA,pDMA,PMI,SMI,
    
    # Anthropometric Measures
    SEBMI,medSESUBSC,medSETRICEP,medSEMUAC,SEMUAFA,SEMUAMA,
    
    # One-Carbon Metabolism Micronutrient Status
    SEFOL,SEB12,SEHCY
  )

# Calculate Correlations
tbl3_M <- df_tbl3 %>%
  cor(., method = "spearman") %>%
  round(., 2)

tbl3_M

# Calculate p-values
tbl3_pval <- df_tbl3 %>%
  cor_pmat(method = "spearman") %>%
  round(., 4)

(tbl3_M <- ifelse(lower.tri(tbl3_M), tbl3_M, NA))
(tbl3_pval <- ifelse(lower.tri(tbl3_pval), tbl3_pval, NA))

# Generate Table 3 
tbl3 <- ifelse(tbl3_pval < 0.05 & tbl3_pval >= 0.01, paste("*",tbl3_M),
        ifelse(tbl3_pval < 0.01 & tbl3_pval >= 0.001, paste("**",tbl3_M),
        ifelse(tbl3_pval < 0.001, paste("***",tbl3_M), tbl3_M)))


colnames(tbl3) <- colnames(df_tbl3)
rownames(tbl3) <- colnames(df_tbl3)

tbl3 <- tbl3 %>% 
  as_tibble(rownames = "var")

tbl3 %>% head()

rm(list = c("df_tbl3","tbl3_M","tbl3_pval"))

##### Export Table #############################################################
write_csv(
  x = tbl3,
  file = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_tbl3.csv",
  col_names = TRUE
)
