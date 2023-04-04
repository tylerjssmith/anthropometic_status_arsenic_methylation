################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Linear Regression Models

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Linear Regression: Percentages ###########################################
# Percentages by BMI
summary(model_lm_ias_bmi_unaj <- lm(piAs ~ SEBMI_IQR, data = df))
summary(model_lm_ias_bmi_adj1 <- lm(piAs ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_ias_bmi_adj2 <- lm(piAs ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_mma_bmi_unaj <- lm(pMMA ~ SEBMI_IQR, data = df))
summary(model_lm_mma_bmi_adj1 <- lm(pMMA ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_mma_bmi_adj2 <- lm(pMMA ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_dma_bmi_unaj <- lm(pDMA ~ SEBMI_IQR, data = df))
summary(model_lm_dma_bmi_adj1 <- lm(pDMA ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_dma_bmi_adj2 <- lm(pDMA ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

# Percentages by Subscapular Skinfold
summary(model_lm_ias_sub_unaj <- lm(piAs ~ medSESUBSC_IQR, data = df))
summary(model_lm_ias_sub_adj1 <- lm(piAs ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_ias_sub_adj2 <- lm(piAs ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_mma_sub_unaj <- lm(pMMA ~ medSESUBSC_IQR, data = df))
summary(model_lm_mma_sub_adj1 <- lm(pMMA ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_mma_sub_adj2 <- lm(pMMA ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_dma_sub_unaj <- lm(pDMA ~ medSESUBSC_IQR, data = df))
summary(model_lm_dma_sub_adj1 <- lm(pDMA ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_dma_sub_adj2 <- lm(pDMA ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

# Percentages by Triceps Skinfold
summary(model_lm_ias_tri_unaj <- lm(piAs ~ medSETRICEP_IQR, data = df))
summary(model_lm_ias_tri_adj1 <- lm(piAs ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_ias_tri_adj2 <- lm(piAs ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_mma_tri_unaj <- lm(pMMA ~ medSETRICEP_IQR, data = df))
summary(model_lm_mma_tri_adj1 <- lm(pMMA ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_mma_tri_adj2 <- lm(pMMA ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_dma_tri_unaj <- lm(pDMA ~ medSETRICEP_IQR, data = df))
summary(model_lm_dma_tri_adj1 <- lm(pDMA ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_dma_tri_adj2 <- lm(pDMA ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

# Percentages by MUAC
summary(model_lm_ias_arm_unaj <- lm(piAs ~ medSEMUAC_IQR, data = df))
summary(model_lm_ias_arm_adj1 <- lm(piAs ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_ias_arm_adj2 <- lm(piAs ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_mma_arm_unaj <- lm(pMMA ~ medSEMUAC_IQR, data = df))
summary(model_lm_mma_arm_adj1 <- lm(pMMA ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_mma_arm_adj2 <- lm(pMMA ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_dma_arm_unaj <- lm(pDMA ~ medSEMUAC_IQR, data = df))
summary(model_lm_dma_arm_adj1 <- lm(pDMA ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_dma_arm_adj2 <- lm(pDMA ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

##### Linear Regression: Indices ###############################################
# Methylation Indices by BMI
summary(model_lm_pmi_bmi_unaj <- lm(ln_PMI ~ SEBMI_IQR, data = df))
summary(model_lm_pmi_bmi_adj1 <- lm(ln_PMI ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_pmi_bmi_adj2 <- lm(ln_PMI ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_smi_bmi_unaj <- lm(ln_SMI ~ SEBMI_IQR, data = df))
summary(model_lm_smi_bmi_adj1 <- lm(ln_SMI ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_smi_bmi_adj2 <- lm(ln_SMI ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

# Indices by Subscapular Skinfold
summary(model_lm_pmi_sub_unaj <- lm(ln_PMI ~ medSESUBSC_IQR, data = df))
summary(model_lm_pmi_sub_adj1 <- lm(ln_PMI ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_pmi_sub_adj2 <- lm(ln_PMI ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_smi_sub_unaj <- lm(ln_SMI ~ medSESUBSC_IQR, data = df))
summary(model_lm_smi_sub_adj1 <- lm(ln_SMI ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_smi_sub_adj2 <- lm(ln_SMI ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

# Indices by Triceps Skinfold
summary(model_lm_pmi_tri_unaj <- lm(ln_PMI ~ medSETRICEP_IQR, data = df))
summary(model_lm_pmi_tri_adj1 <- lm(ln_PMI ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_pmi_tri_adj2 <- lm(ln_PMI ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_smi_tri_unaj <- lm(ln_SMI ~ medSETRICEP_IQR, data = df))
summary(model_lm_smi_tri_adj1 <- lm(ln_SMI ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_smi_tri_adj2 <- lm(ln_SMI ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

# Methylation Indices by MUAC
summary(model_lm_pmi_arm_unaj <- lm(ln_PMI ~ medSEMUAC_IQR, data = df))
summary(model_lm_pmi_arm_adj1 <- lm(ln_PMI ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_pmi_arm_adj2 <- lm(ln_PMI ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

summary(model_lm_smi_arm_unaj <- lm(ln_SMI ~ medSEMUAC_IQR, data = df))
summary(model_lm_smi_arm_adj1 <- lm(ln_SMI ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df))
summary(model_lm_smi_arm_adj2 <- lm(ln_SMI ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY, data = df))

##### Prepare Linear Regression Estimates: Methylation Percentages #############
# Compile Estimates
estimates_lm_per <- rbind(
  # Body Mass Index (BMI)
  tidier(model_lm_ias_bmi_unaj, variable = "SEBMI_IQR", y = "iAs%", x = "BMI", adj = "Unadjusted"),
  tidier(model_lm_ias_bmi_adj1, variable = "SEBMI_IQR", y = "iAs%", x = "BMI", adj = "Adjusted 1"),
  tidier(model_lm_ias_bmi_adj2, variable = "SEBMI_IQR", y = "iAs%", x = "BMI", adj = "Adjusted 2"),
  
  tidier(model_lm_mma_bmi_unaj, variable = "SEBMI_IQR", y = "MMA%", x = "BMI", adj = "Unadjusted"),
  tidier(model_lm_mma_bmi_adj1, variable = "SEBMI_IQR", y = "MMA%", x = "BMI", adj = "Adjusted 1"),
  tidier(model_lm_mma_bmi_adj2, variable = "SEBMI_IQR", y = "MMA%", x = "BMI", adj = "Adjusted 2"),
  
  tidier(model_lm_dma_bmi_unaj, variable = "SEBMI_IQR", y = "DMA%", x = "BMI", adj = "Unadjusted"),
  tidier(model_lm_dma_bmi_adj1, variable = "SEBMI_IQR", y = "DMA%", x = "BMI", adj = "Adjusted 1"),
  tidier(model_lm_dma_bmi_adj2, variable = "SEBMI_IQR", y = "DMA%", x = "BMI", adj = "Adjusted 2"),
  
  # Subscapular Skinfold
  tidier(model_lm_ias_sub_unaj, variable = "medSESUBSC_IQR", y = "iAs%", x = "Subscapular Skinfold", adj = "Unadjusted"),
  tidier(model_lm_ias_sub_adj1, variable = "medSESUBSC_IQR", y = "iAs%", x = "Subscapular Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_ias_sub_adj2, variable = "medSESUBSC_IQR", y = "iAs%", x = "Subscapular Skinfold", adj = "Adjusted 2"),
  
  tidier(model_lm_mma_sub_unaj, variable = "medSESUBSC_IQR", y = "MMA%", x = "Subscapular Skinfold", adj = "Unadjusted"),
  tidier(model_lm_mma_sub_adj1, variable = "medSESUBSC_IQR", y = "MMA%", x = "Subscapular Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_mma_sub_adj2, variable = "medSESUBSC_IQR", y = "MMA%", x = "Subscapular Skinfold", adj = "Adjusted 2"),
  
  tidier(model_lm_dma_sub_unaj, variable = "medSESUBSC_IQR", y = "DMA%", x = "Subscapular Skinfold", adj = "Unadjusted"),
  tidier(model_lm_dma_sub_adj1, variable = "medSESUBSC_IQR", y = "DMA%", x = "Subscapular Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_dma_sub_adj2, variable = "medSESUBSC_IQR", y = "DMA%", x = "Subscapular Skinfold", adj = "Adjusted 2"),
  
  # Triceps Skinfold
  tidier(model_lm_ias_tri_unaj, variable = "medSETRICEP_IQR", y = "iAs%", x = "Triceps Skinfold", adj = "Unadjusted"),
  tidier(model_lm_ias_tri_adj1, variable = "medSETRICEP_IQR", y = "iAs%", x = "Triceps Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_ias_tri_adj2, variable = "medSETRICEP_IQR", y = "iAs%", x = "Triceps Skinfold", adj = "Adjusted 2"),
  
  tidier(model_lm_mma_tri_unaj, variable = "medSETRICEP_IQR", y = "MMA%", x = "Triceps Skinfold", adj = "Unadjusted"),
  tidier(model_lm_mma_tri_adj1, variable = "medSETRICEP_IQR", y = "MMA%", x = "Triceps Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_mma_tri_adj2, variable = "medSETRICEP_IQR", y = "MMA%", x = "Triceps Skinfold", adj = "Adjusted 2"),
  
  tidier(model_lm_dma_tri_unaj, variable = "medSETRICEP_IQR", y = "DMA%", x = "Triceps Skinfold", adj = "Unadjusted"),
  tidier(model_lm_dma_tri_adj1, variable = "medSETRICEP_IQR", y = "DMA%", x = "Triceps Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_dma_tri_adj2, variable = "medSETRICEP_IQR", y = "DMA%", x = "Triceps Skinfold", adj = "Adjusted 2"),
  
  # Mid-upper Arm Circumference (MUAC)
  tidier(model_lm_ias_arm_unaj, variable = "medSEMUAC_IQR", y = "iAs%", x = "MUAC", adj = "Unadjusted"),
  tidier(model_lm_ias_arm_adj1, variable = "medSEMUAC_IQR", y = "iAs%", x = "MUAC", adj = "Adjusted 1"),
  tidier(model_lm_ias_arm_adj2, variable = "medSEMUAC_IQR", y = "iAs%", x = "MUAC", adj = "Adjusted 2"),
  
  tidier(model_lm_mma_arm_unaj, variable = "medSEMUAC_IQR", y = "MMA%", x = "MUAC", adj = "Unadjusted"),
  tidier(model_lm_mma_arm_adj1, variable = "medSEMUAC_IQR", y = "MMA%", x = "MUAC", adj = "Adjusted 1"),
  tidier(model_lm_mma_arm_adj2, variable = "medSEMUAC_IQR", y = "MMA%", x = "MUAC", adj = "Adjusted 2"),
  
  tidier(model_lm_dma_arm_unaj, variable = "medSEMUAC_IQR", y = "DMA%", x = "MUAC", adj = "Unadjusted"),
  tidier(model_lm_dma_arm_adj1, variable = "medSEMUAC_IQR", y = "DMA%", x = "MUAC", adj = "Adjusted 1"),
  tidier(model_lm_dma_arm_adj2, variable = "medSEMUAC_IQR", y = "DMA%", x = "MUAC", adj = "Adjusted 2")
)

# Format Estimates
estimates_lm_per <- estimates_lm_per %>%
  mutate(adj = factor(adj,
    levels = c("Unadjusted","Adjusted 1","Adjusted 2")))

estimates_lm_per <- estimates_lm_per %>%
  mutate(y = factor(y,
    levels = c("iAs%","MMA%","DMA%")))

estimates_lm_per <- estimates_lm_per %>%
  mutate(x = factor(x,
    levels = c("BMI","Subscapular Skinfold","Triceps Skinfold","MUAC")))

estimates_lm_per %>% head()

##### Prepare Linear Regression Estimates: Methylation Indices #################
# Compile Estimates
estimates_lm_indices <- rbind(
  # Body Mass Index (BMI)
  tidier(model_lm_pmi_bmi_unaj, variable = "SEBMI_IQR", y = "PMI", x = "BMI", adj = "Unadjusted"),
  tidier(model_lm_pmi_bmi_adj1, variable = "SEBMI_IQR", y = "PMI", x = "BMI", adj = "Adjusted 1"),
  tidier(model_lm_pmi_bmi_adj2, variable = "SEBMI_IQR", y = "PMI", x = "BMI", adj = "Adjusted 2"),
  
  tidier(model_lm_smi_bmi_unaj, variable = "SEBMI_IQR", y = "SMI", x = "BMI", adj = "Unadjusted"),
  tidier(model_lm_smi_bmi_adj1, variable = "SEBMI_IQR", y = "SMI", x = "BMI", adj = "Adjusted 1"),
  tidier(model_lm_smi_bmi_adj2, variable = "SEBMI_IQR", y = "SMI", x = "BMI", adj = "Adjusted 2"),
  
  # Subscapular Skinfold
  tidier(model_lm_pmi_sub_unaj, variable = "medSESUBSC_IQR", y = "PMI", x = "Subscapular Skinfold", adj = "Unadjusted"),
  tidier(model_lm_pmi_sub_adj1, variable = "medSESUBSC_IQR", y = "PMI", x = "Subscapular Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_pmi_sub_adj2, variable = "medSESUBSC_IQR", y = "PMI", x = "Subscapular Skinfold", adj = "Adjusted 2"),
  
  tidier(model_lm_smi_sub_unaj, variable = "medSESUBSC_IQR", y = "SMI", x = "Subscapular Skinfold", adj = "Unadjusted"),
  tidier(model_lm_smi_sub_adj1, variable = "medSESUBSC_IQR", y = "SMI", x = "Subscapular Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_smi_sub_adj2, variable = "medSESUBSC_IQR", y = "SMI", x = "Subscapular Skinfold", adj = "Adjusted 2"),
  
  # Triceps Skinfold
  tidier(model_lm_pmi_tri_unaj, variable = "medSETRICEP_IQR", y = "PMI", x = "Triceps Skinfold", adj = "Unadjusted"),
  tidier(model_lm_pmi_tri_adj1, variable = "medSETRICEP_IQR", y = "PMI", x = "Triceps Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_pmi_tri_adj2, variable = "medSETRICEP_IQR", y = "PMI", x = "Triceps Skinfold", adj = "Adjusted 2"),
  
  tidier(model_lm_smi_tri_unaj, variable = "medSETRICEP_IQR", y = "SMI", x = "Triceps Skinfold", adj = "Unadjusted"),
  tidier(model_lm_smi_tri_adj1, variable = "medSETRICEP_IQR", y = "SMI", x = "Triceps Skinfold", adj = "Adjusted 1"),
  tidier(model_lm_smi_tri_adj2, variable = "medSETRICEP_IQR", y = "SMI", x = "Triceps Skinfold", adj = "Adjusted 2"),
  
  # Mid-upper Arm Circumference (MUAC)
  tidier(model_lm_pmi_arm_unaj, variable = "medSEMUAC_IQR", y = "PMI", x = "MUAC", adj = "Unadjusted"),
  tidier(model_lm_pmi_arm_adj1, variable = "medSEMUAC_IQR", y = "PMI", x = "MUAC", adj = "Adjusted 1"),
  tidier(model_lm_pmi_arm_adj2, variable = "medSEMUAC_IQR", y = "PMI", x = "MUAC", adj = "Adjusted 2"),
  
  tidier(model_lm_smi_arm_unaj, variable = "medSEMUAC_IQR", y = "SMI", x = "MUAC", adj = "Unadjusted"),
  tidier(model_lm_smi_arm_adj1, variable = "medSEMUAC_IQR", y = "SMI", x = "MUAC", adj = "Adjusted 1"),
  tidier(model_lm_smi_arm_adj2, variable = "medSEMUAC_IQR", y = "SMI", x = "MUAC", adj = "Adjusted 2")
)

# Format Estimates
estimates_lm_indices <- estimates_lm_indices %>%
  mutate(across(c(estimate,conf.low,conf.high), ~ (exp(.x) - 1) * 100))

estimates_lm_indices <- estimates_lm_indices %>%
  mutate(adj = factor(adj,
    levels = c("Unadjusted","Adjusted 1","Adjusted 2")))

estimates_lm_indices <- estimates_lm_indices %>%
  mutate(y = factor(y,
    levels = c("PMI","SMI")))

estimates_lm_indices <- estimates_lm_indices %>%
  mutate(x = factor(x,
    levels = c("BMI","Subscapular Skinfold","Triceps Skinfold","MUAC")))

estimates_lm_indices %>% head()
