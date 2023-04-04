################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Beta Regression Models

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(betareg)

##### Beta Regression ##########################################################
# Proportions by BMI
summary(model_bt_ias_bmi_unaj <- betareg(piAs01 ~ SEBMI_IQR | ln_uSum, data = df))
summary(model_bt_ias_bmi_adj1 <- betareg(piAs01 ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_ias_bmi_adj2 <- betareg(piAs01 ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

summary(model_bt_mma_bmi_unaj <- betareg(pMMA01 ~ SEBMI_IQR | ln_uSum, data = df))
summary(model_bt_mma_bmi_adj1 <- betareg(pMMA01 ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_mma_bmi_adj2 <- betareg(pMMA01 ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

summary(model_bt_dma_bmi_unaj <- betareg(pDMA01 ~ SEBMI_IQR | ln_uSum, data = df))
summary(model_bt_dma_bmi_adj1 <- betareg(pDMA01 ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_dma_bmi_adj2 <- betareg(pDMA01 ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

# Proportions by Subscapular Skinfold
summary(model_bt_ias_sub_unaj <- betareg(piAs01 ~ medSESUBSC_IQR | ln_uSum, data = df))
summary(model_bt_ias_sub_adj1 <- betareg(piAs01 ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_ias_sub_adj2 <- betareg(piAs01 ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

summary(model_bt_mma_sub_unaj <- betareg(pMMA01 ~ medSESUBSC_IQR | ln_uSum, data = df))
summary(model_bt_mma_sub_adj1 <- betareg(pMMA01 ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_mma_sub_adj2 <- betareg(pMMA01 ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

summary(model_bt_dma_sub_unaj <- betareg(pDMA01 ~ medSESUBSC_IQR | ln_uSum, data = df))
summary(model_bt_dma_sub_adj1 <- betareg(pDMA01 ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_dma_sub_adj2 <- betareg(pDMA01 ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

# Proportions by Triceps Skinfold
summary(model_bt_ias_tri_unaj <- betareg(piAs01 ~ medSETRICEP_IQR | ln_uSum, data = df))
summary(model_bt_ias_tri_adj1 <- betareg(piAs01 ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_ias_tri_adj2 <- betareg(piAs01 ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

summary(model_bt_mma_tri_unaj <- betareg(pMMA01 ~ medSETRICEP_IQR | ln_uSum, data = df))
summary(model_bt_mma_tri_adj1 <- betareg(pMMA01 ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_mma_tri_adj2 <- betareg(pMMA01 ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

summary(model_bt_dma_tri_unaj <- betareg(pDMA01 ~ medSETRICEP_IQR | ln_uSum, data = df))
summary(model_bt_dma_tri_adj1 <- betareg(pDMA01 ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_dma_tri_adj2 <- betareg(pDMA01 ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

# Proportions by MUAC
summary(model_bt_ias_arm_unaj <- betareg(piAs01 ~ medSEMUAC_IQR | ln_uSum, data = df))
summary(model_bt_ias_arm_adj1 <- betareg(piAs01 ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_ias_arm_adj2 <- betareg(piAs01 ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

summary(model_bt_mma_arm_unaj <- betareg(pMMA01 ~ medSEMUAC_IQR | ln_uSum, data = df))
summary(model_bt_mma_arm_adj1 <- betareg(pMMA01 ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_mma_arm_adj2 <- betareg(pMMA01 ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

summary(model_bt_dma_arm_unaj <- betareg(pDMA01 ~ medSEMUAC_IQR | ln_uSum, data = df))
summary(model_bt_dma_arm_adj1 <- betareg(pDMA01 ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df))
summary(model_bt_dma_arm_adj2 <- betareg(pDMA01 ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df))

##### Prepare Beta Regression Estimates: Methylation Proportions ###############
# Compile Estimates
estimates_bt_per <- rbind(
  # Body Mass Index (BMI)
  tidier(model_bt_ias_bmi_unaj, variable = "SEBMI_IQR", y = "iAs%", x = "BMI", adj = "Unadjusted"),
  tidier(model_bt_ias_bmi_adj1, variable = "SEBMI_IQR", y = "iAs%", x = "BMI", adj = "Adjusted 1"),
  tidier(model_bt_ias_bmi_adj2, variable = "SEBMI_IQR", y = "iAs%", x = "BMI", adj = "Adjusted 2"),
  
  tidier(model_bt_mma_bmi_unaj, variable = "SEBMI_IQR", y = "MMA%", x = "BMI", adj = "Unadjusted"),
  tidier(model_bt_mma_bmi_adj1, variable = "SEBMI_IQR", y = "MMA%", x = "BMI", adj = "Adjusted 1"),
  tidier(model_bt_mma_bmi_adj2, variable = "SEBMI_IQR", y = "MMA%", x = "BMI", adj = "Adjusted 2"),
  
  tidier(model_bt_dma_bmi_unaj, variable = "SEBMI_IQR", y = "DMA%", x = "BMI", adj = "Unadjusted"),
  tidier(model_bt_dma_bmi_adj1, variable = "SEBMI_IQR", y = "DMA%", x = "BMI", adj = "Adjusted 1"),
  tidier(model_bt_dma_bmi_adj2, variable = "SEBMI_IQR", y = "DMA%", x = "BMI", adj = "Adjusted 2"),
  
  # Subscapular Skinfold
  tidier(model_bt_ias_sub_unaj, variable = "medSESUBSC_IQR", y = "iAs%", x = "Subscapular Skinfold", adj = "Unadjusted"),
  tidier(model_bt_ias_sub_adj1, variable = "medSESUBSC_IQR", y = "iAs%", x = "Subscapular Skinfold", adj = "Adjusted 1"),
  tidier(model_bt_ias_sub_adj2, variable = "medSESUBSC_IQR", y = "iAs%", x = "Subscapular Skinfold", adj = "Adjusted 2"),
  
  tidier(model_bt_mma_sub_unaj, variable = "medSESUBSC_IQR", y = "MMA%", x = "Subscapular Skinfold", adj = "Unadjusted"),
  tidier(model_bt_mma_sub_adj1, variable = "medSESUBSC_IQR", y = "MMA%", x = "Subscapular Skinfold", adj = "Adjusted 1"),
  tidier(model_bt_mma_sub_adj2, variable = "medSESUBSC_IQR", y = "MMA%", x = "Subscapular Skinfold", adj = "Adjusted 2"),
  
  tidier(model_bt_dma_sub_unaj, variable = "medSESUBSC_IQR", y = "DMA%", x = "Subscapular Skinfold", adj = "Unadjusted"),
  tidier(model_bt_dma_sub_adj1, variable = "medSESUBSC_IQR", y = "DMA%", x = "Subscapular Skinfold", adj = "Adjusted 1"),
  tidier(model_bt_dma_sub_adj2, variable = "medSESUBSC_IQR", y = "DMA%", x = "Subscapular Skinfold", adj = "Adjusted 2"),
  
  # Triceps Skinfold
  tidier(model_bt_ias_tri_unaj, variable = "medSETRICEP_IQR", y = "iAs%", x = "Triceps Skinfold", adj = "Unadjusted"),
  tidier(model_bt_ias_tri_adj1, variable = "medSETRICEP_IQR", y = "iAs%", x = "Triceps Skinfold", adj = "Adjusted 1"),
  tidier(model_bt_ias_tri_adj2, variable = "medSETRICEP_IQR", y = "iAs%", x = "Triceps Skinfold", adj = "Adjusted 2"),
  
  tidier(model_bt_mma_tri_unaj, variable = "medSETRICEP_IQR", y = "MMA%", x = "Triceps Skinfold", adj = "Unadjusted"),
  tidier(model_bt_mma_tri_adj1, variable = "medSETRICEP_IQR", y = "MMA%", x = "Triceps Skinfold", adj = "Adjusted 1"),
  tidier(model_bt_mma_tri_adj2, variable = "medSETRICEP_IQR", y = "MMA%", x = "Triceps Skinfold", adj = "Adjusted 2"),
  
  tidier(model_bt_dma_tri_unaj, variable = "medSETRICEP_IQR", y = "DMA%", x = "Triceps Skinfold", adj = "Unadjusted"),
  tidier(model_bt_dma_tri_adj1, variable = "medSETRICEP_IQR", y = "DMA%", x = "Triceps Skinfold", adj = "Adjusted 1"),
  tidier(model_bt_dma_tri_adj2, variable = "medSETRICEP_IQR", y = "DMA%", x = "Triceps Skinfold", adj = "Adjusted 2"),
  
  # Mid-upper Arm Circumference (MUAC)
  tidier(model_bt_ias_arm_unaj, variable = "medSEMUAC_IQR", y = "iAs%", x = "MUAC", adj = "Unadjusted"),
  tidier(model_bt_ias_arm_adj1, variable = "medSEMUAC_IQR", y = "iAs%", x = "MUAC", adj = "Adjusted 1"),
  tidier(model_bt_ias_arm_adj2, variable = "medSEMUAC_IQR", y = "iAs%", x = "MUAC", adj = "Adjusted 2"),
  
  tidier(model_bt_mma_arm_unaj, variable = "medSEMUAC_IQR", y = "MMA%", x = "MUAC", adj = "Unadjusted"),
  tidier(model_bt_mma_arm_adj1, variable = "medSEMUAC_IQR", y = "MMA%", x = "MUAC", adj = "Adjusted 1"),
  tidier(model_bt_mma_arm_adj2, variable = "medSEMUAC_IQR", y = "MMA%", x = "MUAC", adj = "Adjusted 2"),
  
  tidier(model_bt_dma_arm_unaj, variable = "medSEMUAC_IQR", y = "DMA%", x = "MUAC", adj = "Unadjusted"),
  tidier(model_bt_dma_arm_adj1, variable = "medSEMUAC_IQR", y = "DMA%", x = "MUAC", adj = "Adjusted 1"),
  tidier(model_bt_dma_arm_adj2, variable = "medSEMUAC_IQR", y = "DMA%", x = "MUAC", adj = "Adjusted 2")
)

# Format Estimates
estimates_bt_per <- estimates_bt_per %>%
  mutate(adj = factor(adj,
    levels = c("Unadjusted","Adjusted 1","Adjusted 2")))

estimates_bt_per <- estimates_bt_per %>%
  mutate(y = factor(y,
    levels = c("iAs%","MMA%","DMA%")))

estimates_bt_per <- estimates_bt_per %>%
  mutate(x = factor(x,
    levels = c("BMI","Subscapular Skinfold","Triceps Skinfold","MUAC")))

estimates_bt_per
