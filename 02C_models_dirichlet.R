################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Dirichlet Regression Models

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(DirichletReg)

##### Dirichlet Regression #####################################################
# Define Multivariate Outcome
df_dr <- df

df_dr$Y <- DR_data(df_dr[, c("piAs01","pMMA01","pDMA01")], base = 1)

# Proportions by BMI
summary(model_dr_bmi_unaj <- DirichReg(Y ~ SEBMI_IQR | ln_uSum, data = df_dr, model = "alternative"))
summary(model_dr_bmi_adj1 <- DirichReg(Y ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df_dr, model = "alternative"))
summary(model_dr_bmi_adj2 <- DirichReg(Y ~ SEBMI_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df_dr, model = "alternative"))

# Proportions by Subscapular Skinfold
summary(model_dr_sub_unaj <- DirichReg(Y ~ medSESUBSC_IQR | ln_uSum, data = df_dr, model = "alternative"))
summary(model_dr_sub_adj1 <- DirichReg(Y ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df_dr, model = "alternative"))
summary(model_dr_sub_adj2 <- DirichReg(Y ~ medSESUBSC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df_dr, model = "alternative"))

# Proportions by Triceps Skinfold
summary(model_dr_tri_unaj <- DirichReg(Y ~ medSETRICEP_IQR | ln_uSum, data = df_dr, model = "alternative"))
summary(model_dr_tri_adj1 <- DirichReg(Y ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df_dr, model = "alternative"))
summary(model_dr_tri_adj2 <- DirichReg(Y ~ medSETRICEP_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df_dr, model = "alternative"))

# Proportions by MUAC
summary(model_dr_arm_unaj <- DirichReg(Y ~ medSEMUAC_IQR | ln_uSum, data = df_dr, model = "alternative"))
summary(model_dr_arm_adj1 <- DirichReg(Y ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, data = df_dr, model = "alternative"))
summary(model_dr_arm_adj2 <- DirichReg(Y ~ medSEMUAC_IQR + poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, data = df_dr, model = "alternative"))

##### Prepare Dirichlet Regression Estimates: Methylation Proportions ##########
# Compile Estimates
estimates_dr_per <- rbind(
  # Body Mass Index
  tidier_dr(model_dr_bmi_unaj, set = "Unadjusted", x = "SEBMI_IQR"),
  tidier_dr(model_dr_bmi_adj1, set = "Adjusted 1", x = "SEBMI_IQR"),
  tidier_dr(model_dr_bmi_adj2, set = "Adjusted 2", x = "SEBMI_IQR"),
  
  # Subscapular Skinfold
  tidier_dr(model_dr_sub_unaj, set = "Unadjusted", x = "medSESUBSC_IQR"),
  tidier_dr(model_dr_sub_adj1, set = "Adjusted 1", x = "medSESUBSC_IQR"),
  tidier_dr(model_dr_sub_adj2, set = "Adjusted 2", x = "medSESUBSC_IQR"),
  
  # Triceps Skinfold
  tidier_dr(model_dr_tri_unaj, set = "Unadjusted", x = "medSETRICEP_IQR"),
  tidier_dr(model_dr_tri_adj1, set = "Adjusted 1", x = "medSETRICEP_IQR"),
  tidier_dr(model_dr_tri_adj2, set = "Adjusted 2", x = "medSETRICEP_IQR"),
  
  # Mid-upper Arm Circumference
  tidier_dr(model_dr_arm_unaj, set = "Unadjusted", x = "medSEMUAC_IQR"),
  tidier_dr(model_dr_arm_adj1, set = "Adjusted 1", x = "medSEMUAC_IQR"),
  tidier_dr(model_dr_arm_adj2, set = "Adjusted 2", x = "medSEMUAC_IQR")
)

# Format Estimates
estimates_dr_per <- estimates_dr_per %>%
  mutate(set = factor(set, levels = c("Unadjusted","Adjusted 1","Adjusted 2")))

estimates_dr_per <- estimates_dr_per %>%
  mutate(term = factor(term, levels = c("SEBMI_IQR","medSESUBSC_IQR","medSETRICEP_IQR","medSEMUAC_IQR")))

estimates_dr_per <- estimates_dr_per %>%
  mutate(y = factor(y, levels = c("MMA%","DMA%")))

estimates_dr_per
