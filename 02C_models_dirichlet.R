################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Dirichlet Regression Models

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(DirichletReg)

##### Fit Dirichlet Regression Models ##########################################
# Define Multivariate Outcome
df_mvY <- df

df_mvY$Y <- DR_data(df_mvY[, c("piAs01","pMMA01","pDMA01")], base = 1)

# Set Vectors of Anthropometric Terms
tmp_x_dr <- c("SEBMI_IQR","medSESUBSC_IQR","medSETRICEP_IQR","medSEMUAC_IQR",
  "SEMUAFA_IQR","SEMUAMA_IQR")

# Initialize Tibble
df_dr <- tibble()

# Fit Models
for(j in 1:length(tmp_x_dr)) {
  
  # Unadjusted
  fit_unaj <- DirichReg(Y ~ get(tmp_x_dr[j]) | ln_uSum, 
    data = df_mvY, model = "alternative")
  
  df_dr_unaj <- tidy_dr(fit_unaj, 
    x = tmp_x_dr[j], 
    adj = "Unadjusted"
  )
    
  # Adjusted 1
  fit_adj1 <- DirichReg(Y ~ get(tmp_x_dr[j]) + poly(ln_wAs, 2) + 
      AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, 
    data = df_mvY, model = "alternative")
  
  df_dr_adj1 <- tidy_dr(fit_adj1, 
    x = tmp_x_dr[j], 
    adj = "Adjusted 1"
  )
    
  # Adjusted 2
  fit_adj2 <- DirichReg(Y ~ get(tmp_x_dr[j]) + poly(ln_wAs, 2) + 
      AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + 
      ln_SEB12 + ln_SEHCY | ln_uSum, 
    data = df_mvY, model = "alternative")
  
  df_dr_adj2 <- tidy_dr(fit_adj2, 
    x = tmp_x_dr[j], 
    adj = "Adjusted 2"
  )
  
  # Compile Estimates
  df_dr_j <- rbind(
    df_dr_unaj,
    df_dr_adj1,
    df_dr_adj2
  )
  
  df_dr <- rbind(df_dr, df_dr_j)

  rm(list = c("fit_unaj","fit_adj1","fit_adj2","df_dr_unaj","df_dr_adj1",
    "df_dr_adj2","df_dr_j"))
  
}

df_dr %>% head()

##### Format Estimates #########################################################
df_dr <- df_dr %>%
  mutate(adj = factor(adj,
    levels = c("Unadjusted","Adjusted 1","Adjusted 2")))

df_dr <- df_dr %>%
  mutate(y = factor(y,
    levels = c("MMA%","DMA%")
  ))

df_dr <- df_dr %>%
  mutate(x = factor(x,
    levels = tmp_x_bt,
    labels = c("BMI","Subscapular","Triceps","MUAC","MUAFA","MUAMA")
  ))

df_dr <- df_dr %>%
  select(y, x, adj, estimate, conf.low, conf.high, p.value)

df_dr %>% head()

##### Fit Dirichlet Regression Models: MUAFA and MUAMA #########################
fit_dr_mua <- DirichReg(Y ~ SEMUAFA_IQR + SEMUAMA_IQR + poly(ln_wAs, 2) + AGE + 
    factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, 
  data = df_mvY, model = "alternative")

##### Format Estimates: MUAFA and MUAMA ########################################
df_dr_mua <- unclass(summary(fit_dr_mua))$coef.mat %>%
    as_tibble(rownames = "term") %>%
    filter(term %in% c("SEMUAFA_IQR","SEMUAMA_IQR")) %>%
    mutate(y = rep(c("MMA%","DMA%"), 2)) %>%
    mutate(x = term) %>%
    mutate(adj = "Adjusted 2") %>%
    mutate(conf.low = Estimate - 1.96 * `Std. Error`) %>%
    mutate(conf.high = Estimate + 1.96 * `Std. Error`) %>%
    select(y, x, adj, estimate = Estimate, conf.low, conf.high, 
      p.value = `Pr(>|z|)`)

rm(fit_dr_mua)
