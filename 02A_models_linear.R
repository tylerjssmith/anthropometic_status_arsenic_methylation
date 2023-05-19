################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Linear Regression Models

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(broom)

##### Fit Linear Regression Models #############################################
# Set Vectors of Arsenic Methylation and Anthropometric Terms
tmp_y_lm <- c("piAs","pMMA","pDMA","ln_PMI","ln_SMI")
tmp_x_lm <- c("SEBMI_IQR","medSESUBSC_IQR","medSETRICEP_IQR","medSEMUAC_IQR",
  "SEMUAFA_IQR","SEMUAMA_IQR")

# Initialize Tibble
df_lm <- tibble()

# Fit Models
for(i in 1:length(tmp_y_lm)) {
  
  for(j in 1:length(tmp_x_lm)) {
  
    # Unadjusted
    fit_unaj <- lm(get(tmp_y_lm[i]) ~ get(tmp_x_lm[j]), data = df)
    df_lm_unaj <- tidy(fit_unaj, conf.int = TRUE)
    df_lm_unaj <- df_lm_unaj %>%
      mutate(y = tmp_y_lm[i]) %>%
      mutate(x = tmp_x_lm[j]) %>%
      mutate(adj = "Unadjusted")
    
    # Adjusted 1
    fit_adj1 <- lm(get(tmp_y_lm[i]) ~ get(tmp_x_lm[j]) + poly(ln_wAs, 2) + AGE + 
        factor(SEGSTAGE4) + factor(EDUCATION) + LSI, data = df)
    df_lm_adj1 <- tidy(fit_adj1, conf.int = TRUE)
    df_lm_adj1 <- df_lm_adj1 %>%
      mutate(y = tmp_y_lm[i]) %>%
      mutate(x = tmp_x_lm[j]) %>%
      mutate(adj = "Adjusted 1")
    
    # Adjusted 2
    fit_adj2 <- lm(get(tmp_y_lm[i]) ~ get(tmp_x_lm[j]) + poly(ln_wAs, 2) + AGE + 
        factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + 
        ln_SEHCY, data = df)
    df_lm_adj2 <- tidy(fit_adj2, conf.int = TRUE)
    df_lm_adj2 <- df_lm_adj2 %>%
      mutate(y = tmp_y_lm[i]) %>%
      mutate(x = tmp_x_lm[j]) %>%
      mutate(adj = "Adjusted 2")
    
    # Compile Estimates
    df_lm_ij <- rbind(
      df_lm_unaj,
      df_lm_adj1,
      df_lm_adj2
    )
      
    df_lm <- rbind(
      df_lm,
      df_lm_ij
    )
    
  rm(list = c("fit_unaj","fit_adj1","fit_adj2","df_lm_unaj","df_lm_adj1",
    "df_lm_adj2","df_lm_ij"))
    
  }
}

df_lm %>% head()

##### Format Estimates #########################################################
# Limit to Anthropometric Measures; Select Variables
df_lm <- df_lm %>%
  filter(term == "get(tmp_x_lm[j])") %>%
  select(y, x, adj, estimate, conf.low, conf.high, p.value)

# Transform Estimates for ln(PMI) and ln(SMI)
df_lm <- df_lm %>%
  mutate(across(c(estimate,conf.low,conf.high), 
    ~ ifelse(y %in% c("ln_PMI","ln_SMI"), (exp(.x) - 1) * 100, .x)))

df_lm %>% head()

# Label Arsenic Methylation Measures
df_lm <- df_lm %>%
  mutate(y = factor(y,
    levels = tmp_y_lm,
    labels = c("iAs%","MMA%","DMA%","ln_PMI","ln_SMI")
  ))

# Label Anthropometric Measures
df_lm <- df_lm %>%
  mutate(x = factor(x,
    levels = tmp_x_lm,
    labels = c("BMI","Subscapular","Triceps","MUAC","MUAFA","MUAMA")
  ))

# Label Adjustment Sets
df_lm <- df_lm %>%
  mutate(adj = factor(adj,
    levels = c("Unadjusted","Adjusted 1","Adjusted 2")
  ))

df_lm %>% head()

df_lm %>%
  group_by(y, x) %>%
  summarize(n = n()) %>%
  filter(n != 3)

##### Fit Linear Regression Models: MUAFA and MUAMA ############################
# Initialize Tibble
df_lm_mua <- tibble()

# Fit Models
for(i in 1:length(tmp_y_lm)) {
  fit <- lm(get(tmp_y_lm[i]) ~ SEMUAFA + SEMUAMA + poly(ln_wAs, 2) + AGE + 
      factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + 
      ln_SEHCY, data = df)
  df_lm_mua_i <- tidy(fit, conf.int = TRUE)
  df_lm_mua_i <- df_lm_mua_i %>%
    mutate(y = tmp_y_lm[i])
  
  df_lm_mua <- rbind(
    df_lm_mua,
    df_lm_mua_i
  )

  rm(list = c("fit","df_lm_mua_i"))
  
}

##### Format Estimates: MUAFA and MUAMA ########################################
# Limit to Anthropometric Measures
df_lm_mua <- df_lm_mua %>%
  filter(term %in% c("SEMUAFA","SEMUAMA")) %>%
  select(y, x = term, estimate, conf.low, conf.high, p.value)

df_lm_mua %>% head()

# Transform Estimates for ln(PMI) and ln(SMI)
df_lm_mua <- df_lm_mua %>%
  mutate(across(c(estimate,conf.low,conf.high), 
    ~ ifelse(y %in% c("ln_PMI","ln_SMI"), (exp(.x) - 1) * 100, .x)))

# Label Arsenic Methylation Measures
df_lm_mua <- df_lm_mua %>%
  mutate(y = factor(y,
    levels = tmp_y_lm,
    labels = c("iAs%","MMA%","DMA%","ln_PMI","ln_SMI")
  ))

# Label Anthropometric Measures
df_lm_mua <- df_lm_mua %>%
  mutate(x = factor(x,
    levels = c("SEMUAFA","SEMUAMA"),
    labels = c("MUAFA","MUAMA")
  ))

df_lm_mua %>% head()


