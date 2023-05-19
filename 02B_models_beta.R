################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Beta Regression Models

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(betareg)

##### Fit Beta Regression Models ###############################################
# Set Vectors of Arsenic Methylation and Anthropometric Terms
tmp_y_bt <- c("piAs01","pMMA01","pDMA01")
tmp_x_bt <- c("SEBMI_IQR","medSESUBSC_IQR","medSETRICEP_IQR","medSEMUAC_IQR",
  "SEMUAFA_IQR","SEMUAMA_IQR")

# Initialize Tibble
df_bt <- tibble()

# Fit Models
for(i in 1:length(tmp_y_bt)) {
  
  for(j in 1:length(tmp_x_bt)) {
  
    # Unadjusted
    fit_unaj <- betareg(get(tmp_y_bt[i]) ~ get(tmp_x_bt[j]) | ln_uSum, 
      data = df)
    df_bt_unaj <- tidy(fit_unaj, conf.int = TRUE)
    df_bt_unaj <- df_bt_unaj %>%
      mutate(y = tmp_y_bt[i]) %>%
      mutate(x = tmp_x_bt[j]) %>%
      mutate(adj = "Unadjusted")
    
    # Adjusted 1
    fit_adj1 <- betareg(get(tmp_y_bt[i]) ~ get(tmp_x_bt[j]) + poly(ln_wAs, 2) + 
        AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI | ln_uSum, 
      data = df)
    df_bt_adj1 <- tidy(fit_adj1, conf.int = TRUE)
    df_bt_adj1 <- df_bt_adj1 %>%
      mutate(y = tmp_y_bt[i]) %>%
      mutate(x = tmp_x_bt[j]) %>%
      mutate(adj = "Adjusted 1")
    
    # Adjusted 2
    fit_adj2 <- betareg(get(tmp_y_bt[i]) ~ get(tmp_x_bt[j]) + poly(ln_wAs, 2) + 
        AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + ln_SEFOL + 
        ln_SEB12 + ln_SEHCY | ln_uSum, 
      data = df)
    df_bt_adj2 <- tidy(fit_adj2, conf.int = TRUE)
    df_bt_adj2 <- df_bt_adj2 %>%
      mutate(y = tmp_y_bt[i]) %>%
      mutate(x = tmp_x_bt[j]) %>%
      mutate(adj = "Adjusted 2")
    
    # Compile Estimates
    df_bt_ij <- rbind(
      df_bt_unaj,
      df_bt_adj1,
      df_bt_adj2
    )
      
    df_bt <- rbind(
      df_bt,
      df_bt_ij
    )
    
  rm(list = c("fit_unaj","fit_adj1","fit_adj2","df_bt_unaj","df_bt_adj1",
    "df_bt_adj2","df_bt_ij"))
    
  }
}

df_bt %>% head()

##### Format Estimates #########################################################
df_bt <- df_bt %>%
  mutate(adj = factor(adj,
    levels = c("Unadjusted","Adjusted 1","Adjusted 2")))

df_bt <- df_bt %>%
  mutate(y = factor(y,
    levels = tmp_y_bt,
    labels = c("iAs%","MMA%","DMA%")
  ))

df_bt <- df_bt %>%
  mutate(x = factor(x,
    levels = tmp_x_bt,
    labels = c("BMI","Subscapular","Triceps","MUAC","MUAFA","MUAMA")
  ))

df_bt <- df_bt %>%
  filter(component == "mean") %>%
  filter(term == "get(tmp_x_bt[j])") %>%
  select(y, x, adj, estimate, conf.low, conf.high, p.value)

df_bt %>% head()

#### Fit Beta Regression Models: MUAFA and MUAMA ###############################
# Initialize Tibble
df_bt_mua <- tibble()

# Fit Models
for(i in 1:length(tmp_y_bt)) {

    # (Adjusted 2 Only)
    fit <- betareg(get(tmp_y_bt[i]) ~ SEMUAFA_IQR + SEMUAMA_IQR + 
        poly(ln_wAs, 2) + AGE + factor(SEGSTAGE4) + factor(EDUCATION) + LSI + 
        ln_SEFOL + ln_SEB12 + ln_SEHCY | ln_uSum, 
      data = df)
    df_bt_mua_i <- tidy(fit, conf.int = TRUE)
    df_bt_mua_i <- df_bt_mua_i %>%
      mutate(y = tmp_y_bt[i]) %>%
      mutate(adj = "Adjusted 2")
    
    # Compile Estimates
    df_bt_mua <- rbind(df_bt_mua, df_bt_mua_i)
    
    rm(list = c("fit","df_bt_mua_i"))
    
}

df_bt_mua %>% head()

##### Format Estimates: MUAFA and MUAMA ########################################
df_bt_mua <- df_bt_mua %>%
  filter(component == "mean") %>%
  filter(term %in% c("SEMUAFA_IQR","SEMUAMA_IQR"))

df_bt_mua <- df_bt_mua %>%
  mutate(y = factor(y,
    levels = tmp_y_bt,
    labels = c("iAs%","MMA%","DMA%")
  ))

df_bt_mua <- df_bt_mua %>%
  mutate(x = factor(term,
    levels = c("SEMUAFA_IQR","SEMUAMA_IQR"),
    labels = c("MUAFA","MUAMA")
  ))

df_bt_mua <- df_bt_mua %>%
  select(y, x, adj, estimate, conf.low, conf.high, p.value)

df_bt_mua %>% head()


