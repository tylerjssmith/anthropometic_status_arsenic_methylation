################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Table S1

# Tyler Smith
# April 4, 2023

##### Compare AIC ##############################################################
# Set Vectors of Arsenic Methylation and Anthropometric Terms
tmp_y_aic <- c("piAs","pMMA","pDMA","ln_PMI","ln_SMI")
tmp_x_aic <- c("SEBMI_IQR","medSESUBSC_IQR","medSETRICEP_IQR","medSEMUAC_IQR",
  "SEMUAFA_IQR","SEMUAMA_IQR")

# Initialize Tibble
df_tblS1 <- tibble()

# Loop
for(i in 1:length(tmp_y_aic)) {
  
  for(j in 1:length(tmp_x_aic)) {
 
    out_ij <- df %>% 
      compare_aic(
        y = tmp_y_aic[i], 
        x = tmp_x_aic[j]
      )
    
    df_tblS1 <- rbind(df_tblS1, out_ij)
    
    rm(out_ij)
       
  }
}

df_tblS1 %>% head()

##### Generate Table ###########################################################
# Round
tblS1 <- df_tblS1 %>%
  mutate(aic = round(aic, 1))

# Pivot Wider
tblS1 <- tblS1 %>%
  pivot_wider(
    id_cols = c(y,x),
    names_from = degree,
    values_from = aic)

# Indicate Lower AIC
tblS1 <- tblS1 %>%
  mutate(lower_aic = ifelse(`2` < `1`, "Quadratic", "Linear"))

rm(df_tblS1)

##### Export Table #############################################################
write_csv(
  x = tblS1,
  file = "../../tables_figures/pair_bodycomp_tblS1.csv",
  col_names = TRUE
)





