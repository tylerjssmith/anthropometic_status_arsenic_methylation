################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Load and Prepare Data

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)
library(mice)
library(lubridate)

##### Read Data ################################################################
setwd("~/Johns Hopkins/PAIR Data - Documents/Data/Current/")

urine    <- read_csv("assay_urinary_metals/pair_urinaryarsenic_2022_1029.csv")
water    <- read_csv("assay_water_metals/pair_watermetals_pef_2022_1030.csv")
pefsst   <- read_csv("pefsst/pair_pefsst_2022_0310.csv")
pregtrak <- read_csv("j7pregtrak/pair_pregtrak_2022_0309.csv")
parity   <- read_csv("pair_reprohistory/pair_reprohistory_2022_0328.csv")
ses      <- read_csv("ses/pair_ses_2022_0310.csv")
ocm      <- read_csv("assay_ocm/pair_ocm_2023_0328.csv")
agp      <- read_csv("assay_agp/pair_agp_2022_1202.csv")

##### Select and Join Data #####################################################
# Urinary Arsenic
urine <- urine %>%
  select(
    UID,
    piAs = PE_uAs_iAs_SGPER,
    pMMA = PE_uAs_MMA_SGPER,
    pDMA = PE_uAs_DMA_SGPER,
    uiAs = PE_uAs_iAs_SG,
    uMMA = PE_uAs_MMA_SG,
    uDMA = PE_uAs_DMA_SG,
    uSum = PE_uAs_Sum_SG,
    uAsB = PE_uAs_Ab_SG
  )

# Drinking Water Arsenic
water <- water %>%
  select(
    UID,
    wAs = PE_wMetals_As
  )

water <- water %>%
  mutate(UID = as.numeric(UID))

# Body Mass Index
pefsst <- pefsst %>%
  select(
    UID,
    SEDATE,
    SEWKINT,
    medSEHEIGHT,
    SEWEIGHT,
    SEBMI,
    medSEMUAC,
    medSESUBSC,
    medSETRICEP
  )

# Age and Gestational Age
pregtrak <- pregtrak %>%
  select(
    UID,
    DOBYY,
    BGLMPWK
  )

# Parity
parity <- parity %>%
  select(
    UID,
    PARITY = FDPSR_PARITY
  )

# Socioeconomic Status
ses <- ses %>%
  select(
    UID, 
    EDUCATION = wehclass_mc2, 
    LSI = lsi
  )

# Micronutrients
ocm <- ocm %>%
  select(
    UID,
    SEFOL,
    SEB12,
    SEHCY,
    SEFOLCUT,
    SEB12CUT,
    SEHCYCUT
  )

ocm <- ocm %>%
  mutate(UID = as.numeric(UID))

# AGP
agp <- agp %>%
  select(
    UID,
    SEAGP
  )

# Join Data
df <- left_join(pefsst, urine, by = "UID")
df <- left_join(df, water, by = "UID")
df <- left_join(df, pregtrak, by = "UID")
df <- left_join(df, parity, by = "UID")
df <- left_join(df, ses, by = "UID")
df <- left_join(df, ocm, by = "UID")
df <- left_join(df, agp, by = "UID")

df %>% head()

# Remove Data Objects
rm(list = c("pefsst","urine","water","pregtrak","parity","ses","ocm","agp"))

##### Check Missingness ########################################################
# Check Variables
df %>% sapply(function(x) sum(is.na(x)))

100 - (df %>% na.omit() %>% nrow() / df %>% nrow() * 100)

# Check Pattern
df %>% md.pattern(rotate.names = TRUE)

# Evaluate Missingness of Subscapular Skinfold
fig_miss_subsc_bmi <- df %>%
  mutate(observed_subsc = ifelse(!is.na(medSESUBSC), "Yes", "No")) %>%
  ggplot(aes(x = SEBMI, y = pDMA, color = factor(observed_subsc))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(
    title = "Observed Subscapular Skinfold",
    x = expression("Body Mass Index (kg/m" ^ 2 * ")"),
    y = "DMA%",
    color = "Observed\nSubscapular\nSkinfold?") +
  theme_bw()

fig_miss_subsc_tri <- df %>%
  mutate(observed_subsc = ifelse(!is.na(medSESUBSC), "Yes", "No")) %>%
  ggplot(aes(x = medSETRICEP, y = pDMA, color = factor(observed_subsc))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(
    title = "Observed Subscapular Skinfold",
    x = "Triceps Skinfold",
    y = "DMA%",
    color = "Observed\nSubscapular\nSkinfold?") +
  theme_bw()

fig_miss_subsc_muac <- df %>%
  mutate(observed_subsc = ifelse(!is.na(medSESUBSC), "Yes", "No")) %>%
  ggplot(aes(x = medSEMUAC, y = pDMA, color = factor(observed_subsc))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(
    title = "Observed Subscapular Skinfold",
    x = "MUAC",
    y = "DMA%",
    color = "Observed\nSubscapular\nSkinfold?") +
  theme_bw()

# Evaluate Missingness of Drinking Water Arsenic
fig_miss_was_bmi <- df %>%
  mutate(observed_was = ifelse(!is.na(wAs), "Yes", "No")) %>%
  ggplot(aes(x = SEBMI, y = pDMA, color = factor(observed_was))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(
    title = "Observed Drinking Water Arsenic",
    x = expression("Body Mass Index (kg/m" ^ 2 * ")"),
    y = "DMA%",
    color = "Observed\nSubscapular\nSkinfold?") +
  theme_bw()

fig_miss_was_muac <- df %>%
  mutate(observed_was = ifelse(!is.na(wAs), "Yes", "No")) %>%
  ggplot(aes(x = medSEMUAC, y = pDMA, color = factor(observed_was))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(
    title = "Observed Drinking Water Arsenic",
    x = "MUAC",
    y = "DMA%",
    color = "Observed\nSubscapular\nSkinfold?") +
  theme_bw()

# Drop Incomplete Cases
df_all <- df

df <- df %>% na.omit()

##### Prepare: Confounders #####################################################
# Drinking Water Arsenic
df <- df %>%
  mutate(ln_wAs = log(wAs))

df <- df %>%
  quartile(wAs4, wAs)

# Age
df <- df %>% 
  mutate(AGE = year(SEDATE) - DOBYY)

df <- df %>%
  quartile(AGE4, AGE)

df %>%
  group_by(AGE4) %>%
  summarise(
    n = n(),
    min = min(AGE),
    max = max(AGE)
  )

# Gestational Age
df <- df %>% 
  mutate(SEGSTAGE = SEWKINT - BGLMPWK)

df <- df %>%
  mutate(
    SEGSTAGE4 =
      ifelse(SEGSTAGE < 13, 13,
      ifelse(SEGSTAGE > 16, 16, SEGSTAGE))
  )

df %>%
  group_by(SEGSTAGE4) %>%
  summarise(
    n = n(),
    min = min(SEGSTAGE),
    max = max(SEGSTAGE)
  )

# Parity
df <- df %>%
  mutate(PARITY = ifelse(PARITY > 2, 2, PARITY))

df %>%
  count(PARITY) %>%
  mutate(pr = n / sum(n) * 100)

# Education
df <- df %>%
  mutate(EDUCATION = ifelse(EDUCATION > 2, 2, EDUCATION))

df %>%
  count(EDUCATION) %>%
  mutate(pr = n / sum(n) * 100)

# Living Standards Index
df <- df %>%
  quartile(LSI4, LSI)

df %>%
  group_by(LSI4) %>%
  summarise(
    n = n(),
    min = min(LSI),
    max = max(LSI)
  )

# One-carbon Metabolism (OCM) Micronutrient Status
# (Obtain Natural-log Transformation)
df <- df %>%
  mutate(ln_SEFOL = log(SEFOL)) %>%
  mutate(ln_SEB12 = log(SEB12)) %>%
  mutate(ln_SEHCY = log(SEHCY)) %>%
  mutate(ln_SEAGP = log(SEAGP))

# (Obtain Quartiles)
df <- df %>%
  quartile(SEFOL4, SEFOL) %>%
  quartile(SEB124, SEB12) %>%
  quartile(SEHCY4, SEHCY) %>%
  quartile(SEAGP4, SEAGP)

# (Check Deficiencies)
df %>%
  count(SEFOLCUT) %>%
  mutate(pr = n / sum(n) * 100)

df %>%
  count(SEB12CUT) %>%
  mutate(pr = n / sum(n) * 100)

df %>%
  count(SEHCYCUT) %>%
  mutate(pr = n / sum(n) * 100)

##### Prepare: Anthropometric Status ###########################################
# Calculate Height Categories
df <- df %>%
  mutate(medSEHEIGHTLT145 = ifelse(medSEHEIGHT < 145, 1, 0))

# Calculate BMI Categories
df <- df %>%
  mutate(SEBMILT185 = ifelse(SEBMI < 18.5, 1, 0)) %>%
  mutate(SEBMIGT25  = ifelse(SEBMI > 25,   1, 0))

# Age Groups for Percent Body Fat
df <- df %>%
  mutate(
    AGEGROUP =
      ifelse(AGE <= 19,             0,
      ifelse(AGE  > 19 & AGE <= 29, 1,
      ifelse(AGE  > 29 & AGE <= 39, 2,
      ifelse(AGE  > 39,             3, NA))))
  )

df %>%
  group_by(AGEGROUP) %>%
  summarise(
    n = n(),
    min = min(AGE),
    max = max(AGE)
  )

# Percent Body Fat
df <- df %>%
  mutate(SESKNFOLD = log10(medSESUBSC + medSETRICEP))

df <- df %>%
  mutate(SEDENSITY =
      ifelse(AGEGROUP == 0, 1.1468 - 0.0740 * SESKNFOLD,
      ifelse(AGEGROUP == 1, 1.1582 - 0.0813 * SESKNFOLD,
      ifelse(AGEGROUP == 2, 1.1356 - 0.0680 * SESKNFOLD,
      ifelse(AGEGROUP == 3, 1.1230 - 0.0635 * SESKNFOLD, NA))))
  )
    
df <- df %>%
  mutate(SEBODYFAT = ((4.95 / (SEDENSITY)) - 4.5) * 100)

df %>%
  filter(!is.na(SEBODYFAT)) %>%
  summarise(
    n = n(),
    min = min(SEBODYFAT),
    max = max(SEBODYFAT)
  )

# Anthropometric Status (Quartiles)
df <- df %>%
  quartile(SEBMI4, SEBMI) %>%
  quartile(SEBODYFAT4, SEBODYFAT) %>%
  quartile(medSESUBSC4, medSESUBSC) %>%
  quartile(medSETRICEP4, medSETRICEP) %>%
  quartile(medSEMUAC4, medSEMUAC)

# Anthropometric Status (IQR Units)
df <- df %>%
  mutate(SEBMI_IQR       = SEBMI       / IQR(SEBMI,       na.rm = TRUE)) %>%
  mutate(SEBODYFAT_IQR   = SEBODYFAT   / IQR(SEBODYFAT,   na.rm = TRUE)) %>%
  mutate(medSEMUAC_IQR   = medSEMUAC   / IQR(medSEMUAC,   na.rm = TRUE)) %>%
  mutate(medSESUBSC_IQR  = medSESUBSC  / IQR(medSESUBSC,  na.rm = TRUE)) %>%
  mutate(medSETRICEP_IQR = medSETRICEP / IQR(medSETRICEP, na.rm = TRUE))

##### Prepare: Urinary Arsenic #################################################
# Convert Percentages to Proportions for Beta Regression
df <- df %>%
  mutate(piAs01 = piAs / 100) %>%
  mutate(pMMA01 = pMMA / 100) %>%
  mutate(pDMA01 = pDMA / 100)

# Primary and Secondary Methylation Indices
df <- df %>%
  mutate(PMI = uMMA / uiAs) %>%
  mutate(SMI = uDMA / uMMA)

df <- df %>%
  mutate(ln_PMI = log(PMI)) %>%
  mutate(ln_SMI = log(SMI))

# Urinary Arsenic (∑uAs)
df <- df %>%
  mutate(ln_uSum = log(uSum))

df <- df %>%
  quartile(uSum4, uSum)

# Urinary Arsenobetaine
df %>%
  summarise(
    median = median(uAsB),
    q1 = quantile(uAsB, 0.25),
    q3 = quantile(uAsB, 0.75)
  )

##### Finalize Analytic Data Set ###############################################
df %>% colnames()

# Select Variables
df <- df %>%
  select(UID, contains("SEBMI"), contains("medSESUBSC"), contains("medSETRICEP"), 
    contains("SEMUAC"), SEWEIGHT, contains("medSEHEIGHT"),  contains("SEBODYFAT"),
    piAs, piAs01, pMMA, pMMA01, pDMA, pDMA01, PMI, ln_PMI, SMI, ln_SMI, uSum, ln_uSum, uSum4, 
    wAs, ln_wAs, wAs4, AGE, AGE4, SEGSTAGE, SEGSTAGE4, PARITY, EDUCATION, 
    LSI, LSI4, contains("SEFOL"), contains("SEB12"), contains("SEHCY"), 
    contains("SEAGP"), uAsB)

df %>% colnames()
