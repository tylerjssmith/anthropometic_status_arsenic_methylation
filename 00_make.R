################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Make File

# Tyler Smith
# April 4, 2023

##### 01: Setup ################################################################
# Set Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/code/anthropometric_status_arsenic_methylation/")

# Load Functions
source("01A_setup_functions.R")

# Load Themes
source("01B_setup_themes.R")

# Prepare Data
source("01C_setup_data.R")

##### 02: Models ###############################################################
# Set Working Directory
setwd("~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/code/anthropometric_status_arsenic_methylation/")

# Fit Linear Regression Models
source("02A_models_linear.R")

# Fit Beta Regression Models
source("02B_models_beta.R")

# Fit Dirichlet Regression Models
source("02C_models_dirichlet.R")

##### 03: Tables ###############################################################
# Make Table 1
source("03A_tbl1.R")

# Make Table 2
source("03B_tbl2.R")

# Make Table 3
source("03C_tbl3.R")

##### 04: Figures ##############################################################
# Make Figures 1-2 (Linear)
source("04A_fig1_fig2.R")

##### 05: Supplemental Tables ##################################################
# Make Table S1 (Linear)
source("05A_tblS1.R")

# Make Table S2 (Beta)
source("05B_tblS2.R")

# Make Table S3 (Dirichlet)
source("05C_tblS3.R")

##### 06: Supplemental Figures #################################################
# Make Figure S1 (Histograms)
source("06A_figS1.R")

# Make Figure S2-S5 (LOESS)
source("06B_figS2_figS3_figS4_figS5.R")

# Make Figure S6 (Beta)
source("06C_figS6.R")

# Make Figure S7 (Dirichlet)
source("06D_figS7.R")

