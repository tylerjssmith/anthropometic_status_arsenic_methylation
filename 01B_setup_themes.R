################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Load Plot Themes and Labels

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Define Plot Theme ########################################################
th <- theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(lineheight = 1.05, size = 12),
    plot.subtitle = element_text(lineheight = 1.05, margin = margin(b = 10), size = 12),
    strip.text = element_text(size = 12),
    axis.title = element_text(margin = margin(r = 10), lineheight = 1.05, size = 12),
    axis.text.x = element_text(angle = 45, margin = margin(t = 20), hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12, lineheight = 1.05),
    legend.text = element_text(size = 12, lineheight = 1.05),
    panel.spacing = unit(5, "mm")
  )

##### Make Plot Labels #########################################################
# Label Anthropometric Status Measures
x_labs <- c("BMI","Subscapular Skinfold","Triceps Skinfold","MUAC")
names(x_labs) <- c("BMI","Subscapular Skinfold","Triceps Skinfold","MUAC")

# (Dirichlet Models)
x_labs_dr <- c("BMI","Subscapular Skinfold","Triceps Skinfold","MUAC")
names(x_labs_dr) <- c("SEBMI_IQR","medSESUBSC_IQR","medSETRICEP_IQR","medSEMUAC_IQR")

# Label Methylation Indices
index_labs <- c("Primary Methylation Index","Secondary Methylation Index")
names(index_labs) <- c("PMI","SMI")


