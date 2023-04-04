################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Figures S2-S5

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
library(tidyverse)
library(patchwork)

##### Body Mass Index ##########################################################
figS2A <- df %>% 
  fn_figS2S3S4S5(
    x = SEBMI, y = piAs, 
    xlim = c(15,40), ylim = c(0,40), 
    xlab = expression("BMI (kg/m" ^ 2 * ")"), ylab = "iAs%"
  )

figS2B <- df %>% 
  fn_figS2S3S4S5(
    x = SEBMI, y = pMMA, 
    xlim = c(15,40), ylim = c(0,30), 
    xlab = expression("BMI (kg/m" ^ 2 * ")"), ylab = "MMA%"
  )

figS2C <- df %>% 
  fn_figS2S3S4S5(
    x = SEBMI, y = pDMA, 
    xlim = c(15,40), ylim = c(50,100), 
    xlab = expression("BMI (kg/m" ^ 2 * ")"), ylab = "DMA%"
  )

figS2D <- df %>% 
  fn_figS2S3S4S5(
    x = SEBMI, y = ln_PMI, 
    xlim = c(15,40), ylim = c(-2,1), 
    xlab = expression("BMI (kg/m" ^ 2 * ")"), ylab = "ln PMI"
  )

figS2E <- df %>% 
  fn_figS2S3S4S5(
    x = SEBMI, y = ln_SMI, 
    xlim = c(15,40), ylim = c(1,4), 
    xlab = expression("BMI (kg/m" ^ 2 * ")"), ylab = "ln SMI"
  )

(figS2_bmi <- figS2A + figS2B + figS2C + figS2D + figS2E)

##### Subscapular Skinfold #####################################################
figS3A <- df %>% 
  fn_figS2S3S4S5(
    x = medSESUBSC, y = piAs, 
    xlim = c(0,40), ylim = c(0,40), 
    xlab = "Subscapular Skinfold (mm)", ylab = "iAs%"
  )

figS3B <- df %>% 
  fn_figS2S3S4S5(
    x = medSESUBSC, y = pMMA, 
    xlim = c(0,40), ylim = c(0,30), 
    xlab = "Subscapular Skinfold (mm)", ylab = "MMA%"
  )

figS3C <- df %>% 
  fn_figS2S3S4S5(
    x = medSESUBSC, y = pDMA, 
    xlim = c(0,40), ylim = c(50,100), 
    xlab = "Subscapular Skinfold (mm)", ylab = "DMA%"
  )

figS3D <- df %>% 
  fn_figS2S3S4S5(
    x = medSESUBSC, y = ln_PMI, 
    xlim = c(0,40), ylim = c(-2,1), 
    xlab = "Subscapular Skinfold (mm)", ylab = "ln PMI"
  )

figS3E <- df %>% 
  fn_figS2S3S4S5(
    x = medSESUBSC, y = ln_SMI, 
    xlim = c(0,40), ylim = c(1,4), 
    xlab = "Subscapular Skinfold (mm)", ylab = "ln SMI"
  )

(figS3_sub <- figS3A + figS3B + figS3C + figS3D + figS3E)

##### Triceps Skinfold #########################################################
figS4A <- df %>% 
  fn_figS2S3S4S5(
    x = medSETRICEP, y = piAs, 
    xlim = c(0,40), ylim = c(0,40), 
    xlab = "Triceps Skinfold (mm)", ylab = "iAs%"
  )

figS4B <- df %>% 
  fn_figS2S3S4S5(
    x = medSETRICEP, y = pMMA, 
    xlim = c(0,40), ylim = c(0,30), 
    xlab = "Triceps Skinfold (mm)", ylab = "MMA%"
  )

figS4C <- df %>% 
  fn_figS2S3S4S5(
    x = medSETRICEP, y = pDMA, 
    xlim = c(0,40), ylim = c(50,100), 
    xlab = "Triceps Skinfold (mm)", ylab = "DMA%"
  )

figS4D <- df %>% 
  fn_figS2S3S4S5(
    x = medSETRICEP, y = ln_PMI, 
    xlim = c(0,40), ylim = c(-2,1), 
    xlab = "Triceps Skinfold (mm)", ylab = "ln PMI"
  )

figS4E <- df %>% 
  fn_figS2S3S4S5(
    x = medSETRICEP, y = ln_SMI, 
    xlim = c(0,40), ylim = c(1,4), 
    xlab = "Triceps Skinfold (mm)", ylab = "ln SMI"
  )

(figS4_tri <- figS4A + figS4B + figS4C + figS4D + figS4E)

##### Mid-upper Arm Circumference (MUAC) #######################################
figS5A <- df %>% 
  fn_figS2S3S4S5(
    x = medSEMUAC, y = piAs, 
    xlim = c(10,40), ylim = c(0,40), 
    xlab = "MUAC (cm)", ylab = "iAs%"
  )

figS5B <- df %>% 
  fn_figS2S3S4S5(
    x = medSEMUAC, y = pMMA, 
    xlim = c(10,40), ylim = c(0,30), 
    xlab = "MUAC (cm)", ylab = "MMA%"
  )

figS5C <- df %>% 
  fn_figS2S3S4S5(
    x = medSEMUAC, y = pDMA, 
    xlim = c(10,40), ylim = c(50,100), 
    xlab = "MUAC (cm)", ylab = "DMA%"
  )

figS5D <- df %>% 
  fn_figS2S3S4S5(
    x = medSEMUAC, y = ln_PMI, 
    xlim = c(10,40), ylim = c(-2,1), 
    xlab = "MUAC (cm)", ylab = "ln PMI"
  )

figS5E <- df %>% 
  fn_figS2S3S4S5(
    x = medSEMUAC, y = ln_SMI, 
    xlim = c(10,40), ylim = c(1,4), 
    xlab = "MUAC (cm)", ylab = "ln SMI"
  )

(figS5_arm <- figS5A + figS5B + figS5C + figS5D + figS5E)

##### Export Figures ###########################################################
# Body Mass Index
ggsave(
  plot = figS2_bmi,
  filename = "~/Desktop/research/manuscripts/smith_etal_side_pair_bodycomp/tables_figures/pair_bodycomp_figS2.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)

# Subscapular Skinfold
ggsave(
  plot = figS3_sub,
  filename = "~/Desktop/research/manuscripts/smith_etal_side_pair_bodycomp/tables_figures/pair_bodycomp_figS3.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)

# Triceps Skinfold
ggsave(
  plot = figS4_tri,
  filename = "~/Desktop/research/manuscripts/smith_etal_side_pair_bodycomp/tables_figures/pair_bodycomp_figS4.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)

# MUAC
ggsave(
  plot = figS5_arm,
  filename = "~/Desktop/research/manuscripts/smith_etal_side_pair_bodycomp/tables_figures/pair_bodycomp_figS5.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)



