################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Figures S2-S5

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
library(tidyverse)
library(patchwork)

##### Generate Figures #########################################################
# BMI
(figS2_bmi <- df %>% 
  loess_plot(
    x = SEBMI,
    xlab = expression("BMI (kg/m" ^ 2 * ")")
  ))

# Subscapular Skinfold
(figS3_sub <- df %>% 
  loess_plot(
    x = medSESUBSC, 
    xlim = c(0,40), 
    xlab = "Subscapular Skinfold (mm)"
  ))

# Triceps Skinfold
(figS4_tri <- df %>% 
  loess_plot(
    x = medSETRICEP, 
    xlim = c(0,40), 
    xlab = "Triceps Skinfold (mm)"
  ))

# MUAC
(figS5_arm <- df %>% 
  loess_plot(
    x = medSEMUAC, 
    xlim = c(15,40), 
    xlab = "MUAC (cm)"
  ))

# MUAFA
(figS6_afa <- df %>% 
  loess_plot(
    x = SEMUAFA, 
    xlim = c(0,50), 
    xlab = expression("MUAFA (cm" ^ 2 * ")")
  ))

# MUAMA
(figS7_ama <- df %>% 
  loess_plot(
    x = SEMUAMA,
    xlim = c(0,80), 
    xlab = expression("MUAMA (cm" ^ 2 * ")")
  ))

##### Export Figures ###########################################################
# Body Mass Index
ggsave(
  plot = figS2_bmi,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_figS2.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)

# Subscapular Skinfold
ggsave(
  plot = figS3_sub,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_figS3.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)

# Triceps Skinfold
ggsave(
  plot = figS4_tri,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_figS4.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)

# MUAC
ggsave(
  plot = figS5_arm,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_figS5.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)

# MUAFA
ggsave(
  plot = figS6_afa,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_figS6.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)

# MUAMA
ggsave(
  plot = figS7_ama,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_figS7.jpg",
  device = "jpeg",
  width = 12,
  height = 8,
  units = "in",
  dpi = 400
)

