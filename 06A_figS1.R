################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Figure S1

# Tyler Smith
# April 4, 2023

##### Preliminaries ############################################################
# Load Packages
library(tidyverse)

##### Prepare Data #############################################################
# Pivot Longer and Transform
df_figS1 <- df %>%
  select(iAs = piAs, MMA = pMMA, DMA = pDMA, PMI, SMI) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "Measure", 
    values_to = "Linear"
  )

df_figS1 <- df_figS1 %>%
  mutate(Log = log(Linear)) %>%
  pivot_longer(
    cols      = -Measure, 
    names_to  = "Scale", 
    values_to = "Value"
  )

# Label Outcomes
df_figS1 <- df_figS1 %>%
  mutate(Label = paste0(Measure, "%: ", Scale)) %>%
  mutate(Label = ifelse(Measure == "PMI", paste0("PMI: ", Scale), Label)) %>%
  mutate(Label = ifelse(Measure == "SMI", paste0("SMI: ", Scale), Label))

df_figS1 %>% head(n = 10)

df_figS1 <- df_figS1 %>%
  mutate(Label = factor(Label,
    levels = c(
      "iAs%: Linear",
      "iAs%: Log",
      "MMA%: Linear",
      "MMA%: Log",
      "DMA%: Linear",
      "DMA%: Log",
      "PMI: Linear",
      "PMI: Log",
      "SMI: Linear",
      "SMI: Log"))
  )

df_figS1 <- df_figS1 %>%
  group_by(Label) %>%
  mutate(Value = scale(Value)) %>%
  ungroup()

df_figS1 %>% head()

##### Generate Figure S1 #######################################################
(figS1 <- df_figS1 %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = ceiling(sqrt(nrow(df))), fill = "white", color = "black") +
  facet_wrap(. ~ Label, scales = "free_x", ncol = 2) +
  scale_x_continuous(breaks = seq(-20,20,2)) +
  labs(
    x = "Arsenic Methylation Measure (Z-score)",
    y = "Number of Participants") +
  th +
  theme(
    axis.text.x = element_text(angle = 0, margin = margin(t = 5), hjust = 0.5, size = 12)
  ))

# Re-check for Zeroes and Ones
df_figS1 %>%
  filter(Value %in% c(0,1))

rm(df_figS1)

##### Export Figure ############################################################
ggsave(
  plot = figS1,
  filename = "~/Desktop/research/manuscripts/smith_etal_pair_anthropometry/tables_figures/pair_bodycomp_figS1.jpg",
  device = "jpeg",
  height = 10,
  width = 9,
  units = "in",
  dpi = 400
)



