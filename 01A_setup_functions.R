################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Load Functions

# Tyler Smith
# April 4, 2023

##### Functions: Models ########################################################
# Function: Compare AIC
compare_aic <- function(data, y, x, degree = 1:2) {
  require(splines)
  
  # Fit Models; Get AIC
  aic <- degree %>%
    map(~ lm(get(y) ~ bs(get(x), degree = .x) + AGE + 
        factor(SEGSTAGE) + factor(EDUCATION) + LSI + ln_SEFOL + ln_SEB12 + 
        ln_SEHCY, 
      data = data)) %>%
    map(AIC) %>%
    unlist()
  
  # Prepare Results
  out <- tibble(
    y = y,
    x = x,
    degree,
    aic
  )
  
  return(out)
}

# Function: Tidy Dirichlet Model Estimates
tidy_dr <- function(model, adj, x) 
{
  out <- unclass(summary(model))$coef.mat %>%
    as_tibble(rownames = "term") %>%
    filter(term == "get(tmp_x_dr[j])") %>%
    mutate(y = c("MMA%","DMA%")) %>%
    mutate(x = x) %>%
    mutate(adj = adj) %>%
    mutate(conf.low = Estimate - 1.96 * `Std. Error`) %>%
    mutate(conf.high = Estimate + 1.96 * `Std. Error`) %>%
    select(y, x, adj, estimate = Estimate, conf.low, conf.high, 
      p.value = `Pr(>|z|)`)
  return(out)
}

##### Tables ###################################################################
# Function: Table 1, Continuous Variables
tbl1_cont <- function(data, x, group = SEBMICUT, xlab = "Variable") 
{
  # Calculate Summary Statistics
  out_raw <- data %>%
    group_by({{ group }}) %>%
    summarise(
      mean = mean({{ x }}),
      q1 = quantile({{ x }}, 0.25),
      q3 = quantile({{ x }}, 0.75)
    )
  
  # Format Statistics
  out <- out_raw %>%
    mutate(across(everything(), ~ round(.x, 1))) %>%
    mutate(value = paste0(mean, " (", q1, ", ", q3, ")")) %>%
    mutate(xlab = {{ xlab }}) %>%
    mutate(xval = "NA") %>%
    select({{ group }}, xlab, xval, value)
  
  # Return Formatted Result
  return(out)
}

# Function: Table 1, Discrete Variables
tbl1_disc <- function(data, x, group = SEBMICUT, xlab = "Variable") 
{
  # Calculate Summary Statistics
  out_raw <- data %>%
    group_by({{ group }}) %>%
    count({{ x }}) %>%
    mutate(pr = n / sum(n) * 100)
  
  # Format Statistics
  out <- out_raw %>%
    mutate(across(everything(), ~ round(.x, 1))) %>%
    mutate(value = paste0(n, " (", pr, ")")) %>%
    mutate(xlab = {{ xlab }}) %>%
    select({{ group }}, xlab, xval = {{ x }}, value)
  
  # Return Formatted Result
  return(out)
}

##### Figures ##################################################################
# Function: Figures S2-S6 (Make Single Panel)
loess_panel <- function(data, x, y, xlim = NULL, ylim = NULL, xlab = NULL, 
  ylab = NULL) 
{
  out <- data %>%
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim) +
    labs(
      x = xlab,
      y = ylab) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(out)
}

# Function: Figures S2-S6 (Make Multi-panel Plot)
loess_plot <- function(data, x, xlim = NULL, xlab = NULL) 
{
  require(patchwork)
  
  figA <- data %>% 
    loess_panel(
      x = {{ x }}, y = piAs, 
      xlim = xlim, ylim = c(0,40), 
      xlab = xlab, ylab = "iAs%"
    )

  figB <- data %>% 
    loess_panel(
      x = {{ x }}, y = pMMA, 
      xlim = xlim, ylim = c(0,30), 
      xlab = xlab, ylab = "MMA%"
    )

  figC <- data %>% 
    loess_panel(
      x = {{ x }}, y = pDMA, 
      xlim = xlim, ylim = c(50,100), 
      xlab = xlab, ylab = "DMA%"
    )

  figD <- data %>% 
    loess_panel(
      x = {{ x }}, y = ln_PMI, 
      xlim = xlim, ylim = c(-2,1), 
      xlab = xlab, ylab = "ln PMI"
    )

  figE <- data %>% 
    loess_panel(
      x = {{ x }}, y = ln_SMI, 
      xlim = xlim, ylim = c(1,4), 
      xlab = xlab, ylab = "ln SMI"
    )

  out <- figA + figB + figC + figD + figE
  return(out)
  
}

