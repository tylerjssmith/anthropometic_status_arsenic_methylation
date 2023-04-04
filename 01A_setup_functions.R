################################################################################
# Pregnancy, Arsenic, and Immune Response (PAIR) Study
# Anthropometric Status and Arsenic Methylation -- Load Functions

# Tyler Smith
# April 4, 2023

##### Define Functions #########################################################
# Function: Table 1, Continuous Variables
tbl1_cont <- function(data, x, xlab = "Variable") {
  # Calculate Summary Statistics
  out_raw <- data %>%
    summarise(
      mean = mean({{ x }}),
      q1 = quantile({{ x }}, 0.25),
      q3 = quantile({{ x }}, 0.75))
  
  # Format Statistics
  out <- out_raw %>%
    mutate(across(everything(), ~ round(.x, 1))) %>%
    mutate(x = paste0(mean, " (", q1, ", ", q3, ")")) %>%
    mutate(xlab = {{ xlab }}) %>%
    select(xlab, x)
  
  # Return Formatted Result
  return(out)
}

# Function: Table 1, Discrete Variables
tbl1_disc <- function(data, x, xlab = "Variable") {
  # Calculate Summary Statistics
  out_raw <- data %>%
    count({{ x }}) %>%
    mutate(pr = n / sum(n) * 100)
  
  # Format Statistics
  out <- out_raw %>%
    mutate(across(everything(), ~ round(.x, 1))) %>%
    mutate(x = paste0(n, " (", pr, ")")) %>%
    mutate(xlab = {{ xlab }}) %>%
    select(xlab, x)
  
  # Return Formatted Result
  return(out)
}

# Function: Figures S2-S4
fn_figS2S3S4S5 <- function(data, x, y, xlim = NULL, ylim = NULL, xlab, ylab) {
  out <- df %>%
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

# Function: Make Quartiles
quartile <- function(data, name, x) {
  data %>%
    mutate({{ name }} := cut(
      x = {{ x }}, 
      breaks = quantile({{ x }}, c(0.00, 0.25, 0.50, 0.75, 1.00)), 
      include.lowest = TRUE,
      labels = FALSE)
    )
}

# Function: Tidy Model Estimates
tidier <- function(model, variable, y, x, adj) {
  out <- tidy(model, conf.int = TRUE) %>%
    filter(term == variable) %>%
    select(term, estimate, conf.low, conf.high) %>%
    mutate(y = y) %>%
    mutate(x = x) %>%
    mutate(adj = adj)
  return(out)
}

# Function: Tidy Dirichlet Model Estimates
tidier_dr <- function(model, set, x) {
  out <- unclass(summary(model))$coef.mat %>%
    as_tibble(rownames = "term") %>%
    filter(term == x) %>%
    mutate(set = set) %>%
    mutate(y = c("MMA%","DMA%")) %>%
    mutate(lb = Estimate - 1.96 * `Std. Error`) %>%
    mutate(ub = Estimate + 1.96 * `Std. Error`) %>%
    select(set, y, term, Estimate, lb, ub)
  return(out)
}
