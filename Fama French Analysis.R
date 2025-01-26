##
## Fama-French analysis
## last edit:  25 jan 2025
##

 
fpath <- "~/Downloads/"

library(tidyverse)
library(assertr)
library(broom)
library(readr)

# Load FF Data
ff3_data <- read_csv(paste0(fpath,"F-F_Research_Data_Factors.CSV"), skip = 3, 
                     col_names = c("date", "mkt_rf", "smb", "hml", "rf"),
                     col_types = cols(
                       date = col_character(),
                       mkt_rf = col_double(),
                       smb = col_double(),
                       hml = col_double(),
                       rf = col_double()
                     )) %>%
  mutate(date = ymd(paste0(as.character(date), "01")) %m+% months(1) - days(1)) %>%
  filter(!is.na(date))

ff5_data <- read_csv(paste0(fpath,"F-F_Research_Data_5_Factors_2x3.csv"), skip = 3,
                     col_names = c("date", "mkt_rf", "smb", "hml", "rmw", "cma", "rf"),
                     col_types = cols(
                       date = col_character(),
                       mkt_rf = col_double(),
                       smb = col_double(),
                       hml = col_double(),
                       rmw = col_double(),
                       cma = col_double(),
                       rf = col_double()
                     )) %>%
  mutate(date = ymd(paste0(as.character(date), "01")) %m+% months(1) - days(1)) %>%
  filter(!is.na(date))

# Load and validate returns data
returns_data <- read_csv(paste0(fpath,"endowmentTS.csv")) %>%
  verify(has_all_names("date", "Endowment", "First Horizon", "Vanguard", "RBC", "IPS Target")) %>%
  mutate(date = ymd(date)) %>%
  mutate(
    Endowment = (Endowment - 1) * 100,
    `First Horizon` = (`First Horizon` - 1) * 100,
    Vanguard = (Vanguard - 1) * 100,
    RBC = (RBC - 1) * 100,
    `IPS Target` = (`IPS Target` - 1) * 100
  ) %>%
  arrange(date) %>%
  verify(nrow(.) > 0) %>%
  verify(!any(is.na(date))) %>%
  verify(all(map_lgl(select(., -date), is.numeric)))

# Function to run models for each return series
run_ff_models <- function(return_series_name, returns_data, ff3_data, ff5_data) {
  # Handle spaces in column names
  model_data_3f <- returns_data %>%
    select(date, all_of(return_series_name)) %>%
    inner_join(ff3_data, by = "date") %>%
    mutate(excess_return = get(return_series_name) - rf) %>%
    drop_na()
  
  model_data_5f <- returns_data %>%
    select(date, all_of(return_series_name)) %>%
    inner_join(ff5_data, by = "date") %>%
    mutate(excess_return = get(return_series_name) - rf) %>%
    drop_na()
  
  model_3f <- lm(excess_return ~ mkt_rf + smb + hml, data = model_data_3f)
  model_5f <- lm(excess_return ~ mkt_rf + smb + hml + rmw + cma, data = model_data_5f)
  
  list(
    series_name = return_series_name,
    model_3f = list(
      fit = model_3f,
      summary = tidy(model_3f, conf.int = TRUE),
      metrics = glance(model_3f),
      data = model_data_3f
    ),
    model_5f = list(
      fit = model_5f,
      summary = tidy(model_5f, conf.int = TRUE),
      metrics = glance(model_5f),
      data = model_data_5f
    )
  )
}

# Run models for each return series
return_series <- c("Endowment", "First Horizon", "Vanguard", "RBC", "IPS Target")
model_results <- return_series %>%
  set_names() %>%
  map(~run_ff_models(.x, returns_data, ff3_data, ff5_data))

# Create summary table
results_summary <- model_results %>%
  map_dfr(function(result) {
    bind_rows(
      result$model_3f$summary %>%
        mutate(
          series = result$series_name,
          model = "3-Factor",
          r.squared = result$model_3f$metrics$r.squared,
          adj.r.squared = result$model_3f$metrics$adj.r.squared
        ),
      result$model_5f$summary %>%
        mutate(
          series = result$series_name,
          model = "5-Factor",
          r.squared = result$model_5f$metrics$r.squared,
          adj.r.squared = result$model_5f$metrics$adj.r.squared
        )
    )
  }) %>%
  select(series, model, term, estimate, std.error, conf.low, conf.high, 
         statistic, p.value, r.squared, adj.r.squared) %>%
  arrange(series, model, term)

write_csv(results_summary, paste0(fpath,"factor_model_results.csv"))

# Generate diagnostic and coefficient plots
model_results %>%
  walk(function(result) {
    # Original residual plots
    p3f_resid <- augment(result$model_3f$fit) %>%
      ggplot(aes(x = .fitted, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste(result$series_name, "3-Factor Model"),
           x = "Fitted values", y = "Residuals")
    
    p5f_resid <- augment(result$model_5f$fit) %>%
      ggplot(aes(x = .fitted, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste(result$series_name, "5-Factor Model"),
           x = "Fitted values", y = "Residuals")
    
    # Coefficient plots
    p3f_coef <- result$model_3f$summary %>%
      filter(term != "(Intercept)") %>%
      ggplot(aes(x = term, y = estimate)) +
      geom_point() +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste(result$series_name, "3-Factor Coefficients"),
           x = "Factor", y = "Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    p5f_coef <- result$model_5f$summary %>%
      filter(term != "(Intercept)") %>%
      ggplot(aes(x = term, y = estimate)) +
      geom_point() +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste(result$series_name, "5-Factor Coefficients"),
           x = "Factor", y = "Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # P-value plots
    p3f_pval <- result$model_3f$summary %>%
      filter(term != "(Intercept)") %>%
      ggplot(aes(x = term, y = -log10(p.value))) +
      geom_col() +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
      labs(title = paste(result$series_name, "3-Factor P-values"),
           x = "Factor", y = "-log10(p-value)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    p5f_pval <- result$model_5f$summary %>%
      filter(term != "(Intercept)") %>%
      ggplot(aes(x = term, y = -log10(p.value))) +
      geom_col() +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
      labs(title = paste(result$series_name, "5-Factor P-values"),
           x = "Factor", y = "-log10(p-value)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save all plots
    pdf(paste0(fpath,"analysis_", result$series_name, ".pdf"), width = 12, height = 18)
    gridExtra::grid.arrange(
      p3f_resid, p5f_resid,
      p3f_coef, p5f_coef,
      p3f_pval, p5f_pval,
      ncol = 2
    )
    dev.off()
  })