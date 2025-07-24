##
## Improved Fama-French Analysis
## Enhanced with robust tidyverse practices and error handling
## Last edit: 23 Jul 2025
## Author:  Craigile Brumfield
##

# Load required packages with error handling ----
required_packages <- c("tidyverse", "assertr", "broom", "readr", "corrplot", 
                       "lubridate", "gridExtra", "here", "glue", "janitor")

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(glue("Missing required packages: {paste(missing_packages, collapse = ', ')}. 
            Please install with: install.packages(c({paste(paste0('\"', missing_packages, '\"'), collapse = ', ')})"))
}

suppressPackageStartupMessages({
  walk(required_packages, library, character.only = TRUE)
})

# Configuration and Setup ----
CONFIG <- list(
  data_path = "~/Downloads/",
  output_path = "~/Downloads/",
  ff3_filename = "F-F_Research_Data_Factors.CSV",
  ff5_filename = "F-F_Research_Data_5_Factors_2x3.csv",
  returns_filename = "endowmentTS.csv",
  date_format = "%Y%m%d",
  skip_rows = 3L,
  plot_width = 12,
  plot_height = 18,
  cor_plot_width = 10,
  cor_plot_height = 8
)

# Utility Functions ----

#' Validate file existence and readability
#' @param filepath Character path to file
#' @param description Character description for error messages
validate_file <- function(filepath, description = "File") {
  if (!file.exists(filepath)) {
    stop(glue("{description} not found at: {filepath}"))
  }
  if (!file.access(filepath, mode = 4) == 0) {
    stop(glue("{description} is not readable: {filepath}"))
  }
  invisible(TRUE)
}

#' Create safe filename by replacing spaces and special characters
#' @param name Character string to clean
clean_filename <- function(name) {
  str_replace_all(name, "[^A-Za-z0-9_-]", "_") %>%
    str_replace_all("_{2,}", "_") %>%
    str_remove("^_|_$")
}

#' Robust date parsing for Fama-French data
#' @param date_string Character date in YYYYMM format
parse_ff_date <- function(date_string) {
  tryCatch({
    # Convert YYYYMM to end of month date
    parsed_date <- ymd(paste0(as.character(date_string), "01")) %m+% months(1) - days(1)
    if (any(is.na(parsed_date))) {
      warning(glue("Failed to parse {sum(is.na(parsed_date))} dates"))
    }
    parsed_date
  }, error = function(e) {
    stop(glue("Date parsing failed: {e$message}"))
  })
}

#' Enhanced correlation matrix creation and saving
#' @param data Data frame with numeric variables
#' @param title Character title for the plot
#' @param filepath Character path where to save the plot
#' @param method Character correlation method
create_correlation_plot <- function(data, title, filepath, method = "pearson") {
  
  # Validate inputs
  if (!is.data.frame(data)) stop("Data must be a data frame")
  if (ncol(select(data, where(is.numeric))) < 2) {
    warning("Insufficient numeric columns for correlation analysis")
    return(NULL)
  }
  
  tryCatch({
    # Calculate correlation matrix
    numeric_data <- data %>% 
      select(where(is.numeric)) %>%
      drop_na()
    
    if (nrow(numeric_data) == 0) {
      warning("No complete cases available for correlation analysis")
      return(NULL)
    }
    
    cor_matrix <- cor(numeric_data, method = method, use = "complete.obs")
    
    # Create directory if it doesn't exist
    dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
    
    # Save correlation plot
    pdf(filepath, width = CONFIG$cor_plot_width, height = CONFIG$cor_plot_height)
    corrplot(cor_matrix, 
             method = "color",
             type = "upper",
             order = "hclust",
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45,
             title = title,
             mar = c(0, 0, 2, 0),
             number.cex = 0.8)
    dev.off()
    
    message(glue("Correlation plot saved: {filepath}"))
    return(cor_matrix)
    
  }, error = function(e) {
    warning(glue("Failed to create correlation plot: {e$message}"))
    return(NULL)
  })
}

# Data Loading Functions ----

#' Load and clean Fama-French 3-factor data
#' @param filepath Character path to the CSV file
load_ff3_data <- function(filepath) {
  
  validate_file(filepath, "Fama-French 3-factor data file")
  
  message("Loading Fama-French 3-factor data...")
  
  tryCatch({
    ff3_data <- read_csv(
      filepath, 
      skip = CONFIG$skip_rows,
      col_names = c("date", "mkt_rf", "smb", "hml", "rf"),
      col_types = cols(
        date = col_character(),
        mkt_rf = col_double(),
        smb = col_double(),
        hml = col_double(),
        rf = col_double()
      ),
      show_col_types = FALSE
    ) %>%
      clean_names() %>%
      filter(!is.na(date), str_detect(date, "^\\d{6}$")) %>%
      mutate(
        date = parse_ff_date(date),
        across(c(mkt_rf, smb, hml, rf), ~ as.numeric(.x) / 100)  # Convert percentages
      ) %>%
      filter(!is.na(date)) %>%
      arrange(date) %>%
      verify(nrow(.) > 0, error_fun = error_report) %>%
      verify(all(!is.na(date)), error_fun = error_report) %>%
      verify(all(map_lgl(select(., -date), is.numeric)), error_fun = error_report)
    
    message(glue("Loaded {nrow(ff3_data)} observations from {min(ff3_data$date)} to {max(ff3_data$date)}"))
    return(ff3_data)
    
  }, error = function(e) {
    stop(glue("Failed to load FF3 data: {e$message}"))
  })
}

#' Load and clean Fama-French 5-factor data
#' @param filepath Character path to the CSV file
load_ff5_data <- function(filepath) {
  
  validate_file(filepath, "Fama-French 5-factor data file")
  
  message("Loading Fama-French 5-factor data...")
  
  tryCatch({
    ff5_data <- read_csv(
      filepath,
      skip = CONFIG$skip_rows,
      col_names = c("date", "mkt_rf", "smb", "hml", "rmw", "cma", "rf"),
      col_types = cols(
        date = col_character(),
        mkt_rf = col_double(),
        smb = col_double(),
        hml = col_double(),
        rmw = col_double(),
        cma = col_double(),
        rf = col_double()
      ),
      show_col_types = FALSE
    ) %>%
      clean_names() %>%
      filter(!is.na(date), str_detect(date, "^\\d{6}$")) %>%
      mutate(
        date = parse_ff_date(date),
        across(c(mkt_rf, smb, hml, rmw, cma, rf), ~ as.numeric(.x) / 100)  # Convert percentages
      ) %>%
      filter(!is.na(date)) %>%
      arrange(date) %>%
      verify(nrow(.) > 0, error_fun = error_report) %>%
      verify(all(!is.na(date)), error_fun = error_report) %>%
      verify(all(map_lgl(select(., -date), is.numeric)), error_fun = error_report)
    
    message(glue("Loaded {nrow(ff5_data)} observations from {min(ff5_data$date)} to {max(ff5_data$date)}"))
    return(ff5_data)
    
  }, error = function(e) {
    stop(glue("Failed to load FF5 data: {e$message}"))
  })
}

#' Load and clean returns data
#' @param filepath Character path to the CSV file
load_returns_data <- function(filepath) {
  
  validate_file(filepath, "Returns data file")
  
  message("Loading returns data...")
  
  expected_cols <- c("date", "Endowment", "First Horizon", "Vanguard", "RBC", "IPS Target")
  
  tryCatch({
    returns_data <- read_csv(filepath, show_col_types = FALSE) %>%
      clean_names(case = "title") %>%
      verify(has_all_names(expected_cols), error_fun = error_report) %>%
      mutate(
        date = ymd(date),
        across(-date, ~ case_when(
          is.na(.x) ~ NA_real_,
          abs(.x) > 10 ~ (.x / 100),  # Assume percentages if values > 10
          TRUE ~ .x
        ))
      ) %>%
      # Convert to excess returns (assuming input is 1 + return format)
      mutate(across(-date, ~ case_when(
        .x > 1 ~ (.x - 1) * 100,  # Convert from 1.05 to 5%
        TRUE ~ .x * 100           # Convert from 0.05 to 5%
      ))) %>%
      arrange(date) %>%
      verify(nrow(.) > 0, error_fun = error_report) %>%
      verify(all(!is.na(date)), error_fun = error_report) %>%
      verify(all(map_lgl(select(., -date), is.numeric)), error_fun = error_report)
    
    message(glue("Loaded {nrow(returns_data)} observations from {min(returns_data$date)} to {max(returns_data$date)}"))
    
    # Summary statistics
    returns_summary <- returns_data %>%
      select(-date) %>%
      summarise(across(everything(), list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        na_count = ~ sum(is.na(.x))
      ), .names = "{.col}_{.fn}")) %>%
      pivot_longer(everything(), names_to = c("series", "stat"), names_sep = "_(?=[^_]+$)") %>%
      pivot_wider(names_from = stat, values_from = value)
    
    print(returns_summary)
    
    return(returns_data)
    
  }, error = function(e) {
    stop(glue("Failed to load returns data: {e$message}"))
  })
}

# Analysis Functions ----

#' Run Fama-French models for a single return series
#' @param return_series_name Character name of the return series column
#' @param returns_data Data frame with returns data
#' @param ff3_data Data frame with 3-factor data
#' @param ff5_data Data frame with 5-factor data
run_factor_models <- function(return_series_name, returns_data, ff3_data, ff5_data) {
  
  message(glue("Running factor models for: {return_series_name}"))
  
  tryCatch({
    # Prepare 3-factor model data
    model_data_3f <- returns_data %>%
      select(date, all_of(return_series_name)) %>%
      inner_join(ff3_data, by = "date", suffix = c("", "_ff")) %>%
      mutate(excess_return = !!sym(return_series_name) - rf) %>%
      drop_na() %>%
      verify(nrow(.) >= 24, error_fun = error_report)  # At least 2 years of data
    
    # Prepare 5-factor model data
    model_data_5f <- returns_data %>%
      select(date, all_of(return_series_name)) %>%
      inner_join(ff5_data, by = "date", suffix = c("", "_ff")) %>%
      mutate(excess_return = !!sym(return_series_name) - rf) %>%
      drop_na() %>%
      verify(nrow(.) >= 24, error_fun = error_report)  # At least 2 years of data
    
    message(glue("3-factor model: {nrow(model_data_3f)} observations"))
    message(glue("5-factor model: {nrow(model_data_5f)} observations"))
    
    # Generate correlation matrices
    clean_series_name <- clean_filename(return_series_name)
    
    cor_3f <- create_correlation_plot(
      model_data_3f %>% select(excess_return, mkt_rf, smb, hml),
      glue("{return_series_name} 3-Factor Correlations"),
      file.path(CONFIG$output_path, glue("corr_3f_{clean_series_name}.pdf"))
    )
    
    cor_5f <- create_correlation_plot(
      model_data_5f %>% select(excess_return, mkt_rf, smb, hml, rmw, cma),
      glue("{return_series_name} 5-Factor Correlations"),
      file.path(CONFIG$output_path, glue("corr_5f_{clean_series_name}.pdf"))
    )
    
    # Fit models with robust error handling
    model_3f <- safely(~ lm(excess_return ~ mkt_rf + smb + hml, data = .x))(model_data_3f)
    model_5f <- safely(~ lm(excess_return ~ mkt_rf + smb + hml + rmw + cma, data = .x))(model_data_5f)
    
    if (is.null(model_3f$result) || is.null(model_5f$result)) {
      stop("Model fitting failed")
    }
    
    # Extract model results
    result <- list(
      series_name = return_series_name,
      model_3f = list(
        fit = model_3f$result,
        summary = tidy(model_3f$result, conf.int = TRUE),
        metrics = glance(model_3f$result),
        data = model_data_3f,
        correlations = cor_3f,
        residuals = augment(model_3f$result)
      ),
      model_5f = list(
        fit = model_5f$result,
        summary = tidy(model_5f$result, conf.int = TRUE),
        metrics = glance(model_5f$result),
        data = model_data_5f,
        correlations = cor_5f,
        residuals = augment(model_5f$result)
      )
    )
    
    message(glue("Models completed for {return_series_name}"))
    return(result)
    
  }, error = function(e) {
    warning(glue("Failed to run models for {return_series_name}: {e$message}"))
    return(NULL)
  })
}

#' Create enhanced diagnostic and visualization plots
#' @param model_result List containing model results
create_diagnostic_plots <- function(model_result) {
  
  if (is.null(model_result)) return(NULL)
  
  series_name <- model_result$series_name
  clean_name <- clean_filename(series_name)
  
  tryCatch({
    # Enhanced residual plots with additional diagnostics
    p3f_resid <- model_result$model_3f$residuals %>%
      ggplot(aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
      labs(
        title = glue("{series_name}: 3-Factor Model Residuals"),
        subtitle = glue("R² = {round(model_result$model_3f$metrics$r.squared, 3)}"),
        x = "Fitted Values (%)", 
        y = "Residuals (%)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 11, face = "bold"))
    
    p5f_resid <- model_result$model_5f$residuals %>%
      ggplot(aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
      labs(
        title = glue("{series_name}: 5-Factor Model Residuals"),
        subtitle = glue("R² = {round(model_result$model_5f$metrics$r.squared, 3)}"),
        x = "Fitted Values (%)", 
        y = "Residuals (%)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 11, face = "bold"))
    
    # Enhanced coefficient plots with significance indicators
    p3f_coef <- model_result$model_3f$summary %>%
      filter(term != "(Intercept)") %>%
      mutate(
        significant = p.value < 0.05,
        term = factor(term, levels = c("mkt_rf", "smb", "hml"))
      ) %>%
      ggplot(aes(x = term, y = estimate, color = significant)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "steelblue"), 
                         name = "Significant\n(p < 0.05)") +
      labs(
        title = glue("{series_name}: 3-Factor Loadings"),
        x = "Factor", 
        y = "Beta Coefficient"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(angle = 0),
        legend.position = "bottom"
      )
    
    p5f_coef <- model_result$model_5f$summary %>%
      filter(term != "(Intercept)") %>%
      mutate(
        significant = p.value < 0.05,
        term = factor(term, levels = c("mkt_rf", "smb", "hml", "rmw", "cma"))
      ) %>%
      ggplot(aes(x = term, y = estimate, color = significant)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "steelblue"), 
                         name = "Significant\n(p < 0.05)") +
      labs(
        title = glue("{series_name}: 5-Factor Loadings"),
        x = "Factor", 
        y = "Beta Coefficient"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(angle = 0),
        legend.position = "bottom"
      )
    
    # Enhanced p-value visualization
    p3f_pval <- model_result$model_3f$summary %>%
      filter(term != "(Intercept)") %>%
      mutate(
        log_pval = -log10(pmax(p.value, 1e-10)),  # Avoid -Inf for very small p-values
        significant = p.value < 0.05,
        term = factor(term, levels = c("mkt_rf", "smb", "hml"))
      ) %>%
      ggplot(aes(x = term, y = log_pval, fill = significant)) +
      geom_col(alpha = 0.8) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red", linewidth = 1) +
      scale_fill_manual(values = c("FALSE" = "gray60", "TRUE" = "steelblue"), 
                        name = "Significant\n(p < 0.05)") +
      labs(
        title = glue("{series_name}: 3-Factor Significance"),
        x = "Factor", 
        y = "-log10(p-value)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(angle = 0),
        legend.position = "bottom"
      )
    
    p5f_pval <- model_result$model_5f$summary %>%
      filter(term != "(Intercept)") %>%
      mutate(
        log_pval = -log10(pmax(p.value, 1e-10)),  # Avoid -Inf for very small p-values
        significant = p.value < 0.05,
        term = factor(term, levels = c("mkt_rf", "smb", "hml", "rmw", "cma"))
      ) %>%
      ggplot(aes(x = term, y = log_pval, fill = significant)) +
      geom_col(alpha = 0.8) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red", linewidth = 1) +
      scale_fill_manual(values = c("FALSE" = "gray60", "TRUE" = "steelblue"), 
                        name = "Significant\n(p < 0.05)") +
      labs(
        title = glue("{series_name}: 5-Factor Significance"),
        x = "Factor", 
        y = "-log10(p-value)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(angle = 0),
        legend.position = "bottom"
      )
    
    # Save comprehensive diagnostic plots
    output_file <- file.path(CONFIG$output_path, glue("analysis_{clean_name}.pdf"))
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    
    pdf(output_file, width = CONFIG$plot_width, height = CONFIG$plot_height)
    grid.arrange(
      p3f_resid, p5f_resid,
      p3f_coef, p5f_coef,
      p3f_pval, p5f_pval,
      ncol = 2,
      top = textGrob(glue("Fama-French Analysis: {series_name}"), 
                     gp = gpar(fontsize = 16, fontface = "bold"))
    )
    dev.off()
    
    message(glue("Diagnostic plots saved: {output_file}"))
    
  }, error = function(e) {
    warning(glue("Failed to create diagnostic plots for {series_name}: {e$message}"))
  })
}

# Main Analysis Pipeline ----

#' Main function to run complete Fama-French analysis
run_fama_french_analysis <- function() {
  
  message("=== Starting Enhanced Fama-French Analysis ===")
  start_time <- Sys.time()
  
  tryCatch({
    # Load data
    ff3_data <- load_ff3_data(file.path(CONFIG$data_path, CONFIG$ff3_filename))
    ff5_data <- load_ff5_data(file.path(CONFIG$data_path, CONFIG$ff5_filename))
    returns_data <- load_returns_data(file.path(CONFIG$data_path, CONFIG$returns_filename))
    
    # Extract return series names (excluding date)
    return_series <- setdiff(names(returns_data), "date")
    message(glue("Analyzing {length(return_series)} return series: {paste(return_series, collapse = ', ')}"))
    
    # Run models for each return series
    model_results <- return_series %>%
      set_names() %>%
      map(~ run_factor_models(.x, returns_data, ff3_data, ff5_data)) %>%
      compact()  # Remove NULL results
    
    if (length(model_results) == 0) {
      stop("No successful model results obtained")
    }
    
    # Create comprehensive results summary
    results_summary <- model_results %>%
      map_dfr(function(result) {
        bind_rows(
          result$model_3f$summary %>%
            mutate(
              series = result$series_name,
              model = "3-Factor",
              r_squared = result$model_3f$metrics$r.squared,
              adj_r_squared = result$model_3f$metrics$adj.r.squared,
              aic = result$model_3f$metrics$AIC,
              bic = result$model_3f$metrics$BIC,
              observations = result$model_3f$metrics$nobs
            ),
          result$model_5f$summary %>%
            mutate(
              series = result$series_name,
              model = "5-Factor",
              r_squared = result$model_5f$metrics$r.squared,
              adj_r_squared = result$model_5f$metrics$adj.r.squared,
              aic = result$model_5f$metrics$AIC,
              bic = result$model_5f$metrics$BIC,
              observations = result$model_5f$metrics$nobs
            )
        )
      }) %>%
      select(series, model, term, estimate, std.error, conf.low, conf.high, 
             statistic, p.value, r_squared, adj_r_squared, aic, bic, observations) %>%
      arrange(series, model, term)
    
    # Save results
    output_file <- file.path(CONFIG$output_path, "factor_model_results.csv")
    write_csv(results_summary, output_file)
    message(glue("Results summary saved: {output_file}"))
    
    # Generate diagnostic plots for each series
    message("Generating diagnostic plots...")
    walk(model_results, create_diagnostic_plots)
    
    # Create model comparison summary
    model_comparison <- model_results %>%
      map_dfr(function(result) {
        tibble(
          series = result$series_name,
          r2_3f = result$model_3f$metrics$r.squared,
          r2_5f = result$model_5f$metrics$r.squared,
          adj_r2_3f = result$model_3f$metrics$adj.r.squared,
          adj_r2_5f = result$model_5f$metrics$adj.r.squared,
          aic_3f = result$model_3f$metrics$AIC,
          aic_5f = result$model_5f$metrics$AIC,
          r2_improvement = result$model_5f$metrics$r.squared - result$model_3f$metrics$r.squared,
          better_model = if_else(result$model_5f$metrics$AIC < result$model_3f$metrics$AIC, "5-Factor", "3-Factor")
        )
      })
    
    write_csv(model_comparison, file.path(CONFIG$output_path, "model_comparison.csv"))
    
    # Print summary
    end_time <- Sys.time()
    runtime <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
    
    message("=== Analysis Complete ===")
    message(glue("Successfully analyzed {length(model_results)} return series"))
    message(glue("Total runtime: {runtime} seconds"))
    message(glue("Output files saved to: {CONFIG$output_path}"))
    
    print(model_comparison)
    
    return(list(
      results = model_results,
      summary = results_summary,
      comparison = model_comparison
    ))
    
  }, error = function(e) {
    stop(glue("Analysis failed: {e$message}"))
  })
}

# Execute Analysis ----
if (interactive() || !exists("SKIP_EXECUTION")) {
  analysis_results <- run_fama_french_analysis()
}