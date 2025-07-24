#' ACT Analysis: Comprehensive Student Performance Analysis
#'
#' @description
#' This script performs comprehensive analysis of ACT test scores across multiple
#' scenarios including repeat test analysis, honors student performance, and
#' score improvement patterns.
#'
#' @details
#' The analysis includes 7 different scenarios:
#' \itemize{
#'   \item Scenario 1: Full dataset including repeat tests
#'   \item Scenario 2: Best composite score per student
#'   \item Scenario 3: Worst composite score per student
#'   \item Scenario 4: Median scores per student
#'   \item Scenario 5: Score changes for repeat test takers
#'   \item Scenario 6: Honors students with all tests
#'   \item Scenario 7: Final attempt with attempt count
#' }
#'
#' @author Craigile J. Brumfield
#' @date 2024-12-14
#' @version 2.0
#' @last_edit 2025-07-23

# Load necessary libraries
library(tidyverse)  # Loads dplyr, tidyr, ggplot2, purrr, readr, tibble, stringr, forcats
library(quantreg)
library(broom)      # For model tidying

#' ACT Benchmarks
#'
#' @description Benchmark scores for different ACT subject areas
#'
#' @format A tibble with 1 row and 5 variables:
#' \describe{
#'   \item{English}{English benchmark score}
#'   \item{Math}{Math benchmark score}
#'   \item{Science}{Science benchmark score}
#'   \item{STEM}{STEM benchmark score}
#'   \item{Social Sciences}{Social Sciences benchmark score}
#' }
act_benchmarks <- tibble(
  English = 18,
  Math = 22,
  Science = 23,
  STEM = 26,
  `Social Sciences` = 22
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Normalize Numeric Vector
#'
#' @description Standardize a numeric vector by subtracting the mean and 
#' dividing by the standard deviation (z-score normalization)
#'
#' @param x A numeric vector to normalize
#'
#' @return A normalized numeric vector with mean 0 and standard deviation 1
#'
#' @examples
#' normalize(c(1, 2, 3, 4, 5))
#' normalize(mtcars$mpg)
#'
#' @export
normalize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

#' Calculate Correlation Matrix
#'
#' @description Calculate correlation matrix for all numeric columns in a data frame
#'
#' @param df A data frame containing numeric columns
#'
#' @return A correlation matrix of numeric variables
#'
#' @examples
#' correlation_matrix(mtcars)
#'
#' @export
correlation_matrix <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    cor(use = "complete.obs")
}

#' Impute Missing Values with Median
#'
#' @description Replace missing values in numeric columns with their respective medians
#'
#' @param df A data frame that may contain missing values
#'
#' @return A data frame with missing numeric values replaced by medians
#'
#' @examples
#' df <- data.frame(x = c(1, 2, NA, 4), y = c(NA, 2, 3, 4))
#' impute_medians(df)
#'
#' @export
impute_medians <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x)))
}

#' Clean Data for Modeling
#'
#' @description Prepare data for statistical modeling by removing specified columns,
#' dropping missing values, and normalizing numeric variables
#'
#' @param df A data frame to clean
#' @param exclude_cols Character vector of column names to exclude from the cleaned data
#'
#' @return A cleaned data frame ready for modeling
#'
#' @examples
#' clean_for_modeling(mtcars, exclude_cols = c("vs", "am"))
#'
#' @export
clean_for_modeling <- function(df, exclude_cols = c("Count", "Last Name", "Test Date", "last_date", "Grade")) {
  df %>%
    select(-any_of(exclude_cols)) %>%
    drop_na() %>%
    mutate(across(where(is.numeric), normalize))
}

#' Fit Linear and Quantile Regression Models
#'
#' @description Fit both linear regression and quantile regression models using
#' the specified formula and data
#'
#' @param df A cleaned data frame for modeling
#' @param formula_str Character string representing the model formula
#' @param quantile_taus Numeric vector of quantiles for quantile regression
#'
#' @return A list containing linear and quantile regression models
#'
#' @examples
#' data(mtcars)
#' clean_data <- mtcars %>% mutate(across(where(is.numeric), normalize))
#' fit_models(clean_data, "mpg ~ wt + hp")
#'
#' @export
fit_models <- function(df, formula_str = "`Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`", 
                       quantile_taus = c(0.1, 0.25, 0.5, 0.75)) {
  
  formula_obj <- as.formula(formula_str)
  
  list(
    linear = lm(formula_obj, data = df),
    quantile = rq(formula_obj, data = df, tau = quantile_taus)
  )
}

#' Comprehensive Scenario Analysis
#'
#' @description Perform complete analysis for a given scenario including correlation
#' analysis, model fitting, and summary statistics
#'
#' @param df A data frame containing the scenario data
#' @param scenario_name Character string identifying the scenario
#' @param exclude_cols Character vector of columns to exclude from modeling
#' @param cor_exclude_cols Character vector of columns to exclude from correlation analysis
#' @param formula_str Character string representing the model formula
#' @param quantile_taus Numeric vector of quantiles for quantile regression
#'
#' @return A list containing scenario results including data, models, correlations, and summaries
#'
#' @examples
#' \dontrun{
#' result <- analyze_scenario(
#'   df = student_data,
#'   scenario_name = "baseline",
#'   formula_str = "outcome ~ predictor1 + predictor2"
#' )
#' }
#'
#' @export
analyze_scenario <- function(df, scenario_name, exclude_cols = c("Count", "Last Name", "Test Date", "last_date", "Grade"),
                             cor_exclude_cols = c("Grade", "Count", "Composite Score"),
                             formula_str = "`Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`",
                             quantile_taus = c(0.1, 0.25, 0.5, 0.75)) {
  
  # Calculate correlation matrix
  cor_matrix <- df %>%
    select(-any_of(cor_exclude_cols)) %>%
    mutate(across(where(is.numeric), normalize)) %>%
    correlation_matrix()
  
  # Clean data for modeling
  clean_df <- clean_for_modeling(df, exclude_cols)
  
  # Fit models
  models <- fit_models(clean_df, formula_str, quantile_taus)
  
  # Return results
  list(
    scenario = scenario_name,
    data = df,
    clean_data = clean_df,
    correlation_matrix = cor_matrix,
    models = models,
    summary = list(
      n_obs = nrow(df),
      n_clean = nrow(clean_df),
      linear_r2 = summary(models$linear)$r.squared,
      linear_adj_r2 = summary(models$linear)$adj.r.squared
    )
  )
}

# =============================================================================
# DATA PREPROCESSING
# =============================================================================

#' Initial Data Preprocessing
#'
#' @description Process the raw student population data by cleaning date formats
#' and grade information
#'
#' @details
#' This preprocessing step:
#' \itemize{
#'   \item Renames the date column for easier handling
#'   \item Converts date strings to Date objects
#'   \item Extracts numeric grade values from grade strings
#' }
#'
#' @note Requires 'stpop' data frame to be available in the environment
df_original <- stpop %>%
  rename(last_date = `Last Updated (CT)`) %>%
  mutate(
    last_date = as.Date(last_date, format = "%m/%d/%Y"),
    Grade = as.numeric(str_remove(Grade, "th"))
  )

#' Enhanced Honors Data Processing
#'
#' @description Process honors student data and join with original student data
#'
#' @details
#' This step:
#' \itemize{
#'   \item Renames honor score columns for consistency
#'   \item Joins honors data with original student data
#'   \item Handles duplicate count columns from the join
#' }
#'
#' @note Requires 'honors' and 'df_original' data frames to be available
df_honors <- honors %>%
  rename_with(~ paste0(.x, " Score"), !matches("Test Date|Count")) %>%
  left_join(
    df_original,
    by = join_by(
      `Test Date`, `Composite Score`, `Math Score`, `Science Score`, 
      `STEM Score`, `English Score`, `Reading Score`
    )
  ) %>%
  select(-Count.y) %>%
  rename(Count = Count.x)

#' Original Data Excluding Honors Students
#'
#' @description Create dataset of original students excluding honors students
#'
#' @details
#' Uses anti_join to identify students in the original dataset who are not
#' in the honors program based on matching test scores and dates
df_original_ex_honors <- df_original %>%
  anti_join(
    df_honors,
    by = join_by(
      `Test Date`, `Composite Score`, `Math Score`, `Science Score`,
      `STEM Score`, `English Score`, `Reading Score`
    )
  )

# =============================================================================
# SCENARIO DEFINITIONS
# =============================================================================

#' Scenario Transformation Functions
#'
#' @description List of functions that define how data should be transformed
#' for each analysis scenario
#'
#' @details
#' Each function takes a data frame as input and returns a transformed version
#' according to the specific scenario requirements:
#' \itemize{
#'   \item scenario_1: Full dataset with median imputation
#'   \item scenario_2: Best performance per student
#'   \item scenario_3: Worst performance per student
#'   \item scenario_4: Median performance per student
#'   \item scenario_5: Score changes for repeat test takers
#'   \item scenario_6: Honors students (identity transformation)
#'   \item scenario_7: Final attempt with attempt counting
#' }
scenario_transforms <- list(
  
  #' @describeIn scenario_transforms Full dataset including repeat tests
  scenario_1 = function(df) {
    df %>% impute_medians()
  },
  
  #' @describeIn scenario_transforms Best composite score per student
  scenario_2 = function(df) {
    df %>%
      group_by(`Last Name`) %>%
      slice_max(`Composite Score`, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      impute_medians()
  },
  
  #' @describeIn scenario_transforms Worst composite score per student
  scenario_3 = function(df) {
    df %>%
      group_by(`Last Name`) %>%
      slice_min(`Composite Score`, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      impute_medians()
  },
  
  #' @describeIn scenario_transforms Median scores per student
  scenario_4 = function(df) {
    df %>%
      group_by(`Last Name`) %>%
      summarise(
        across(contains("Score"), ~ median(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      left_join(
        df %>%
          distinct(`Last Name`, .keep_all = TRUE) %>%
          select(`Last Name`, `Average GPA`),
        by = "Last Name"
      ) %>%
      impute_medians()
  },
  
  #' @describeIn scenario_transforms Score changes for repeat test takers
  scenario_5 = function(df) {
    df %>%
      group_by(`Last Name`) %>%
      arrange(`Last Name`, last_date) %>%
      filter(n() > 1) %>%
      mutate(Attempts = row_number()) %>%
      filter(row_number() %in% c(1, n())) %>%
      impute_medians() %>%
      mutate(across(contains("Score"), ~ .x - lag(.x), .names = "diff_{.col}")) %>%
      ungroup() %>%
      select(contains("diff_"), `Last Name`, Attempts, `Average GPA`) %>%
      drop_na()
  },
  
  #' @describeIn scenario_transforms Honors students with all tests
  scenario_6 = function(df) {
    df %>% impute_medians()
  },
  
  #' @describeIn scenario_transforms Final attempt with attempt count
  scenario_7 = function(df) {
    df_mult_attempts <- df %>%
      group_by(`Last Name`) %>%
      arrange(`Last Name`, last_date) %>%
      mutate(Attempts = row_number()) %>%
      filter(Attempts > 1) %>%
      slice_max(Attempts, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    df %>%
      group_by(`Last Name`) %>%
      arrange(`Last Name`, last_date) %>%
      mutate(Attempts = row_number()) %>%
      ungroup() %>%
      anti_join(df_mult_attempts, by = "Last Name") %>%
      bind_rows(df_mult_attempts) %>%
      impute_medians()
  }
)

# =============================================================================
# SCENARIO CONFIGURATIONS
# =============================================================================

#' Scenario Configuration Table
#'
#' @description Configuration table defining parameters for each analysis scenario
#'
#' @format A tibble with 7 rows and 7 variables:
#' \describe{
#'   \item{scenario}{Scenario identifier}
#'   \item{data_source}{Name of the source data frame}
#'   \item{transform_func}{Name of the transformation function to apply}
#'   \item{formula}{Model formula as character string}
#'   \item{quantile_taus}{Quantiles for quantile regression as character string}
#'   \item{exclude_cols}{Columns to exclude from modeling as character string}
#'   \item{cor_exclude_cols}{Columns to exclude from correlation analysis as character string}
#' }
scenario_configs <- tribble(
  ~scenario, ~data_source, ~transform_func, ~formula, ~quantile_taus, ~exclude_cols, ~cor_exclude_cols,
  "scenario_1", "df_original", "scenario_1", "`Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`", "c(0.1, 0.25, 0.5, 0.75)", "c('Count', 'Last Name', 'Test Date', 'last_date')", "c('Grade', 'Count', 'Composite Score')",
  "scenario_2", "df_original", "scenario_2", "`Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`", "c(0.1, 0.25, 0.5, 0.75)", "c('Count', 'Last Name', 'Test Date', 'last_date')", "c('Grade', 'Count', 'Composite Score')",
  "scenario_3", "df_original", "scenario_3", "`Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`", "c(0.1, 0.25, 0.5, 0.75)", "c('Count', 'Last Name', 'Test Date', 'last_date')", "c('Grade', 'Count', 'Composite Score')",
  "scenario_4", "df_original", "scenario_4", "`Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`", "c(0.1, 0.25, 0.5, 0.75)", "c('Last Name')", "c('Composite Score')",
  "scenario_5", "df_original", "scenario_5", "`diff_Composite Score` ~ Attempts + `diff_Math Score` + `diff_Science Score` + `diff_English Score` + `diff_Reading Score`", "c(0.1, 0.25, 0.5, 0.75, 0.9)", "c('Last Name')", "c()",
  "scenario_6", "df_honors", "scenario_6", "`Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`", "c(0.1, 0.25, 0.5, 0.75)", "c('Last Name', 'Test Date', 'last_date')", "c('Grade', 'Count', 'Composite Score')",
  "scenario_7", "df_original", "scenario_7", "`Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score` + Attempts", "c(0.1, 0.25, 0.5, 0.75)", "c('Last Name', 'Test Date', 'last_date')", "c('Grade', 'Count', 'Composite Score')"
)

# =============================================================================
# SCENARIO EXECUTION ENGINE
# =============================================================================

#' Execute All Analysis Scenarios
#'
#' @description Execute all configured analysis scenarios and return comprehensive results
#'
#' @param configs A tibble containing scenario configurations (defaults to scenario_configs)
#'
#' @return A named list where each element contains the complete analysis results
#' for one scenario, including data, models, correlations, and summaries
#'
#' @details
#' This function iterates through all configured scenarios and:
#' \itemize{
#'   \item Applies the appropriate data transformation
#'   \item Performs correlation analysis
#'   \item Fits linear and quantile regression models
#'   \item Calculates summary statistics
#'   \item Returns structured results for comparison
#' }
#'
#' @examples
#' \dontrun{
#' # Execute all default scenarios
#' results <- execute_scenarios()
#' 
#' # Execute custom configuration
#' custom_config <- scenario_configs %>% filter(scenario %in% c("scenario_1", "scenario_2"))
#' results <- execute_scenarios(custom_config)
#' }
#'
#' @export
execute_scenarios <- function(configs = scenario_configs) {
  
  results <- list()
  
  for (i in seq_len(nrow(configs))) {
    config <- configs[i, ]
    
    cat("Executing", config$scenario, "...\n")
    
    # Get data source
    source_data <- get(config$data_source)
    
    # Apply transformation
    transformed_data <- scenario_transforms[[config$transform_func]](source_data)
    
    # Parse configuration strings
    exclude_cols <- eval(parse(text = config$exclude_cols))
    cor_exclude_cols <- eval(parse(text = config$cor_exclude_cols))
    quantile_taus <- eval(parse(text = config$quantile_taus))
    
    # Run analysis
    results[[config$scenario]] <- analyze_scenario(
      df = transformed_data,
      scenario_name = config$scenario,
      exclude_cols = exclude_cols,
      cor_exclude_cols = cor_exclude_cols,
      formula_str = config$formula,
      quantile_taus = quantile_taus
    )
  }
  
  return(results)
}

# =============================================================================
# ANALYSIS EXECUTION
# =============================================================================

#' Execute All Scenarios
#'
#' @description Run all configured analysis scenarios
scenario_results <- execute_scenarios()

#' Extract Models for Backward Compatibility
#'
#' @description Extract individual model objects to maintain compatibility
#' with existing code that expects specific variable names

# Linear models
model_1 <- scenario_results$scenario_1$models$linear
model_2 <- scenario_results$scenario_2$models$linear
model_3 <- scenario_results$scenario_3$models$linear
model_4 <- scenario_results$scenario_4$models$linear
model_5 <- scenario_results$scenario_5$models$linear
model_6 <- scenario_results$scenario_6$models$linear
model_7 <- scenario_results$scenario_7$models$linear

# Quantile regression models
model_1_multi <- scenario_results$scenario_1$models$quantile
model_2_multi <- scenario_results$scenario_2$models$quantile
model_3_multi <- scenario_results$scenario_3$models$quantile
model_4_multi <- scenario_results$scenario_4$models$quantile
model_5_multi <- scenario_results$scenario_5$models$quantile
model_6_multi <- scenario_results$scenario_6$models$quantile
model_7_multi <- scenario_results$scenario_7$models$quantile

# Correlation matrices
cor_matrix <- scenario_results$scenario_1$correlation_matrix
cor_matrix_2 <- scenario_results$scenario_2$correlation_matrix
cor_matrix_3 <- scenario_results$scenario_3$correlation_matrix
cor_matrix_4 <- scenario_results$scenario_4$correlation_matrix
cor_matrix_5 <- scenario_results$scenario_5$correlation_matrix
cor_matrix_6 <- scenario_results$scenario_6$correlation_matrix
cor_matrix_7 <- scenario_results$scenario_7$correlation_matrix

# =============================================================================
# RESULTS SUMMARY AND UTILITIES
# =============================================================================

#' Summarize Analysis Scenarios
#'
#' @description Create a summary table of all scenario results for easy comparison
#'
#' @param results A list of scenario results from execute_scenarios()
#'
#' @return A tibble containing summary statistics for each scenario including
#' sample sizes, R-squared values, and model formulas
#'
#' @examples
#' \dontrun{
#' summary_table <- summarize_scenarios(scenario_results)
#' print(summary_table)
#' }
#'
#' @export
summarize_scenarios <- function(results) {
  
  summary_df <- map_dfr(results, ~ {
    tibble(
      scenario = .x$scenario,
      n_observations = .x$summary$n_obs,
      n_clean_observations = .x$summary$n_clean,
      r_squared = round(.x$summary$linear_r2, 4),
      adj_r_squared = round(.x$summary$linear_adj_r2, 4),
      formula = deparse(.x$models$linear$call$formula)
    )
  })
  
  return(summary_df)
}

#' Compare Model Coefficients Across Scenarios
#'
#' @description Extract and compare model coefficients across all scenarios
#'
#' @param results A list of scenario results from execute_scenarios()
#'
#' @return A tibble containing coefficient estimates, standard errors, and
#' significance tests for each variable in each scenario
#'
#' @examples
#' \dontrun{
#' coef_comparison <- compare_models(scenario_results)
#' print(coef_comparison)
#' }
#'
#' @export
compare_models <- function(results) {
  
  model_comparison <- map_dfr(results, ~ {
    model_summary <- tidy(.x$models$linear)
    model_summary$scenario <- .x$scenario
    model_summary
  }, .id = "scenario_id")
  
  return(model_comparison)
}

#' Compare Correlation Matrices Across Scenarios
#'
#' @description Extract and optionally subset correlation matrices for comparison
#'
#' @param results A list of scenario results from execute_scenarios()
#' @param variables Optional character vector of variable names to subset correlations
#'
#' @return A named list of correlation matrices, optionally subsetted to specified variables
#'
#' @examples
#' \dontrun{
#' # Get all correlations
#' all_cors <- compare_correlations(scenario_results)
#' 
#' # Get correlations for specific variables
#' gpa_cors <- compare_correlations(scenario_results, c("Average GPA", "Composite Score"))
#' }
#'
#' @export
compare_correlations <- function(results, variables = NULL) {
  
  correlations <- map(results, ~ .x$correlation_matrix)
  
  if (!is.null(variables)) {
    correlations <- map(correlations, ~ .x[variables, variables])
  }
  
  return(correlations)
}

#' Generate and Display Scenario Summary
#'
#' @description Generate summary table and print to console
scenario_summary <- summarize_scenarios(scenario_results)
print("Scenario Summary:")
print(scenario_summary)

# =============================================================================
# ENHANCED VISUALIZATION FUNCTIONS
# =============================================================================

#' Plot Score Distributions
#'
#' @description Create density plots showing the distribution of different test scores
#'
#' @param df A data frame containing score columns
#' @param title_suffix Optional character string to append to the plot title
#'
#' @return A ggplot object showing score distributions
#'
#' @examples
#' \dontrun{
#' plot_score_distributions(df_original, " - All Students")
#' plot_score_distributions(df_honors, " - Honors Students")
#' }
#'
#' @export
plot_score_distributions <- function(df, title_suffix = "") {
  df %>%
    select(contains("Score")) %>%
    pivot_longer(everything(), names_to = "Subject", values_to = "Score") %>%
    mutate(Subject = str_remove(Subject, " Score")) %>%
    ggplot(aes(x = Score, fill = Subject)) +
    geom_density(alpha = 0.6) +
    labs(
      title = str_glue("Distributions of Subject Scores{title_suffix}"),
      x = "Score",
      y = "Density",
      fill = "Subject"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#' Plot Correlation Heatmap
#'
#' @description Create a heatmap visualization of a correlation matrix
#'
#' @param cor_matrix A correlation matrix (typically from correlation_matrix())
#' @param title Character string for the plot title
#'
#' @return A ggplot object showing the correlation heatmap
#'
#' @examples
#' \dontrun{
#' plot_correlation_heatmap(cor_matrix, "Scenario 1: Full Dataset")
#' }
#'
#' @export
plot_correlation_heatmap <- function(cor_matrix, title = "Correlation Matrix") {
  cor_matrix %>%
    as_tibble(rownames = "var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
    ggplot(aes(x = var1, y = var2, fill = correlation)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "red",
      mid = "white", 
      high = "green",
      midpoint = 0
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = "", y = "", fill = "Correlation")
}

#' Plot Model Coefficients Comparison
#'
#' @description Create a plot comparing model coefficients across scenarios
#'
#' @param results A list of scenario results from execute_scenarios()
#' @param coefficient Optional character string to filter to a specific coefficient
#'
#' @return A ggplot object showing coefficient estimates with error bars across scenarios
#'
#' @examples
#' \dontrun{
#' # Plot all coefficients
#' plot_model_coefficients(scenario_results)
#' 
#' # Plot specific coefficient
#' plot_model_coefficients(scenario_results, "Average GPA")
#' }
#'
#' @export
plot_model_coefficients <- function(results, coefficient = NULL) {
  
  coef_data <- compare_models(results)
  
  if (!is.null(coefficient)) {
    coef_data <- coef_data %>% filter(term == coefficient)
  }
  
  coef_data %>%
    filter(term != "(Intercept)") %>%
    ggplot(aes(x = scenario, y = estimate, color = term)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
    facet_wrap(~ term, scales = "free_y") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Model Coefficients Across Scenarios",
      x = "Scenario",
      y = "Coefficient Estimate",
      color = "Variable"
    )
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

#' @examples
#' \dontrun{
#' # Generate plots
#' plot_score_distributions(df_original, " - All Students")
#' plot_score_distributions(df_honors, " - Honors Students") 
#' plot_correlation_heatmap(cor_matrix, "Scenario 1: Full Dataset")
#' plot_model_coefficients(scenario_results)
#' 
#' # Compare specific correlations
#' gpa_correlations <- compare_correlations(scenario_results, c("Average GPA", "Composite Score"))
#' 
#' # Access specific scenario results
#' scenario_1_results <- scenario_results$scenario_1
#' scenario_1_linear_model <- scenario_1_results$models$linear
#' scenario_1_quantile_models <- scenario_1_results$models$quantile
#' 
#' # Extract coefficient information
#' model_comparison <- compare_models(scenario_results)
#' scenario_summary <- summarize_scenarios(scenario_results)
#' 
#' # Create visualizations
#' p1 <- plot_score_distributions(df_original, " - All Students")
#' p2 <- plot_correlation_heatmap(cor_matrix)
#' p3 <- plot_model_coefficients(scenario_results)
#' }