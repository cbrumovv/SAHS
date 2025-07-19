#' Time-Varying Portfolio Risk Contribution Analysis
#'
#' This script calculates the contribution of each asset to total portfolio risk
#' with support for time-varying weights and active weight calculation. It provides
#' comprehensive analysis including portfolio vs benchmark comparisons, factor analysis,
#' and equal risk contribution (ERC) portfolio construction.
#'
#' @author Craigile J. Brumfield, CFA
#' @date Created: 2025-04-13, Last Modified: 2025-04-19
#' @keywords portfolio risk contribution time-varying active weights benchmark
#' @import ggplot2 reshape2 zoo xts tidyverse fs here glue

# Install required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("zoo")) install.packages("zoo")
if (!require("xts")) install.packages("xts")

#' Calculate Risk Contributions with Fixed Weights
#'
#' This function calculates the contribution of each asset to total portfolio risk
#' using fixed (static) weights. It computes both Marginal Contribution to Risk (MCTR)
#' and Percentage Contribution to Risk (PCTR) for each asset.
#'
#' @param returns A data frame or matrix of asset returns where each column represents
#'   a different asset and each row represents a time period
#' @param weights A numeric vector of asset weights that sum to 1
#'
#' @return A list containing:
#'   \item{result}{A data frame with columns Asset, Weight, PCTR, and PCTR_Percent}
#'   \item{portfolio_volatility}{The portfolio's standard deviation}
#'   \item{covariance_matrix}{The covariance matrix of asset returns}
#'
#' @details The function calculates:
#'   - Marginal Contribution to Risk (MCTR): Partial derivative of portfolio risk with respect to weight
#'   - Percentage Contribution to Risk (PCTR): The percentage of total portfolio risk attributable to each asset
#'
#' @examples
#' \dontrun{
#' # Sample data
#' returns <- data.frame(
#'   Equity = rnorm(100, 0.01, 0.05),
#'   Bonds = rnorm(100, 0.005, 0.02),
#'   Commodities = rnorm(100, 0.008, 0.06)
#' )
#' weights <- c(0.6, 0.3, 0.1)
#' 
#' # Calculate risk contributions
#' risk_contrib <- calculate_risk_contributions(returns, weights)
#' print(risk_contrib$result)
#' }
#'
#' @export
calculate_risk_contributions <- function(returns, weights) {
  # Calculate covariance matrix of returns
  cov_matrix <- cov(returns)
  
  # Calculate portfolio variance
  portfolio_variance <- t(weights) %*% cov_matrix %*% weights
  
  # Calculate portfolio volatility (standard deviation)
  portfolio_volatility <- sqrt(as.numeric(portfolio_variance))
  
  # Calculate each asset's contribution to risk
  contributions <- numeric(length(weights))
  asset_names <- colnames(returns)
  
  for (i in 1:length(weights)) {
    # Marginal contribution to risk (MCTR)
    # This is the partial derivative of portfolio risk with respect to weight
    mctr <- as.numeric((cov_matrix %*% weights)[i] / portfolio_volatility)
    
    # Percentage contribution to risk (PCTR)
    pctr <- as.numeric(weights[i] * mctr / portfolio_volatility)
    
    contributions[i] <- pctr
  }
  
  # Create a dataframe with results
  result <- data.frame(
    Asset = asset_names,
    Weight = weights,
    PCTR = contributions,
    PCTR_Percent = contributions / sum(contributions) * 100
  )
  
  return(list(
    result = result,
    portfolio_volatility = portfolio_volatility,
    covariance_matrix = cov_matrix
  ))
}

#' Calculate Active Weights
#'
#' This function calculates active weights as the difference between portfolio weights
#' and benchmark weights. Active weights represent the portfolio manager's active bets
#' relative to the benchmark.
#'
#' @param portfolio_weights A matrix or data frame of portfolio weights over time,
#'   where each row represents a time period and each column represents an asset
#' @param benchmark_weights A matrix or data frame of benchmark weights over time,
#'   with the same dimensions as portfolio_weights
#'
#' @return A matrix of active weights (portfolio_weights - benchmark_weights)
#'   with the same dimensions and column names as the input matrices
#'
#' @details Active weights indicate:
#'   - Positive values: Overweight positions relative to benchmark
#'   - Negative values: Underweight positions relative to benchmark
#'   - Zero values: Neutral positions (same weight as benchmark)
#'
#' @examples
#' \dontrun{
#' # Time-varying weights data
#' portfolio_weights <- data.frame(
#'   Equity = runif(50, 0.5, 0.8),
#'   Bonds = runif(50, 0.2, 0.5)
#' )
#' benchmark_weights <- data.frame(
#'   Equity = rep(0.6, 50),
#'   Bonds = rep(0.4, 50)
#' )
#' 
#' # Calculate active weights
#' active_weights <- calculate_active_weights(portfolio_weights, benchmark_weights)
#' summary(active_weights)
#' }
#'
#' @export
calculate_active_weights <- function(portfolio_weights, benchmark_weights) {
  # Ensure both matrices have the same dimensions
  if (ncol(portfolio_weights) != ncol(benchmark_weights)) {
    stop("Portfolio and benchmark weights must have the same number of assets")
  }
  
  if (nrow(portfolio_weights) != nrow(benchmark_weights)) {
    stop("Portfolio and benchmark weights must have the same number of time periods")
  }
  
  # Calculate active weights as difference between portfolio and benchmark weights
  active_weights <- portfolio_weights - benchmark_weights
  
  # Ensure column names are preserved
  colnames(active_weights) <- colnames(portfolio_weights)
  
  return(active_weights)
}

#' Calculate Time-Varying Risk Contributions
#'
#' This function calculates the contribution of each asset to total portfolio risk
#' using time-varying weights over rolling windows. It applies a rolling window approach
#' to analyze how risk contributions evolve over time.
#'
#' @param returns A data frame or matrix of asset returns where each column represents
#'   a different asset and each row represents a time period
#' @param weight_matrix A matrix or data frame of asset weights over time, where each
#'   row represents a time period and each column represents an asset
#' @param window_size Integer specifying the number of periods to include in each rolling window (default: 12)
#' @param step_size Integer specifying the number of periods to step forward for each calculation (default: 1)
#'
#' @return A list of results for each time window, where each element contains:
#'   \item{result}{A data frame with risk contribution results for that window}
#'   \item{portfolio_volatility}{The portfolio's standard deviation for that window}
#'   \item{covariance_matrix}{The covariance matrix for that window}
#'   \item{period_start}{Starting index of the window}
#'   \item{period_end}{Ending index of the window}
#'
#' @details The function uses average weights over each window for stability.
#'   Alternative approaches like end-of-period weights can be implemented by
#'   modifying the weight calculation section.
#'
#' @examples
#' \dontrun{
#' # Sample time-varying data
#' returns <- data.frame(
#'   Equity = rnorm(60, 0.01, 0.05),
#'   Bonds = rnorm(60, 0.005, 0.02)
#' )
#' weights <- data.frame(
#'   Equity = runif(60, 0.4, 0.8),
#'   Bonds = runif(60, 0.2, 0.6)
#' )
#' # Normalize weights to sum to 1
#' weights <- weights / rowSums(weights)
#' 
#' # Calculate time-varying risk contributions
#' risk_contrib <- calculate_time_varying_risk_contributions(returns, weights)
#' }
#'
#' @export
calculate_time_varying_risk_contributions <- function(returns, weight_matrix, window_size = 12, step_size = 1) {
  # Ensure returns and weights cover the same periods
  if (nrow(returns) != nrow(weight_matrix)) {
    stop("Returns and weights must have the same number of rows (time periods)")
  }
  
  num_periods <- nrow(returns)
  num_assets <- ncol(returns)
  
  # Prepare storage for results
  all_results <- list()
  period_indices <- seq(window_size, num_periods, by = step_size)
  
  for (end_idx in period_indices) {
    start_idx <- end_idx - window_size + 1
    period_label <- paste0("Period_", start_idx, "_to_", end_idx)
    
    # Extract returns and average weights for this window
    window_returns <- returns[start_idx:end_idx, ]
    
    # Use either average weights over the window or ending weights
    # Option 1: Average weights over the window
    window_weights <- colMeans(weight_matrix[start_idx:end_idx, ])
    
    # Option 2: Use end-of-period weights
    # window_weights <- weight_matrix[end_idx, ]
    
    # Calculate risk contributions for this window
    window_results <- calculate_risk_contributions(window_returns, window_weights)
    window_results$result$Period <- period_label
    window_results$period_start <- start_idx
    window_results$period_end <- end_idx
    
    all_results[[length(all_results) + 1]] <- window_results
  }
  
  return(all_results)
}

#' Plot Static Risk Contributions
#'
#' This function creates a bar chart visualization of risk contributions for
#' a portfolio with static (fixed) weights.
#'
#' @param risk_contributions Output from calculate_risk_contributions function
#'
#' @return A ggplot object showing percentage contribution to portfolio risk by asset
#'
#' @details The plot shows each asset's percentage contribution to total active risk,
#'   with assets ordered by their contribution size.
#'
#' @examples
#' \dontrun{
#' returns <- data.frame(Equity = rnorm(100), Bonds = rnorm(100))
#' weights <- c(0.6, 0.4)
#' risk_results <- calculate_risk_contributions(returns, weights)
#' plot_risk_contributions(risk_results)
#' }
#'
#' @export
plot_risk_contributions <- function(risk_contributions) {
  # Plot percentage contribution to risk
  ggplot(risk_contributions$result, aes(x = reorder(Asset, PCTR_Percent), y = PCTR_Percent, fill = Asset)) +
    geom_bar(stat = "identity") +
    labs(title = "Percentage Contribution to Portfolio Risk by Asset",
         x = "Asset",
         y = "Percentage of Total Active Risk (%)") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
          axis.text.y = element_text(family = "Times New Roman"),
          axis.title = element_text(family = "Times New Roman"),
          plot.title = element_text(family = "Times New Roman"))
}

#' Plot Time-Varying Risk Contributions
#'
#' This function creates visualizations showing the evolution of risk contributions
#' over time, including both stacked bar charts and line plots.
#'
#' @param all_results List of risk contribution results from time-varying analysis
#' @param dates Vector of dates corresponding to the time periods
#'
#' @return Prints two plots: a stacked bar chart and a line plot showing evolution
#'   of each asset's risk contribution over time
#'
#' @details The function creates:
#'   - Stacked bar chart: Shows cumulative risk contributions over time
#'   - Line plot: Shows the evolution of each asset's individual contribution
#'   
#'   The function handles both Date objects and numeric time periods automatically.
#'
#' @examples
#' \dontrun{
#' # After time-varying risk contribution calculation
#' time_results <- calculate_time_varying_risk_contributions(returns, weights)
#' dates <- seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 50)
#' plot_time_varying_risk_contributions(time_results, dates)
#' }
#'
#' @export
plot_time_varying_risk_contributions <- function(all_results, dates) {
  # Combine all period results
  all_data <- do.call(rbind, lapply(all_results, function(x) x$result))
  
  # Map period indices to actual dates
  all_data$period_end_idx <- as.numeric(gsub(".*_.*_(.*)", "\\1", all_data$Period))
  all_data$Date <- dates[all_data$period_end_idx]
  
  # Handle Date column based on whether we have actual dates or indices
  if (inherits(dates, "Date")) {
    all_data$Date <- as.Date(all_data$Date, origin = "1970-01-01")
    x_label <- "Date"
    use_date_scale <- TRUE
  } else {
    all_data$Date <- as.numeric(all_data$Date)
    x_label <- "Time Period"
    use_date_scale <- FALSE
  }
  
  # Sort the data
  all_data <- all_data[order(all_data$Date), ]
  
  # Plot time series of risk contributions
  p1 <- ggplot(all_data, aes(x = Date, y = PCTR_Percent, fill = Asset, group = Asset)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Time-Varying Percentage Contribution to Portfolio Risk",
         x = x_label,
         y = "Percentage of Total Active Risk (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, family = "Times New Roman"),
          axis.text.y = element_text(family = "Times New Roman"),
          axis.title = element_text(family = "Times New Roman"),
          plot.title = element_text(family = "Times New Roman"),
          legend.text = element_text(family = "Times New Roman"),
          legend.title = element_text(family = "Times New Roman"))
  
  # Only add date scale if we have valid dates
  if (use_date_scale && all(!is.na(all_data$Date))) {
    p1 <- p1 + scale_x_date(date_labels = "%b-%y", date_breaks = "6 months")
  }
  
  print(p1)
  
  # Alternative: line plot showing evolution of each asset's contribution (based on active weights)
  p2 <- ggplot(all_data, aes(x = Date, y = PCTR_Percent, color = Asset, group = Asset)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = "Evolution of Risk Contribution by Asset Class",
         x = x_label,
         y = "Percentage of Total Active Risk (%)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, family = "Times New Roman"),
      axis.text.y = element_text(family = "Times New Roman"),
      axis.title = element_text(family = "Times New Roman"),
      plot.title = element_text(family = "Times New Roman"),
      legend.text = element_text(family = "Times New Roman"),
      legend.title = element_text(family = "Times New Roman")
    )
  
  # Only add date scale if we have valid dates
  if (use_date_scale && all(!is.na(all_data$Date))) {
    p2 <- p2 + scale_x_date(date_labels = "%b-%y", date_breaks = "6 months")
  }
  
  print(p2)
}

#' Plot Active Weights Over Time
#'
#' This function creates a line plot showing the evolution of active weights
#' (portfolio minus benchmark weights) over time for each asset.
#'
#' @param active_weights_df A data frame of active weights where each column
#'   represents an asset and each row represents a time period
#' @param dates Vector of dates corresponding to the time periods
#'
#' @return A ggplot object showing the evolution of active weights with a
#'   horizontal reference line at zero
#'
#' @details The plot includes:
#'   - Line for each asset showing active weight evolution
#'   - Horizontal dashed line at zero for reference
#'   - Automatic handling of Date objects vs numeric time periods
#'
#' @examples
#' \dontrun{
#' portfolio_weights <- data.frame(Equity = runif(50, 0.5, 0.8), Bonds = runif(50, 0.2, 0.5))
#' benchmark_weights <- data.frame(Equity = rep(0.6, 50), Bonds = rep(0.4, 50))
#' active_weights <- calculate_active_weights(portfolio_weights, benchmark_weights)
#' dates <- seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 50)
#' plot_active_weights(active_weights, dates)
#' }
#'
#' @export
plot_active_weights <- function(active_weights_df, dates) {
  # Convert to long format for plotting
  active_weights_long <- reshape2::melt(
    cbind(Date = dates, active_weights_df),
    id.vars = "Date",
    variable.name = "Asset",
    value.name = "Active_Weight"
  )
  
  # Handle Date column based on whether we have actual dates or indices
  if (inherits(dates, "Date")) {
    active_weights_long$Date <- as.Date(active_weights_long$Date, origin = "1970-01-01")
    x_label <- "Date"
    use_date_scale <- TRUE
  } else {
    active_weights_long$Date <- as.numeric(active_weights_long$Date)
    x_label <- "Time Period"
    use_date_scale <- FALSE
  }
  
  p <- ggplot(active_weights_long, aes(x = Date, y = Active_Weight, color = Asset)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    labs(title = "Evolution of Active Weights Over Time",
         x = x_label,
         y = "Active Weight (Portfolio - Benchmark)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
      axis.text.y = element_text(family = "Times New Roman"),
      axis.title = element_text(family = "Times New Roman"),
      plot.title = element_text(family = "Times New Roman"),
      legend.text = element_text(family = "Times New Roman"),
      legend.title = element_text(family = "Times New Roman")
    )
  
  # Only add date scale if we have valid dates
  if (use_date_scale && all(!is.na(active_weights_long$Date))) {
    p <- p + scale_x_date(date_labels = "%b-%y", date_breaks = "6 months")
  }
  
  return(p)
}

#' Read and Process Trading Data Files
#'
#' This utility function reads CSV files from the trading data directory and
#' optionally removes the Date column for calculations.
#'
#' @param file_path Character string specifying the full path to the CSV file
#' @param keep_date Logical indicating whether to keep the Date column (default: FALSE)
#'
#' @return A data frame with the CSV contents, optionally excluding the Date column
#'
#' @details The function:
#'   - Checks if the file exists before attempting to read
#'   - Uses read_csv for robust CSV parsing
#'   - Optionally removes Date column for numerical calculations
#'
#' @examples
#' \dontrun{
#' # Read returns data keeping dates
#' returns_with_dates <- read_trading_data("path/to/returns.csv", keep_date = TRUE)
#' 
#' # Read weights data for calculations
#' weights_data <- read_trading_data("path/to/weights.csv", keep_date = FALSE)
#' }
#'
#' @export
read_trading_data <- function(file_path, keep_date = FALSE) {
  if (!file_exists(file_path)) {
    stop(glue::glue("File not found: {file_path}"))
  }
  
  data <- read_csv(file_path)
  
  if (keep_date) {
    return(data)
  } else {
    return(data %>% select(-Date))
  }
}

#' Parse Dates from Multiple Formats
#'
#' This function attempts to parse date strings using multiple common date formats
#' and returns the successfully parsed dates or sequential indices as fallback.
#'
#' @param date_strings A vector of date strings to parse
#'
#' @return A vector of Date objects if parsing succeeds, or sequential integers as fallback
#'
#' @details The function tries multiple date formats including:
#'   - ISO format (YYYY-MM-DD)
#'   - US format (MM/DD/YYYY)
#'   - European format (DD/MM/YYYY)
#'   - Other common variations
#'   
#'   If no format achieves >80% success rate, it falls back to sequential indices.
#'
#' @examples
#' \dontrun{
#' # Various date formats
#' dates1 <- c("2023-01-15", "2023-02-15", "2023-03-15")
#' dates2 <- c("01/15/2023", "02/15/2023", "03/15/2023")
#' 
#' parsed1 <- parse_dates(dates1)  # Returns Date objects
#' parsed2 <- parse_dates(dates2)  # Returns Date objects
#' }
#'
#' @export
parse_dates <- function(date_strings) {
  # Common date formats to try
  formats <- c(
    "%Y-%m-%d",      # 2023-01-15
    "%m/%d/%Y",      # 01/15/2023
    "%d/%m/%Y",      # 15/01/2023
    "%Y/%m/%d",      # 2023/01/15
    "%m-%d-%Y",      # 01-15-2023
    "%d-%m-%Y",      # 15-01-2023
    "%Y%m%d",        # 20230115
    "%m/%d/%y",      # 01/15/23
    "%d/%m/%y",      # 15/01/23
    "%B %d, %Y",     # January 15, 2023
    "%b %d, %Y",     # Jan 15, 2023
    "%d %B %Y",      # 15 January 2023
    "%d %b %Y"       # 15 Jan 2023
  )
  
  for (fmt in formats) {
    tryCatch({
      parsed_dates <- as.Date(date_strings, format = fmt)
      if (sum(!is.na(parsed_dates)) / length(parsed_dates) > 0.8) {  # At least 80% success
        message("Successfully parsed dates using format: ", fmt)
        message("Sample parsed dates: ")
        print(head(parsed_dates, 5))
        return(parsed_dates)
      }
    }, error = function(e) NULL)
  }
  
  # If no format worked, try automatic parsing
  tryCatch({
    parsed_dates <- as.Date(date_strings)
    message("Successfully parsed dates using automatic detection")
    return(parsed_dates)
  }, error = function(e) {
    warning("Could not parse dates with any known format. Using row indices instead.")
    return(seq_along(date_strings))
  })
}

#' Analyze Factor Exposure
#'
#' This function performs factor analysis on asset returns by running linear
#' regressions of each asset's returns against a set of factors.
#'
#' @param returns A data frame or matrix of asset returns where each column
#'   represents a different asset and each row represents a time period
#' @param factors A data frame or matrix of factor returns with the same number
#'   of rows as the returns data
#'
#' @return A matrix where each row represents an asset and each column represents
#'   a factor exposure (regression coefficient)
#'
#' @details The function runs linear regressions of the form:
#'   Asset_Return = α + β₁*Factor₁ + β₂*Factor₂ + ... + ε
#'   
#'   Returns only the β coefficients (factor exposures), excluding the intercept.
#'
#' @examples
#' \dontrun{
#' # Sample data
#' returns <- data.frame(
#'   Equity = rnorm(100),
#'   Bonds = rnorm(100)
#' )
#' factors <- data.frame(
#'   Market = rnorm(100),
#'   Value = rnorm(100)
#' )
#' 
#' # Analyze factor exposures
#' exposures <- analyze_factor_exposure(returns, factors)
#' print(exposures)
#' }
#'
#' @export
analyze_factor_exposure <- function(returns, factors) {
  # Simple factor regression for each asset
  factor_exposures <- list()
  
  for (i in 1:ncol(returns)) {
    asset_name <- colnames(returns)[i]
    lm_result <- lm(returns[,i] ~ factors)
    factor_exposures[[asset_name]] <- coef(lm_result)[-1]  # Exclude intercept
  }
  
  return(do.call(rbind, factor_exposures))
}

#' Decompose Risk Contributions into Factor Components
#'
#' This function decomposes portfolio risk contributions into factor-based components
#' using factor exposures and factor covariance matrix.
#'
#' @param returns A data frame or matrix of asset returns
#' @param factors A data frame or matrix of factor returns
#' @param weights A numeric vector of portfolio weights
#'
#' @return A vector of factor contributions to total portfolio risk (as percentages)
#'
#' @details The function:
#'   1. Calculates factor exposures for each asset
#'   2. Computes portfolio-level factor exposures
#'   3. Calculates each factor's contribution to total portfolio risk
#'
#' @examples
#' \dontrun{
#' returns <- data.frame(Equity = rnorm(100), Bonds = rnorm(100))
#' factors <- data.frame(Market = rnorm(100), Interest_Rate = rnorm(100))
#' weights <- c(0.6, 0.4)
#' 
#' factor_contrib <- decompose_risk_contributions(returns, factors, weights)
#' print(factor_contrib)
#' }
#'
#' @export
decompose_risk_contributions <- function(returns, factors, weights) {
  # Get factor exposures
  factor_exposures <- analyze_factor_exposure(returns, factors)
  
  # Factor covariance matrix
  factor_cov <- cov(factors)
  
  # Calculate portfolio factor exposures
  portfolio_factor_exposure <- t(weights) %*% factor_exposures
  
  # Calculate factor contribution to total risk
  factor_risk_contrib <- portfolio_factor_exposure %*% factor_cov %*% t(portfolio_factor_exposure)
  
  # Calculate each factor's percentage contribution
  factor_pctr <- (portfolio_factor_exposure * (factor_cov %*% t(portfolio_factor_exposure)))
  factor_pctr_pct <- factor_pctr / sum(factor_pctr) * 100
  
  return(factor_pctr_pct)
}

#' Calculate Equal Risk Contribution (ERC) Portfolio Weights
#'
#' This function finds optimal portfolio weights such that each asset contributes
#' equally to the total portfolio risk. It uses an iterative optimization approach
#' to achieve equal risk contribution.
#'
#' @param returns A data frame or matrix of asset returns
#' @param max_iter Integer specifying the maximum number of iterations (default: 100)
#' @param tolerance Numeric tolerance for convergence (default: 1e-8)
#'
#' @return A numeric vector of optimal weights for the Equal Risk Contribution portfolio
#'
#' @details The ERC portfolio is constructed so that each asset contributes equally
#'   to the total portfolio risk. The algorithm iteratively adjusts weights until
#'   convergence is achieved. This approach is useful for risk budgeting and
#'   diversification.
#'
#' @examples
#' \dontrun{
#' returns <- data.frame(
#'   Equity = rnorm(100, 0.01, 0.05),
#'   Bonds = rnorm(100, 0.005, 0.02),
#'   Commodities = rnorm(100, 0.008, 0.06)
#' )
#' 
#' # Calculate ERC weights
#' erc_weights <- calculate_erc_weights(returns)
#' print(erc_weights)
#' 
#' # Verify equal risk contributions
#' erc_risk <- calculate_risk_contributions(returns, erc_weights)
#' print(erc_risk$result)
#' }
#'
#' @export
calculate_erc_weights <- function(returns, max_iter = 100, tolerance = 1e-8) {
  n <- ncol(returns)
  weights <- rep(1/n, n)
  cov_matrix <- cov(returns)
  
  for (iter in 1:max_iter) {
    risk_contrib <- numeric(n)
    portfolio_vol <- sqrt(t(weights) %*% cov_matrix %*% weights)
    
    for (i in 1:n) {
      # Marginal contribution to risk
      mctr <- (cov_matrix %*% weights)[i] / portfolio_vol
      risk_contrib[i] <- weights[i] * mctr
    }
    
    # Target contribution is equal for all (1/n)
    target_contrib <- 1/n
    
    # Update weights
    new_weights <- weights * (target_contrib / risk_contrib)
    # Normalize to sum to 1
    new_weights <- new_weights / sum(new_weights)
    
    # Check convergence
    if (sqrt(sum((new_weights - weights)^2)) < tolerance) {
      weights <- new_weights
      break
    }
    
    weights <- new_weights
  }
  
  return(weights)
}

#' Calculate Time-Varying Equal Risk Contribution (ERC) Weights
#'
#' This function calculates ERC weights over time using rolling windows, allowing
#' for dynamic risk budgeting that adapts to changing market conditions.
#'
#' @param returns A data frame or matrix of asset returns
#' @param window_size Integer specifying the rolling window size for ERC calculation (default: 12)
#' @param step_size Integer specifying the step size between calculations (default: 1)
#'
#' @return A matrix of time-varying ERC weights where each row represents a time period
#'   and each column represents an asset
#'
#' @details The function:
#'   - Calculates ERC weights using rolling windows
#'   - Forward fills missing values for periods before first calculation
#'   - Uses equal weights for initial periods before sufficient data
#'
#' @examples
#' \dontrun{
#' returns <- data.frame(
#'   Equity = rnorm(60, 0.01, 0.05),
#'   Bonds = rnorm(60, 0.005, 0.02)
#' )
#' 
#' # Calculate time-varying ERC weights
#' erc_weights_tv <- calculate_time_varying_erc(returns, window_size = 12, step_size = 3)
#' head(erc_weights_tv)
#' }
#'
#' @export
calculate_time_varying_erc <- function(returns, window_size = 12, step_size = 1) {
  num_periods <- nrow(returns)
  num_assets <- ncol(returns)
  
  # Prepare storage for results
  erc_weights_matrix <- matrix(NA, nrow = num_periods, ncol = num_assets)
  colnames(erc_weights_matrix) <- colnames(returns)
  
  for (end_idx in seq(window_size, num_periods, by = step_size)) {
    start_idx <- end_idx - window_size + 1
    window_returns <- returns[start_idx:end_idx, ]
    
    # Calculate ERC weights for this window
    window_erc_weights <- calculate_erc_weights(window_returns)
    
    # Store weights (assign to end of period)
    erc_weights_matrix[end_idx, ] <- window_erc_weights
  }
  
  # Forward fill NA values (use last available weights)
  for (i in 1:num_assets) {
    erc_weights_matrix[,i] <- na.locf(erc_weights_matrix[,i], na.rm = FALSE)
  }
  
  # For the initial periods before first calculation, use equal weights
  na_rows <- which(is.na(erc_weights_matrix[,1]))
  if (length(na_rows) > 0) {
    erc_weights_matrix[na_rows, ] <- matrix(rep(1/num_assets, length(na_rows) * num_assets),
                                            ncol = num_assets)
  }
  
  return(erc_weights_matrix)
}

#' Main Analysis Script for Time-Varying Risk Contributions
#'
#' This section contains the main analysis workflow that demonstrates comprehensive
#' time-varying risk analysis including portfolio vs benchmark comparisons, active
#' weight analysis, and Equal Risk Contribution portfolio construction.
#'
#' @section File Requirements:
#' The script expects the following files in the trading directory:
#' \itemize{
#'   \item SAHS Asset Returns.csv: Asset returns data with Date column
#'   \item SAHS Portfolio Weights.csv: Portfolio weights data over time
#'   \item SAHS Benchmark Weights.csv: Benchmark weights data over time
#' }
#'
#' @section Analysis Components:
#' \enumerate{
#'   \item Date parsing and data validation
#'   \item Active weight calculation (Portfolio - Benchmark)
#'   \item Time-varying risk contribution analysis
#'   \item Portfolio weight evolution visualization
#'   \item Equal Risk Contribution portfolio analysis
#'   \item Comprehensive summary reporting
#' }
#'
#' @examples
#' \dontrun{
#' # The main analysis runs automatically when the script is sourced
#' # Ensure the required CSV files are in the expected location
#' source("time_varying_marginal_contribution_to_total_risk.R")
#' }

## Data Inputs and Main Analysis
library(tidyverse)
library(fs)

# File paths using here() for better path management
returns_path <- here::here("~", "Desktop", "TRADING", "SAHS Asset Returns.csv")
portfolio_weights_path <- here::here("~", "Desktop", "TRADING", "SAHS Portfolio Weights.csv")
benchmark_weights_path <- here::here("~", "Desktop", "TRADING", "SAHS Benchmark Weights.csv")

# Read the three input files
returns_data <- returns_path %>% read_trading_data(keep_date = TRUE)
portfolio_weights_data <- portfolio_weights_path %>% read_trading_data(keep_date = TRUE)
benchmark_weights_data <- benchmark_weights_path %>% read_trading_data(keep_date = TRUE)

# Extract and convert dates to proper Date class
# First, let's see what the date format looks like
message("Sample date values from file:")
print(head(returns_data$Date, 10))
message("Date class before conversion: ", class(returns_data$Date))

# Try different common date formats
dates_raw <- returns_data$Date

# Parse the dates
dates <- parse_dates(dates_raw)

# Verify that dates are properly formatted
if (inherits(dates, "Date")) {
  message("Date range: ", min(dates, na.rm = TRUE), " to ", max(dates, na.rm = TRUE))
  message("Date class: ", class(dates))
  message("Number of NA dates: ", sum(is.na(dates)))
} else {
  message("Warning: Using sequential numbers instead of dates")
  message("Range: ", min(dates), " to ", max(dates))
}

# Convert to data frames without dates for calculations
returns_matrix <- returns_data %>% select(-Date)
portfolio_weights_matrix <- portfolio_weights_data %>% select(-Date)
benchmark_weights_matrix <- benchmark_weights_data %>% select(-Date)

# Convert to data frames
returns <- as.data.frame(returns_matrix)
portfolio_weights <- as.data.frame(portfolio_weights_matrix)
benchmark_weights <- as.data.frame(benchmark_weights_matrix)

# Ensure all matrices have consistent column names
colnames(portfolio_weights) <- colnames(returns)
colnames(benchmark_weights) <- colnames(returns)

# Calculate active weights (Portfolio - Benchmark)
active_weights <- calculate_active_weights(portfolio_weights, benchmark_weights)
active_weights_df <- as.data.frame(active_weights)

# Display summary statistics for active weights
message("Active Weights Summary:")
message("======================")
print(summary(active_weights_df))

# Plot active weights evolution
plot_active_weights(active_weights_df, dates)

# Example 1: Using fixed weights (average over the entire period)
avg_active_weights <- colMeans(active_weights)
risk_info_avg <- calculate_risk_contributions(returns, avg_active_weights)

# Print results for fixed weights
message("\nPortfolio Volatility with Average Active Weights: ", risk_info_avg$portfolio_volatility)
print(risk_info_avg$result)

# Example 2: Using time-varying active weights with rolling windows
window_size <- 12  # 12-month rolling window
step_size <- 3     # Calculate every 3 months

time_varying_results <- calculate_time_varying_risk_contributions(
  returns, 
  active_weights,
  window_size = window_size, 
  step_size = step_size
)

# Plot the evolution of risk contributions over time
plot_time_varying_risk_contributions(time_varying_results, dates)

# Plot the evolution of portfolio weights over time
portfolio_weights_long <- reshape2::melt(
  cbind(Date = dates, portfolio_weights),
  id.vars = "Date",
  variable.name = "Asset",
  value.name = "Weight"
)

# Handle Date column based on whether we have actual dates or indices
if (inherits(dates, "Date")) {
  portfolio_weights_long$Date <- as.Date(portfolio_weights_long$Date, origin = "1970-01-01")
  x_label <- "Date"
  use_date_scale <- TRUE
} else {
  portfolio_weights_long$Date <- as.numeric(portfolio_weights_long$Date)
  x_label <- "Time Period"
  use_date_scale <- FALSE
}

p3 <- ggplot(portfolio_weights_long, aes(x = Date, y = Weight, color = Asset)) +
  geom_line(linewidth = 1) +
  labs(title = "Evolution of Portfolio Weights Over Time",
       x = x_label,
       y = "Portfolio Weight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        axis.title = element_text(family = "Times New Roman"),
        plot.title = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"),
        legend.title = element_text(family = "Times New Roman"))

# Only add date scale if we have valid dates
if (use_date_scale && all(!is.na(portfolio_weights_long$Date))) {
  p3 <- p3 + scale_x_date(date_labels = "%b-%y", date_breaks = "6 months")
}

print(p3)

# Plot the evolution of benchmark weights over time
benchmark_weights_long <- reshape2::melt(
  cbind(Date = dates, benchmark_weights),
  id.vars = "Date",
  variable.name = "Asset",
  value.name = "Weight"
)

# Handle Date column based on whether we have actual dates or indices
if (inherits(dates, "Date")) {
  benchmark_weights_long$Date <- as.Date(benchmark_weights_long$Date, origin = "1970-01-01")
  x_label <- "Date"
  use_date_scale <- TRUE
} else {
  benchmark_weights_long$Date <- as.numeric(benchmark_weights_long$Date)
  x_label <- "Time Period"
  use_date_scale <- FALSE
}

p4 <- ggplot(benchmark_weights_long, aes(x = Date, y = Weight, color = Asset)) +
  geom_line(linewidth = 1) +
  labs(title = "Evolution of Benchmark Weights Over Time",
       x = x_label,
       y = "Benchmark Weight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        axis.title = element_text(family = "Times New Roman"),
        plot.title = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"),
        legend.title = element_text(family = "Times New Roman"))

# Only add date scale if we have valid dates
if (use_date_scale && all(!is.na(benchmark_weights_long$Date))) {
  p4 <- p4 + scale_x_date(date_labels = "%b-%y", date_breaks = "6 months")
}

print(p4)

# Example of calculating ERC weights
erc_weights <- calculate_erc_weights(returns)
message("\nEqual Risk Contribution Weights:")
print(data.frame(Asset = colnames(returns), ERC_Weight = erc_weights))

# Calculate risk contributions with ERC weights
erc_risk_info <- calculate_risk_contributions(returns, erc_weights)
message("\nERC Portfolio - Risk Contributions:")
print(erc_risk_info$result)

# Calculate time-varying ERC weights
time_varying_erc_weights <- calculate_time_varying_erc(returns, window_size = 12, step_size = 3)

# Calculate risk contributions with time-varying ERC weights
erc_time_varying_results <- calculate_time_varying_risk_contributions(
  returns,
  time_varying_erc_weights,
  window_size = 12,
  step_size = 3
)

# Plot the evolution of risk contributions with ERC weights
plot_time_varying_risk_contributions(erc_time_varying_results, dates)

# Summary Report
message("\n", paste(rep("=", 50), collapse=""))
message("SUMMARY REPORT")
message(paste(rep("=", 50), collapse=""))

# Diagnostic checks for weight data
message("0. DIAGNOSTIC CHECKS:")
message("   Portfolio Weights - Standard Deviations:")
portfolio_sds <- sapply(portfolio_weights, sd)
print(round(portfolio_sds, 6))

message("   Benchmark Weights - Standard Deviations:")
benchmark_sds <- sapply(benchmark_weights, sd)
print(round(benchmark_sds, 6))

message("   Portfolio Weights - Sample Data (first 5 rows):")
print(head(portfolio_weights, 5))

message("   Benchmark Weights - Sample Data (first 5 rows):")
print(head(benchmark_weights, 5))

message("1. Active Weights Range:")
print(sapply(active_weights_df, function(x) paste("Min:", round(min(x), 4), "Max:", round(max(x), 4))))

message("2. Average Active Weights:")
print(round(colMeans(active_weights_df), 4))

message("3. Active Weight Standard Deviations:")
print(round(sapply(active_weights_df, sd), 4))

message("4. Portfolio vs Benchmark Weight Correlations:")
# Enhanced correlation calculation with error handling
correlations <- sapply(1:ncol(returns), function(i) {
  portfolio_sd <- sd(portfolio_weights[,i])
  benchmark_sd <- sd(benchmark_weights[,i])
  
  if (portfolio_sd == 0 && benchmark_sd == 0) {
    # Both are constant - check if they're the same constant
    if (all(portfolio_weights[,i] == benchmark_weights[,i])) {
      return("IDENTICAL_CONSTANT")
    } else {
      return("DIFFERENT_CONSTANTS")
    }
  } else if (portfolio_sd == 0 || benchmark_sd == 0) {
    return("ONE_CONSTANT")
  } else {
    return(cor(portfolio_weights[,i], benchmark_weights[,i]))
  }
})
names(correlations) <- colnames(returns)
print(correlations)