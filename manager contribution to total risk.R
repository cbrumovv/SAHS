#' Portfolio Risk Contribution Analysis
#'
#' This script calculates the contribution of each manager to total portfolio risk.
#' It supports both static weights and time-series of manager weights.
#'
#' @author Craigile J. Brumfield, CFA
#' @date Created: 2025-04-13, Modified: 2025-07-17
#' @note Added time-series support for manager weights
#' @keywords portfolio risk contribution manager analysis
#' @import ggplot2 reshape2 zoo xts tidyverse fs

# Install required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("zoo")) install.packages("zoo")
if (!require("xts")) install.packages("xts")

#' Calculate Risk Contributions with Static Weights
#'
#' This function calculates the contribution of each manager to total portfolio risk
#' using static (fixed) weights. It computes the Percentage Contribution to Risk (PCTR)
#' for each manager based on their weights and the covariance matrix of returns.
#'
#' @param returns A data frame or matrix of manager returns where each column represents
#'   a different manager and each row represents a time period
#' @param weights A numeric vector of manager weights that sum to 1
#'
#' @return A list containing:
#'   \item{result}{A data frame with columns Manager, Weight, PCTR, and PCTR_Percent}
#'   \item{portfolio_volatility}{The portfolio's standard deviation}
#'   \item{covariance_matrix}{The covariance matrix of manager returns}
#'
#' @details The function calculates:
#'   - Marginal Contribution to Risk (MCTR): Partial derivative of portfolio risk with respect to weight
#'   - Percentage Contribution to Risk (PCTR): The percentage of total portfolio risk attributable to each manager
#'
#' @examples
#' \dontrun{
#' # Sample data
#' returns <- data.frame(
#'   Manager_A = rnorm(100, 0.01, 0.05),
#'   Manager_B = rnorm(100, 0.008, 0.04)
#' )
#' weights <- c(0.6, 0.4)
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
  
  # Calculate each manager's contribution to risk
  contributions <- numeric(length(weights))
  manager_names <- colnames(returns)
  
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
    Manager = manager_names,
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

#' Calculate Risk Contributions with Time-Varying Weights
#'
#' This function calculates the contribution of each manager to total portfolio risk
#' using time-varying weights over rolling windows. It applies a rolling window approach
#' to analyze how risk contributions change over time.
#'
#' @param returns A data frame or matrix of manager returns where each column represents
#'   a different manager and each row represents a time period
#' @param weight_matrix A matrix or data frame of manager weights over time, where each
#'   row represents a time period and each column represents a manager
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
#'   Manager_A = rnorm(50, 0.01, 0.05),
#'   Manager_B = rnorm(50, 0.008, 0.04)
#' )
#' weights <- data.frame(
#'   Manager_A = runif(50, 0.4, 0.8),
#'   Manager_B = runif(50, 0.2, 0.6)
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
  num_managers <- ncol(returns)
  
  # Prepare storage for results
  all_results <- list()
  period_indices <- seq(window_size, num_periods, by = step_size)
  
  for (end_idx in period_indices) {
    start_idx <- end_idx - window_size + 1
    period_label <- paste0("Period_", start_idx, "_to_", end_idx)
    
    # Extract returns and weights for this window
    window_returns <- returns[start_idx:end_idx, ]
    
    # Use average weights over the window for stability
    window_weights <- colMeans(weight_matrix[start_idx:end_idx, ])
    
    # Alternative: Use end-of-period weights
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

#' Detect Weight Type
#'
#' This function determines whether the provided weights are static (constant over time)
#' or time-varying (change over time).
#'
#' @param weights Either a numeric vector (static weights) or a matrix/data frame
#'   (time-varying weights) of manager weights
#'
#' @return A character string: "static" for constant weights or "time_varying" for
#'   weights that change over time
#'
#' @details The function uses a small numerical threshold (1e-8) to account for
#'   floating-point precision when determining if weights are effectively constant.
#'
#' @examples
#' \dontrun{
#' # Static weights
#' static_weights <- c(0.6, 0.4)
#' detect_weight_type(static_weights)  # Returns "static"
#' 
#' # Time-varying weights
#' time_weights <- matrix(runif(20), nrow = 10, ncol = 2)
#' detect_weight_type(time_weights)  # Returns "time_varying"
#' }
#'
#' @export
detect_weight_type <- function(weights) {
  if (is.vector(weights) || (is.matrix(weights) && nrow(weights) == 1)) {
    return("static")
  } else if (is.matrix(weights) || is.data.frame(weights)) {
    # Check if all rows are identical (static weights stored as matrix)
    if (nrow(weights) > 1) {
      # Check if there's variation in weights over time
      weight_sds <- apply(weights, 2, sd)
      if (all(weight_sds < 1e-8)) {  # Very small threshold for numerical precision
        return("static")
      } else {
        return("time_varying")
      }
    } else {
      return("static")
    }
  } else {
    stop("Weights must be either a vector (static) or matrix/data.frame (time-varying)")
  }
}

#' Universal Risk Contribution Calculator
#'
#' This enhanced function automatically detects whether weights are static or time-varying
#' and applies the appropriate calculation method. It serves as a universal interface
#' for both types of risk contribution analysis.
#'
#' @param returns A data frame or matrix of manager returns where each column represents
#'   a different manager and each row represents a time period
#' @param weights Either a numeric vector (static weights) or a matrix/data frame
#'   (time-varying weights) of manager weights
#' @param window_size Integer specifying the rolling window size for time-varying analysis (default: 12)
#' @param step_size Integer specifying the step size for time-varying analysis (default: 1)
#' @param dates Optional vector of dates corresponding to the time periods in returns
#'
#' @return For static weights: A list with risk contribution results
#'   For time-varying weights: A list of results for each time window
#'
#' @details The function automatically:
#'   - Detects the type of weights provided
#'   - Applies the appropriate calculation method
#'   - Provides informative messages about the analysis being performed
#'   - Incorporates dates if provided for time-varying analysis
#'
#' @examples
#' \dontrun{
#' # Works with both static and time-varying weights
#' returns <- data.frame(
#'   Manager_A = rnorm(100, 0.01, 0.05),
#'   Manager_B = rnorm(100, 0.008, 0.04)
#' )
#' 
#' # Static weights example
#' static_weights <- c(0.6, 0.4)
#' result1 <- calculate_universal_risk_contributions(returns, static_weights)
#' 
#' # Time-varying weights example
#' time_weights <- matrix(runif(200), nrow = 100, ncol = 2)
#' time_weights <- time_weights / rowSums(time_weights)
#' result2 <- calculate_universal_risk_contributions(returns, time_weights)
#' }
#'
#' @export
calculate_universal_risk_contributions <- function(returns, weights, window_size = 12, step_size = 1, dates = NULL) {
  weight_type <- detect_weight_type(weights)
  
  if (weight_type == "static") {
    message("Detected static weights - using traditional calculation")
    if (is.matrix(weights) || is.data.frame(weights)) {
      # Convert to vector by taking first row or column means
      weights <- as.numeric(colMeans(weights))
    }
    return(calculate_risk_contributions(returns, weights))
    
  } else {
    message("Detected time-varying weights - using rolling window calculation")
    message(paste("Window size:", window_size, "periods"))
    message(paste("Step size:", step_size, "periods"))
    
    results <- calculate_time_varying_risk_contributions(returns, weights, window_size, step_size)
    
    # Add dates if provided
    if (!is.null(dates)) {
      for (i in seq_along(results)) {
        results[[i]]$period_end_date <- dates[results[[i]]$period_end]
      }
    }
    
    return(results)
  }
}

#' Plot Risk Contributions
#'
#' This function creates visualizations of risk contributions, automatically handling
#' both static and time-varying weight scenarios.
#'
#' @param risk_contributions Output from calculate_universal_risk_contributions or
#'   related functions
#' @param dates Optional vector of dates for time-varying visualizations
#'
#' @return For static weights: A ggplot bar chart
#'   For time-varying weights: Multiple plots showing evolution over time
#'
#' @details The function creates different visualizations based on the input:
#'   - Static weights: Bar chart of percentage contributions
#'   - Time-varying weights: Stacked bar chart and line plot showing evolution
#'
#' @examples
#' \dontrun{
#' # After calculating risk contributions
#' risk_results <- calculate_universal_risk_contributions(returns, weights)
#' plot_risk_contributions(risk_results)
#' }
#'
#' @export
plot_risk_contributions <- function(risk_contributions, dates = NULL) {
  # Check if this is static (single result) or time-varying (list of results)
  if (is.list(risk_contributions) && "result" %in% names(risk_contributions)) {
    # Static results
    ggplot(risk_contributions$result, aes(x = reorder(Manager, PCTR_Percent), y = PCTR_Percent, fill = Manager)) +
      geom_bar(stat = "identity") +
      labs(title = "Percentage Contribution to Portfolio Risk by Manager",
           x = "Manager",
           y = "Percentage of Total Risk (%)") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
            axis.text.y = element_text(family = "Times New Roman"),
            axis.title.x = element_text(family = "Times New Roman"),
            axis.title.y = element_text(family = "Times New Roman"),
            plot.title = element_text(family = "Times New Roman"),
            legend.text = element_text(family = "Times New Roman"),
            legend.title = element_text(family = "Times New Roman"))
    
  } else if (is.list(risk_contributions) && length(risk_contributions) > 1) {
    # Time-varying results
    plot_time_varying_risk_contributions(risk_contributions, dates)
  } else {
    stop("Invalid risk_contributions format")
  }
}

#' Plot Time-Varying Risk Contributions
#'
#' This function creates specialized visualizations for time-varying risk contributions,
#' including both stacked bar charts and line plots to show the evolution over time.
#'
#' @param all_results List of risk contribution results from time-varying analysis
#' @param dates Optional vector of dates corresponding to the time periods
#'
#' @return A list containing:
#'   \item{stacked_plot}{A ggplot stacked bar chart showing contributions over time}
#'   \item{line_plot}{A ggplot line chart showing the evolution of each manager's contribution}
#'
#' @details The function handles both Date objects and numeric time periods,
#'   automatically formatting axes appropriately.
#'
#' @examples
#' \dontrun{
#' # After time-varying risk contribution calculation
#' time_results <- calculate_time_varying_risk_contributions(returns, weights)
#' plots <- plot_time_varying_risk_contributions(time_results, dates)
#' }
#'
#' @export
plot_time_varying_risk_contributions <- function(all_results, dates = NULL) {
  # Combine all period results
  all_data <- do.call(rbind, lapply(all_results, function(x) x$result))
  
  # Handle dates
  if (!is.null(dates)) {
    # Map period indices to actual dates
    all_data$period_end_idx <- as.numeric(gsub(".*_.*_(.*)", "\\1", all_data$Period))
    all_data$Date <- dates[all_data$period_end_idx]
    
    if (inherits(dates, "Date")) {
      all_data$Date <- as.Date(all_data$Date, origin = "1970-01-01")
      x_label <- "Date"
      use_date_scale <- TRUE
    } else {
      all_data$Date <- as.numeric(all_data$Date)
      x_label <- "Time Period"
      use_date_scale <- FALSE
    }
  } else {
    # Use period labels if no dates provided
    all_data$Date <- all_data$Period
    x_label <- "Period"
    use_date_scale <- FALSE
  }
  
  # Sort the data
  all_data <- all_data[order(all_data$Date), ]
  
  # Stacked bar chart showing risk contributions over time
  p1 <- ggplot(all_data, aes(x = Date, y = PCTR_Percent, fill = Manager, group = Manager)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Time-Varying Percentage Contribution to Portfolio Risk",
         x = x_label,
         y = "Percentage of Total Risk (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, family = "Times New Roman"),
          axis.text.y = element_text(family = "Times New Roman"),
          axis.title.x = element_text(family = "Times New Roman"),
          axis.title.y = element_text(family = "Times New Roman"),
          plot.title = element_text(family = "Times New Roman"),
          legend.text = element_text(family = "Times New Roman"),
          legend.title = element_text(family = "Times New Roman"),
          legend.position = "bottom")
  
  # Add date scale if appropriate
  if (use_date_scale && !is.null(dates) && all(!is.na(all_data$Date))) {
    p1 <- p1 + scale_x_date(date_labels = "%b-%y", date_breaks = "6 months")
  }
  
  print(p1)
  
  # Line plot showing evolution of each manager's contribution
  p2 <- ggplot(all_data, aes(x = Date, y = PCTR_Percent, color = Manager, group = Manager)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = "Evolution of Risk Contribution by Manager",
         x = x_label,
         y = "Percentage of Total Risk (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, family = "Times New Roman"),
          axis.text.y = element_text(family = "Times New Roman"),
          axis.title.x = element_text(family = "Times New Roman"),
          axis.title.y = element_text(family = "Times New Roman"),
          plot.title = element_text(family = "Times New Roman"),
          legend.text = element_text(family = "Times New Roman"),
          legend.title = element_text(family = "Times New Roman"),
          legend.position = "bottom")
  
  # Add date scale if appropriate  
  if (use_date_scale && !is.null(dates) && all(!is.na(all_data$Date))) {
    p2 <- p2 + scale_x_date(date_labels = "%b-%y", date_breaks = "6 months")
  }
  
  print(p2)
  
  return(list(stacked_plot = p1, line_plot = p2))
}

#' Plot Manager Weights
#'
#' This function creates visualizations of manager weights, handling both static
#' and time-varying weight scenarios.
#'
#' @param weight_matrix Either a numeric vector (static weights) or a matrix/data frame
#'   (time-varying weights) of manager weights
#' @param dates Optional vector of dates corresponding to the time periods
#'
#' @return A ggplot object showing weight distribution or evolution
#'
#' @details For static weights, creates a bar chart. For time-varying weights,
#'   creates a line plot showing the evolution of each manager's weight over time.
#'
#' @examples
#' \dontrun{
#' # Static weights
#' static_weights <- c(Manager_A = 0.6, Manager_B = 0.4)
#' plot_manager_weights(static_weights)
#' 
#' # Time-varying weights
#' time_weights <- matrix(runif(200), nrow = 100, ncol = 2)
#' colnames(time_weights) <- c("Manager_A", "Manager_B")
#' plot_manager_weights(time_weights, dates = 1:100)
#' }
#'
#' @export
plot_manager_weights <- function(weight_matrix, dates = NULL) {
  if (is.vector(weight_matrix)) {
    message("Static weights detected - creating single bar chart")
    weight_df <- data.frame(
      Manager = names(weight_matrix),
      Weight = weight_matrix
    )
    
    ggplot(weight_df, aes(x = reorder(Manager, Weight), y = Weight, fill = Manager)) +
      geom_bar(stat = "identity") +
      labs(title = "Manager Weights",
           x = "Manager",
           y = "Weight") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
            axis.text.y = element_text(family = "Times New Roman"),
            axis.title.x = element_text(family = "Times New Roman"),
            axis.title.y = element_text(family = "Times New Roman"),
            plot.title = element_text(family = "Times New Roman"),
            legend.text = element_text(family = "Times New Roman"),
            legend.title = element_text(family = "Times New Roman"))
    
  } else {
    # Time-varying weights
    if (is.null(dates)) {
      dates <- 1:nrow(weight_matrix)
    }
    
    # Convert to long format
    weights_long <- reshape2::melt(
      cbind(Date = dates, weight_matrix),
      id.vars = "Date",
      variable.name = "Manager",
      value.name = "Weight"
    )
    
    # Handle date formatting
    if (inherits(dates, "Date")) {
      weights_long$Date <- as.Date(weights_long$Date, origin = "1970-01-01")
      x_label <- "Date"
      use_date_scale <- TRUE
    } else {
      weights_long$Date <- as.numeric(weights_long$Date)
      x_label <- "Time Period"
      use_date_scale <- FALSE
    }
    
    p <- ggplot(weights_long, aes(x = Date, y = Weight, color = Manager)) +
      geom_line(linewidth = 1) +
      labs(title = "Evolution of Manager Weights Over Time",
           x = x_label,
           y = "Manager Weight") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
            axis.text.y = element_text(family = "Times New Roman"),
            axis.title.x = element_text(family = "Times New Roman"),
            axis.title.y = element_text(family = "Times New Roman"),
            plot.title = element_text(family = "Times New Roman"),
            legend.text = element_text(family = "Times New Roman"),
            legend.title = element_text(family = "Times New Roman"),
            legend.position = "bottom")
    
    # Add date scale if appropriate
    if (use_date_scale && all(!is.na(weights_long$Date))) {
      p <- p + scale_x_date(date_labels = "%b-%y", date_breaks = "6 months")
    }
    
    print(p)
    return(p)
  }
}

#' Calculate Equal Risk Contribution (ERC) Portfolio Weights
#'
#' This function calculates optimal portfolio weights such that each manager contributes
#' equally to the total portfolio risk. It uses an iterative optimization approach
#' to find the Equal Risk Contribution portfolio.
#'
#' @param returns A data frame or matrix of manager returns where each column represents
#'   a different manager and each row represents a time period
#' @param max_iter Integer specifying the maximum number of iterations for optimization (default: 100)
#' @param tolerance Numeric tolerance for convergence (default: 1e-8)
#'
#' @return A numeric vector of optimal weights for the Equal Risk Contribution portfolio
#'
#' @details The ERC portfolio is constructed so that each manager contributes equally
#'   to the total portfolio risk. The algorithm iteratively adjusts weights until
#'   convergence is achieved within the specified tolerance.
#'
#' @examples
#' \dontrun{
#' returns <- data.frame(
#'   Manager_A = rnorm(100, 0.01, 0.05),
#'   Manager_B = rnorm(100, 0.008, 0.04),
#'   Manager_C = rnorm(100, 0.012, 0.06)
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

#' Main Analysis Script
#'
#' This section contains the main analysis workflow that demonstrates the usage
#' of all the functions defined above. It reads manager returns and weights data,
#' performs comprehensive risk analysis, and generates visualizations.
#'
#' @section File Requirements:
#' The script expects the following files:
#' \itemize{
#'   \item SAHS Mgr Rets.csv: Manager returns data with optional Date column
#'   \item SAHS Mgr Wts.csv: Manager weights data (static or time-varying)
#' }
#'
#' @section Analysis Steps:
#' \enumerate{
#'   \item Load and validate input data
#'   \item Detect weight type (static vs time-varying)
#'   \item Calculate risk contributions using appropriate method
#'   \item Generate visualizations
#'   \item Perform additional analysis including ERC portfolio
#' }
#'
#' @examples
#' \dontrun{
#' # The main analysis runs automatically when the script is sourced
#' # Ensure the required CSV files are in the expected location
#' source("manager_contribution_to_total_risk.R")
#' }

# Example usage with enhanced functionality
# -----------------------------------------

library(tidyverse)
library(fs) 

# File paths - using the existing file structure
mgr_rets_fn <- here::here("~", "Desktop", "TRADING", "SAHS Mgr Rets.csv")
mgr_wts_fn <- here::here("~", "Desktop", "TRADING", "SAHS Mgr Wts.csv")

# Read manager returns
if (file_exists(mgr_rets_fn)) {
  returns_data <- read.csv(mgr_rets_fn, header = TRUE)
  
  # Check if Date column exists and extract it
  if ("Date" %in% colnames(returns_data)) {
    dates_raw <- returns_data$Date
    returns_matrix <- returns_data %>% select(-Date)
    
    # Try to parse dates
    tryCatch({
      dates <- as.Date(dates_raw)
      if (sum(is.na(dates)) / length(dates) > 0.2) {  # More than 20% NA
        dates <- 1:length(dates_raw)  # Fall back to indices
        message("Could not parse dates reliably - using time indices")
      }
    }, error = function(e) {
      dates <- 1:length(dates_raw)
      message("Date parsing failed - using time indices")
    })
  } else {
    returns_matrix <- returns_data
    dates <- 1:nrow(returns_matrix)
    message("No Date column found - using time indices")
  }
} else {
  stop("Manager Returns: No file found!!!")
}

# Read manager weights (can be static or time-varying)
if (file_exists(mgr_wts_fn)) {
  weights_data <- read.csv(mgr_wts_fn, header = TRUE)
  
  # Remove Date column if it exists
  if ("Date" %in% colnames(weights_data)) {
    weights_matrix <- weights_data %>% select(-Date)
  } else {
    weights_matrix <- weights_data
  }
  
  # Ensure column names match between returns and weights
  colnames(weights_matrix) <- colnames(returns_matrix)
  
} else {
  stop("Manager Weights: No file found!!!")
}

# Convert to appropriate data structures
returns <- as.data.frame(returns_matrix)
weights <- as.data.frame(weights_matrix)

# Print summary information
message("=== DATA SUMMARY ===")
message("Number of time periods: ", nrow(returns))
message("Number of managers: ", ncol(returns))
message("Weight data dimensions: ", nrow(weights), " x ", ncol(weights))
message("Weight type: ", detect_weight_type(weights))

# Calculate risk contributions using the universal function
# This automatically detects whether weights are static or time-varying
risk_info <- calculate_universal_risk_contributions(
  returns = returns, 
  weights = weights,
  window_size = 12,  # 12-period rolling window for time-varying analysis
  step_size = 3,     # Calculate every 3 periods
  dates = dates
)

# Display results
if (detect_weight_type(weights) == "static") {
  message("\n=== STATIC WEIGHTS ANALYSIS ===")
  message("Portfolio Volatility: ", round(risk_info$portfolio_volatility, 4))
  print(risk_info$result)
  
  # Plot static results
  plot_risk_contributions(risk_info)
  
} else {
  message("\n=== TIME-VARYING WEIGHTS ANALYSIS ===")
  message("Number of rolling window calculations: ", length(risk_info))
  
  # Show sample results from first and last periods
  message("\nFirst period results:")
  print(risk_info[[1]]$result)
  
  message("\nLast period results:")
  print(risk_info[[length(risk_info)]]$result)
  
  # Plot time-varying results
  plot_risk_contributions(risk_info, dates)
}

# Plot manager weights evolution
message("\n=== MANAGER WEIGHTS VISUALIZATION ===")
plot_manager_weights(weights, dates)

# Display correlation matrix
correlation_matrix <- cor(returns)
message("\n=== MANAGER CORRELATION MATRIX ===")
print(round(correlation_matrix, 3))

# Additional analysis: Risk decomposition over time (for time-varying weights)
if (detect_weight_type(weights) == "time_varying") {
  message("\n=== RISK DECOMPOSITION SUMMARY ===")
  
  # Extract portfolio volatilities over time
  portfolio_vols <- sapply(risk_info, function(x) x$portfolio_volatility)
  
  message("Portfolio volatility range: ", 
          round(min(portfolio_vols), 4), " to ", round(max(portfolio_vols), 4))
  message("Average portfolio volatility: ", round(mean(portfolio_vols), 4))
  
  # Calculate average risk contributions across all periods
  all_pctr <- do.call(rbind, lapply(risk_info, function(x) x$result$PCTR_Percent))
  avg_pctr <- colMeans(all_pctr)
  names(avg_pctr) <- colnames(returns)
  
  message("\nAverage risk contributions across all periods:")
  print(round(sort(avg_pctr, decreasing = TRUE), 2))
}

# Risk budgeting section (Optional - requires optimization)
message("\n=== RISK BUDGETING ANALYSIS ===")

# Calculate Equal Risk Contribution weights
erc_weights <- calculate_erc_weights(returns)
names(erc_weights) <- colnames(returns)

message("Equal Risk Contribution Weights:")
print(round(erc_weights, 4))

# Calculate risk contributions with ERC weights
erc_risk_info <- calculate_risk_contributions(returns, erc_weights)
message("\nERC Portfolio - Risk Contributions:")
print(erc_risk_info$result)

message("\n=== ANALYSIS COMPLETE ===")