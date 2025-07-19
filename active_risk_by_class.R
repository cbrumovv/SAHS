## last edit:  16 july 2025 (roxygen2 formatted)
# create date:  14 april 2025

#' Asset Class Active Risk Contribution Analysis
#' 
#' @description
#' This script calculates contribution to active risk (tracking error) by asset class.
#' It handles both time-varying and constant portfolio weights and provides 
#' visualization of risk contributions and interaction effects.
#' 
#' @author Craigile J. Brumfield, CFA
#' @version 11.0

#' Install and Load Required Packages
#' 
#' @description Installs and loads necessary packages for the analysis
#' @details Packages include ggplot2, reshape2, dplyr, extrafont, tidyverse, and fs
#' 
#' @return NULL (loads packages)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("extrafont")) install.packages("extrafont")

# Load extrafont for Times New Roman
library(extrafont)

# Import fonts if not already done (only needs to be run once)
# font_import() # Uncomment and run this line once if Times New Roman is not available

#' Check Font Availability
#' 
#' @description Checks if Times New Roman font is available for plotting
#' @return character string with font name to use
if(!"Times New Roman" %in% fonts()) {
  message("Times New Roman font not found. Using default font instead.")
  message("To use Times New Roman, run: font_import() once, then restart R.")
  plot_font <- ""  # Use default font
} else {
  plot_font <- "Times New Roman"
  message("Using Times New Roman font for plots.")
}

#' Calculate Active Risk Contributions by Asset Class
#' 
#' @description 
#' Calculates the contribution to active risk (tracking error) by asset class for
#' either time-varying or constant portfolio weights.
#' 
#' @param portfolio_returns matrix of portfolio returns (time x assets)
#' @param benchmark_returns matrix of benchmark returns (time x assets) 
#' @param portfolio_weights matrix or vector of portfolio weights
#' @param benchmark_weights matrix or vector of benchmark weights
#' @param asset_class_mapping data.frame with columns 'Asset' and 'AssetClass'
#' 
#' @return list containing:
#' \itemize{
#'   \item asset_class_contributions: data.frame of risk contributions by asset class
#'   \item active_risk: scalar of total active risk (tracking error)
#'   \item active_cov_matrix: covariance matrix of active returns
#'   \item period_results: list of period-by-period results (time-varying case)
#'   \item all_contributions: combined contributions across periods (time-varying case)
#'   \item all_active_risks: vector of active risks by period (time-varying case)
#' }
#' 
#' @details
#' For time-varying weights, the function calculates risk contributions for each
#' period and aggregates results. For constant weights, it performs a single
#' calculation across all periods.
#' 
#' @examples
#' \dontrun{
#' results <- calculate_active_risk_contributions(
#'   portfolio_returns = portfolio_asset_returns,
#'   benchmark_returns = benchmark_asset_returns,
#'   portfolio_weights = portfolio_weights,
#'   benchmark_weights = benchmark_weights,
#'   asset_class_mapping = asset_class_mapping
#' )
#' }
#' 
#' @export
calculate_active_risk_contributions <- function(portfolio_returns, benchmark_returns,
                                                portfolio_weights, benchmark_weights,
                                                asset_class_mapping) {
  
  # Ensure inputs are properly formatted
  portfolio_returns <- as.matrix(portfolio_returns)
  benchmark_returns <- as.matrix(benchmark_returns)
  
  # Debug: Print dimensions
  message("Debug info:")
  message("Portfolio returns dimensions: ", paste(dim(portfolio_returns), collapse = " x "))
  message("Benchmark returns dimensions: ", paste(dim(benchmark_returns), collapse = " x "))
  message("Portfolio weights dimensions: ", paste(dim(portfolio_weights), collapse = " x "))
  message("Benchmark weights dimensions: ", paste(dim(benchmark_weights), collapse = " x "))
  
  # Check if weights are time-varying (matrices with multiple rows)
  time_varying_weights <- (is.matrix(portfolio_weights) && nrow(portfolio_weights) > 1) ||
    (is.matrix(benchmark_weights) && nrow(benchmark_weights) > 1)
  
  # Calculate active returns (portfolio returns - benchmark returns)
  active_returns <- portfolio_returns - benchmark_returns 
  
  # Calculate covariance matrix of active returns
  active_cov_matrix <- cov(active_returns)
  
  # Debug: Print covariance matrix dimensions
  message("Active covariance matrix dimensions: ", paste(dim(active_cov_matrix), collapse = " x "))
  message("Number of assets in covariance matrix: ", ncol(active_cov_matrix))
  message("Number of assets in weights: ", ncol(portfolio_weights))
  
  # Get unique asset classes
  asset_classes <- unique(asset_class_mapping$AssetClass)
  num_asset_classes <- length(asset_classes)
  
  # Number of time periods
  num_periods <- nrow(portfolio_returns)
  
  if (time_varying_weights) {
    # For time-varying weights: calculate for each period
    
    # Ensure weights are matrices with proper dimensions
    if (!is.matrix(portfolio_weights)) {
      portfolio_weights <- matrix(portfolio_weights, nrow = 1)
    }
    if (!is.matrix(benchmark_weights)) {
      benchmark_weights <- matrix(benchmark_weights, nrow = 1)
    }
    
    # If weights have fewer rows than returns, repeat the weights to match
    if (nrow(portfolio_weights) < num_periods) {
      portfolio_weights <-
        matrix(rep(portfolio_weights, length.out = num_periods * ncol(portfolio_weights)),
               nrow = num_periods)
    }
    if (nrow(benchmark_weights) < num_periods) {
      benchmark_weights <-
        matrix(rep(benchmark_weights, length.out = num_periods * ncol(benchmark_weights)),
               nrow = num_periods)
    }
    
    # Initialize storage for period-by-period results
    period_results <- list()
    
    # Calculate for each period
    for (period in 1:num_periods) {
      # Get weights for this period
      period_portfolio_weights <- portfolio_weights[period, ]
      period_benchmark_weights <- benchmark_weights[period, ]
      
      # Calculate active weights for this period
      period_active_weights <- period_portfolio_weights - period_benchmark_weights
      
      # Debug: Check dimensions before matrix multiplication
      message("Period ", period, " active weights length: ", length(period_active_weights))
      
      # FIXED: Ensure period_active_weights is a proper vector with correct length
      if (length(period_active_weights) != ncol(active_cov_matrix)) {
        stop("Dimension mismatch: active weights length (", length(period_active_weights), 
             ") does not match covariance matrix size (", ncol(active_cov_matrix), ")")
      }
      
      # Calculate total active risk for this period
      active_risk_variance <- as.numeric(t(period_active_weights) %*% active_cov_matrix %*% period_active_weights)
      active_risk <- sqrt(active_risk_variance)
      
      # Initialize results for this period
      period_asset_class_contributions <- data.frame(
        Period = period,
        AssetClass = asset_classes,
        ActiveWeight = numeric(num_asset_classes),
        RiskContribution = numeric(num_asset_classes),
        PercentContribution = numeric(num_asset_classes)
      )
      
      # Calculate asset class contributions for this period
      for (i in 1:num_asset_classes) {
        current_class <- asset_classes[i]
        
        # Get assets in this asset class
        class_assets <- asset_class_mapping$Asset[asset_class_mapping$AssetClass == current_class]
        class_indices <- match(class_assets, colnames(portfolio_returns))
        class_indices <- class_indices[!is.na(class_indices)]
        
        # Skip if no assets from this class in the portfolio
        if (length(class_indices) == 0) {
          next
        }
        
        # Calculate asset class active weight for this period
        class_active_weight <- sum(period_active_weights[class_indices])
        period_asset_class_contributions$ActiveWeight[i] <- class_active_weight
        
        # Create weight vector with only this asset class's weights
        class_only_weights <- numeric(length(period_active_weights))
        class_only_weights[class_indices] <- period_active_weights[class_indices]
        
        # Calculate asset class contribution to active risk
        class_contribution <- as.numeric(t(class_only_weights) %*% active_cov_matrix %*% period_active_weights)
        period_asset_class_contributions$RiskContribution[i] <- class_contribution
        
        # Calculate percentage contribution (avoid division by zero)
        if (active_risk_variance > 0) {
          period_asset_class_contributions$PercentContribution[i] <-
            class_contribution / active_risk_variance * 100
        } else {
          period_asset_class_contributions$PercentContribution[i] <- 0
        }
      }
      
      # Sort by absolute percentage contribution
      period_asset_class_contributions <- period_asset_class_contributions[
        order(abs(period_asset_class_contributions$PercentContribution), decreasing = TRUE),
      ]
      
      # Store results for this period
      period_results[[period]] <- list(
        asset_class_contributions = period_asset_class_contributions,
        active_risk = active_risk,
        active_weights = period_active_weights
      )
    }
    
    # Combine all periods into a single data frame for easier analysis
    all_contributions <- do.call(rbind, lapply(period_results, function(x) x$asset_class_contributions))
    all_active_risks <- sapply(period_results, function(x) x$active_risk)
    
    return(list(
      period_results = period_results,
      all_contributions = all_contributions,
      all_active_risks = all_active_risks,
      active_cov_matrix = active_cov_matrix
    ))
    
  } else {
    # For constant weights: original calculation
    
    # Convert to vectors if needed
    if (is.matrix(portfolio_weights)) portfolio_weights <- as.vector(portfolio_weights)
    if (is.matrix(benchmark_weights)) benchmark_weights <- as.vector(benchmark_weights)
    
    # Calculate active weights
    active_weights <- portfolio_weights - benchmark_weights
    
    # FIXED: Check dimensions before matrix multiplication
    message("Active weights length: ", length(active_weights))
    message("Covariance matrix dimensions: ", paste(dim(active_cov_matrix), collapse = " x "))
    
    if (length(active_weights) != ncol(active_cov_matrix)) {
      stop("Dimension mismatch: active weights length (", length(active_weights), 
           ") does not match covariance matrix size (", ncol(active_cov_matrix), ")")
    }
    
    # Calculate total active risk
    active_risk_variance <- as.numeric(t(active_weights) %*% active_cov_matrix %*% active_weights)
    active_risk <- sqrt(active_risk_variance)
    
    # Initialize results storage
    asset_class_contributions <- data.frame(
      AssetClass = asset_classes,
      ActiveWeight = numeric(num_asset_classes),
      RiskContribution = numeric(num_asset_classes),
      PercentContribution = numeric(num_asset_classes)
    )
    
    # Calculate contributions by asset class
    for (i in 1:num_asset_classes) {
      current_class <- asset_classes[i]
      
      # Get assets in this asset class
      class_assets <- asset_class_mapping$Asset[asset_class_mapping$AssetClass == current_class]
      class_indices <- match(class_assets, colnames(portfolio_returns))
      class_indices <- class_indices[!is.na(class_indices)]
      
      # Skip if no assets from this class in the portfolio
      if (length(class_indices) == 0) {
        next
      }
      
      # Calculate asset class active weight
      class_active_weight <- sum(active_weights[class_indices])
      asset_class_contributions$ActiveWeight[i] <- class_active_weight
      
      # Calculate marginal contribution to risk for this asset class
      class_active_weights <- rep(0, length(active_weights))
      class_active_weights[class_indices] <- active_weights[class_indices]
      
      # Calculate asset class contribution to active risk
      class_contribution <- as.numeric(t(class_active_weights) %*% active_cov_matrix %*% active_weights)
      asset_class_contributions$RiskContribution[i] <- class_contribution
      
      # Calculate percentage contribution
      if (active_risk_variance > 0) {
        asset_class_contributions$PercentContribution[i] <-
          class_contribution / active_risk_variance * 100
      } else {
        asset_class_contributions$PercentContribution[i] <- 0
      }
    }
    
    # Sort by absolute percentage contribution
    asset_class_contributions <- asset_class_contributions[
      order(abs(asset_class_contributions$PercentContribution), decreasing = TRUE),
    ]
    
    return(list(
      asset_class_contributions = asset_class_contributions,
      active_risk = active_risk,
      active_cov_matrix = active_cov_matrix
    ))
  }
}

#' Plot Asset Class Risk Contributions
#' 
#' @description 
#' Creates a horizontal bar chart showing the percentage contribution of each
#' asset class to total active risk.
#' 
#' @param risk_results list output from calculate_active_risk_contributions()
#' 
#' @return ggplot object showing asset class risk contributions
#' 
#' @details
#' The function handles both time-varying and constant weight scenarios.
#' For time-varying weights, it aggregates contributions across periods.
#' Positive contributions are shown in dark blue, negative in dark red.
#' 
#' @examples
#' \dontrun{
#' plot_asset_class_risk_contributions(active_risk_results)
#' }
#' 
#' @export
plot_asset_class_risk_contributions <- function(risk_results) {
  # Debug: Check the structure of risk_results
  message("Debug - risk_results structure:")
  message("Names in risk_results: ", paste(names(risk_results), collapse = ", "))
  
  # Handle time-varying weights case vs constant weights case
  if ("all_contributions" %in% names(risk_results)) {
    # Time-varying weights case - use aggregated contributions
    contributions <- risk_results$all_contributions
    message("Using all_contributions (time-varying weights case)")
  } else if ("asset_class_contributions" %in% names(risk_results)) {
    # Constant weights case
    contributions <- risk_results$asset_class_contributions
    message("Using asset_class_contributions (constant weights case)")
  } else {
    stop("Cannot find asset class contributions in risk_results")
  }
  
  # Debug: Check contributions structure
  message("Debug - contributions structure:")
  message("Contributions class: ", paste(class(contributions), collapse = ", "))
  
  # Ensure it's a proper data frame
  if (!is.data.frame(contributions)) {
    message("Converting contributions to data frame...")
    contributions <- as.data.frame(contributions)
  }
  
  # Remove any rows with all NA values or missing AssetClass
  contributions <- contributions[!is.na(contributions$AssetClass) & contributions$AssetClass != "", ]
  
  if (nrow(contributions) == 0) {
    stop("No valid asset class contributions found for plotting")
  }
  
  # For time-varying case, we might want to aggregate or use average contributions
  if ("Period" %in% names(contributions)) {
    message("Aggregating time-varying contributions...")
    # Calculate average contributions across periods
    contributions <- contributions %>%
      group_by(AssetClass) %>%
      summarise(
        ActiveWeight = mean(ActiveWeight, na.rm = TRUE),
        RiskContribution = mean(RiskContribution, na.rm = TRUE),
        PercentContribution = mean(PercentContribution, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      as.data.frame()
  }
  
  # Create color palette based on sign of contribution
  contributions$Direction <- ifelse(contributions$PercentContribution >= 0, "Positive", "Negative")
  
  # Debug: Check final data frame
  message("Debug - final contributions for plotting:")
  message("Number of asset classes: ", nrow(contributions))
  
  # Plot percentage contribution to active risk
  ggplot(contributions, aes(x = reorder(AssetClass, PercentContribution),
                            y = PercentContribution,
                            fill = Direction)) +
    geom_bar(stat = "identity") +
    coord_flip() +  # Horizontal bars
    labs(title = "Percentage Contribution to Active Risk by Asset Class",
         subtitle = paste("Total Active Risk (TE):", round(mean(risk_results$all_active_risks) * 100, 2), "%"),
         x = "Asset Class",
         y = "Percentage of Active Risk (%)") +
    theme_minimal() +
    theme(text = element_text(family = plot_font, size = 12),
          plot.title = element_text(family = plot_font, size = 14, face = "bold"),
          plot.subtitle = element_text(family = plot_font, size = 12),
          axis.title = element_text(family = plot_font, size = 12),
          axis.text = element_text(family = plot_font, size = 11),
          legend.title = element_text(family = plot_font, size = 12),
          legend.text = element_text(family = plot_font, size = 11)) +
    scale_fill_manual(values = c("Positive" = "darkblue", "Negative" = "darkred"))
}

#' Calculate Active Risk Attribution with Interaction Effects
#' 
#' @description 
#' Calculates active risk attribution including pairwise interaction effects
#' between asset classes.
#' 
#' @param portfolio_returns matrix of portfolio returns (time x assets)
#' @param benchmark_returns matrix of benchmark returns (time x assets)
#' @param portfolio_weights matrix or vector of portfolio weights  
#' @param benchmark_weights matrix or vector of benchmark weights
#' @param asset_class_mapping data.frame with columns 'Asset' and 'AssetClass'
#' 
#' @return list containing:
#' \itemize{
#'   \item basic_results: output from calculate_active_risk_contributions()
#'   \item interaction_matrix: matrix of pairwise interaction effects
#'   \item interaction_effects: data.frame of all interaction effects
#'   \item meaningful_interactions: data.frame of significant interactions only
#'   \item total_active_risk: scalar total active risk from interaction matrix
#' }
#' 
#' @details
#' The interaction matrix shows how each pair of asset classes contributes to
#' total active risk variance. Diagonal elements are direct effects, 
#' off-diagonal elements are interaction effects.
#' 
#' @examples
#' \dontrun{
#' interaction_results <- calculate_active_risk_with_interactions(
#'   portfolio_asset_returns, benchmark_asset_returns,
#'   portfolio_weights, benchmark_weights, asset_class_mapping
#' )
#' }
#' 
#' @export
calculate_active_risk_with_interactions <- function(portfolio_returns, benchmark_returns,
                                                    portfolio_weights, benchmark_weights,
                                                    asset_class_mapping) {
  # Basic active risk calculation
  basic_results <- calculate_active_risk_contributions(
    portfolio_returns, benchmark_returns, 
    portfolio_weights, benchmark_weights,
    asset_class_mapping
  )
  
  # Debug: Check basic_results structure
  message("Debug - basic_results structure:")
  message("Names in basic_results: ", paste(names(basic_results), collapse = ", "))
  
  # Get the appropriate asset class contributions based on the structure
  if ("all_contributions" %in% names(basic_results)) {
    # Time-varying weights case - aggregate contributions
    message("Using time-varying weights case")
    asset_class_contributions <- basic_results$all_contributions %>%
      group_by(AssetClass) %>%
      summarise(
        RiskContribution = mean(RiskContribution, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      as.data.frame()
  } else {
    # Constant weights case
    message("Using constant weights case")
    asset_class_contributions <- basic_results$asset_class_contributions
  }
  
  # Debug: Check asset class contributions
  message("Debug - asset_class_contributions:")
  message("Number of asset classes: ", nrow(asset_class_contributions))
  
  # Get active returns and weights
  active_returns <- portfolio_returns - benchmark_returns
  
  # Handle weights based on structure
  if (is.matrix(portfolio_weights) && nrow(portfolio_weights) > 1) {
    # Time-varying weights - use average weights for interaction calculation
    portfolio_weights_avg <- colMeans(portfolio_weights)
    benchmark_weights_avg <- colMeans(benchmark_weights)
    active_weights <- portfolio_weights_avg - benchmark_weights_avg
  } else {
    # Constant weights
    if (is.matrix(portfolio_weights)) portfolio_weights <- as.vector(portfolio_weights)
    if (is.matrix(benchmark_weights)) benchmark_weights <- as.vector(benchmark_weights)
    active_weights <- portfolio_weights - benchmark_weights
  }
  
  active_cov_matrix <- basic_results$active_cov_matrix
  
  # Get asset classes
  asset_classes <- unique(asset_class_mapping$AssetClass)
  num_asset_classes <- length(asset_classes)
  
  # Initialize matrix for pairwise interactions
  interaction_matrix <- matrix(0, nrow = num_asset_classes, ncol = num_asset_classes)
  rownames(interaction_matrix) <- asset_classes
  colnames(interaction_matrix) <- asset_classes
  
  # Calculate interaction effects
  for (i in 1:num_asset_classes) {
    class_i <- asset_classes[i]
    assets_i <- asset_class_mapping$Asset[asset_class_mapping$AssetClass == class_i]
    indices_i <- match(assets_i, colnames(portfolio_returns))
    indices_i <- indices_i[!is.na(indices_i)]
    
    if (length(indices_i) == 0) next
    
    weights_i <- rep(0, length(active_weights))
    weights_i[indices_i] <- active_weights[indices_i]
    
    for (j in i:num_asset_classes) {
      class_j <- asset_classes[j]
      assets_j <- asset_class_mapping$Asset[asset_class_mapping$AssetClass == class_j]
      indices_j <- match(assets_j, colnames(portfolio_returns))
      indices_j <- indices_j[!is.na(indices_j)]
      
      if (length(indices_j) == 0) next
      
      weights_j <- rep(0, length(active_weights))
      weights_j[indices_j] <- active_weights[indices_j]
      
      # Skip self-interaction (that's the direct effect)
      if (i == j) {
        # FIXED: Find the risk contribution for this asset class
        risk_contrib_row <- asset_class_contributions[asset_class_contributions$AssetClass == class_i, ]
        
        if (nrow(risk_contrib_row) > 0) {
          interaction_matrix[i, j] <- risk_contrib_row$RiskContribution[1]
        } else {
          message("Warning: No risk contribution found for asset class: ", class_i)
          interaction_matrix[i, j] <- 0
        }
      } else {
        # Calculate interaction effect
        interaction_ij <- t(weights_i) %*% active_cov_matrix %*% weights_j
        
        # Store in matrix (symmetric)
        interaction_matrix[i, j] <- as.numeric(interaction_ij)
        interaction_matrix[j, i] <- as.numeric(interaction_ij)
      }
    }
  }
  
  # Calculate total active risk from matrix
  total_active_risk_variance <- sum(interaction_matrix)
  
  # Verify it matches the direct calculation
  direct_active_risk_variance <- as.numeric(t(active_weights) %*% active_cov_matrix %*% active_weights)
  
  # Create interaction effect dataframe
  interaction_df <- reshape2::melt(interaction_matrix, varnames = c("AssetClass1", "AssetClass2"),
                                   value.name = "InteractionEffect")
  
  # Add percentage contribution
  if (total_active_risk_variance > 0) {
    interaction_df$PercentContribution <-
      interaction_df$InteractionEffect / total_active_risk_variance * 100
  } else {
    interaction_df$PercentContribution <- 0
  }
  
  # Filter for meaningful interactions (non-zero and not self-interactions)
  meaningful_interactions <- interaction_df %>%
    filter(abs(PercentContribution) > 0.5 & AssetClass1 != AssetClass2)
  
  return(list(
    basic_results = basic_results,
    interaction_matrix = interaction_matrix,
    interaction_effects = interaction_df,
    meaningful_interactions = meaningful_interactions,
    total_active_risk = sqrt(abs(total_active_risk_variance))  # Use abs() to handle potential negative values
  ))
}

#' Load Required Libraries for Data Processing
#' 
#' @description Loads tidyverse and fs packages for data manipulation
#' @return NULL (loads packages)
library(tidyverse)
library(fs)

#' Load Asset Returns Data
#' 
#' @description 
#' Loads asset returns data from CSV file and converts to matrix format
#' 
#' @param csv_filename character path to CSV file
#' @return matrix of asset returns (time x assets)
csv_filename <- "~/Desktop/TRADING/SAHS Asset Returns.csv"
if (file_exists(csv_filename)) {
  asset_returns_df <- read.csv(csv_filename, header = TRUE)
  asset_returns <- asset_returns_df %>% select(-Date) %>% as.matrix()
  message("Asset returns loaded. Dimensions: ", paste(dim(asset_returns), collapse = " x "))
  message("Asset names: ", paste(colnames(asset_returns), collapse = ", "))
} else {
  stop("Asset returns file not found: ", csv_filename)
}

#' Load Benchmark Weights Data
#' 
#' @description 
#' Loads benchmark weights data from CSV file and converts to matrix format
#' 
#' @param csv_filename character path to CSV file  
#' @return matrix of benchmark weights (time x assets)
csv_filename <- "~/Desktop/TRADING/SAHS Benchmark Weights.csv"
if (file_exists(csv_filename)) {
  benchmark_weights_df <- read.csv(csv_filename, header = TRUE)
  benchmark_weights <- benchmark_weights_df %>% select(-Date) %>% as.matrix()
  message("Benchmark weights loaded. Dimensions: ", paste(dim(benchmark_weights), collapse = " x "))
  message("Benchmark weight columns: ", paste(colnames(benchmark_weights), collapse = ", "))
} else {
  stop("Benchmark weights file not found: ", csv_filename)
}

#' Load Portfolio Weights Data
#' 
#' @description 
#' Loads portfolio weights data from CSV file and converts to matrix format
#' 
#' @param csv_filename character path to CSV file
#' @return matrix of portfolio weights (time x assets)  
csv_filename <- "~/Desktop/TRADING/SAHS Portfolio Weights.csv"
if (file_exists(csv_filename)) {
  portfolio_weights_df <- read.csv(csv_filename, header = TRUE)
  portfolio_weights <- portfolio_weights_df %>% select(-Date) %>% as.matrix()
  message("Portfolio weights loaded. Dimensions: ", paste(dim(portfolio_weights), collapse = " x "))
  message("Portfolio weight columns: ", paste(colnames(portfolio_weights), collapse = ", "))
} else {
  stop("Portfolio weights file not found: ", csv_filename)
}

#' Check Column Name Consistency
#' 
#' @description 
#' Verifies that all data sources have consistent asset naming and finds
#' common assets across all datasets
#' 
#' @return character vector of common asset names
message("\nChecking column name consistency:")
message("Asset returns columns: ", paste(colnames(asset_returns), collapse = ", "))
message("Benchmark weights columns: ", paste(colnames(benchmark_weights), collapse = ", "))
message("Portfolio weights columns: ", paste(colnames(portfolio_weights), collapse = ", "))

# Ensure all matrices have the same column names and order
common_assets <- intersect(intersect(colnames(asset_returns), colnames(benchmark_weights)), 
                           colnames(portfolio_weights))

if (length(common_assets) == 0) {
  stop("No common assets found between returns and weights data!")
}

message("Common assets found: ", length(common_assets))
message("Common assets: ", paste(common_assets, collapse = ", "))

#' Subset Data to Common Assets
#' 
#' @description 
#' Ensures all matrices contain only the common assets in the same order
#' 
#' @return matrices with consistent column structure
asset_returns <- asset_returns[, common_assets, drop = FALSE]
benchmark_weights <- benchmark_weights[, common_assets, drop = FALSE]
portfolio_weights <- portfolio_weights[, common_assets, drop = FALSE]

#' Create Asset Class Mapping
#' 
#' @description 
#' Maps individual assets to their respective asset classes for risk attribution
#' 
#' @return data.frame with Asset and AssetClass columns
asset_class_mapping <- data.frame(
  Asset = common_assets,
  AssetClass = case_when(
    common_assets %in% c("US.Equity") ~ "Equities",
    common_assets %in% c("IntExUS.Equity") ~ "International", 
    common_assets %in% c("Fixed.Income") ~ "Fixed Income",
    TRUE ~ "Other"  # fallback for any unexpected assets
  )
)

message("\nAsset class mapping:")
print(asset_class_mapping)

#' Calculate Weighted Asset Returns
#' 
#' @description 
#' Calculates individual weighted asset returns for portfolio and benchmark
#' to enable proper active risk decomposition
#' 
#' @return matrices of weighted asset returns
portfolio_asset_returns <- asset_returns * portfolio_weights
benchmark_asset_returns <- asset_returns * benchmark_weights

message("\nCorrected dimensions:")
message("Portfolio asset returns dimensions: ", paste(dim(portfolio_asset_returns), collapse = " x "))
message("Benchmark asset returns dimensions: ", paste(dim(benchmark_asset_returns), collapse = " x "))

#' Calculate Active Risk Contributions
#' 
#' @description 
#' Main analysis: calculates active risk contributions by asset class
#' 
#' @return list with risk attribution results
active_risk_results <- calculate_active_risk_contributions(
  portfolio_asset_returns,   # CORRECTED: individual weighted asset returns
  benchmark_asset_returns,   # CORRECTED: individual weighted asset returns  
  portfolio_weights, 
  benchmark_weights,
  asset_class_mapping
)

#' Display Results
#' 
#' @description 
#' Prints the main results of the active risk analysis
#' 
#' @return NULL (prints results to console)
message("\nTotal Active Risk (Tracking Error): ", round(active_risk_results$active_risk * 100, 2), "%\n")

# Debug: Check the structure of active_risk_results
message("Debug - active_risk_results structure:")
str(active_risk_results)

print(active_risk_results$asset_class_contributions)

#' Create Risk Contribution Plot
#' 
#' @description 
#' Generates visualization of asset class risk contributions
#' 
#' @return ggplot object
message("\nAttempting to plot results...")
plot_asset_class_risk_contributions(active_risk_results)

#' Calculate Interaction Effects
#' 
#' @description 
#' Analyzes interaction effects between asset classes
#' 
#' @return list with interaction analysis results
interaction_results <- calculate_active_risk_with_interactions(
  portfolio_asset_returns,   # CORRECTED: individual weighted asset returns
  benchmark_asset_returns,   # CORRECTED: individual weighted asset returns
  portfolio_weights, 
  benchmark_weights,
  asset_class_mapping
)

#' Display Interaction Results
#' 
#' @description 
#' Shows the top asset class interactions contributing to active risk
#' 
#' @return NULL (prints results to console)
message("\nTop Asset Class Interactions Contributing to Active Risk:")
if (nrow(interaction_results$meaningful_interactions) > 0) {
  top_interactions <- head(interaction_results$meaningful_interactions[
    order(abs(interaction_results$meaningful_interactions$PercentContribution), decreasing = TRUE),
  ], 5)
  print(top_interactions)
} else {
  message("No significant interactions found.")
}

#' Create Interaction Heatmap
#' 
#' @description 
#' Visualizes interaction effects as a heatmap showing risk contribution
#' percentages for each asset class pair
#' 
#' @return ggplot object with heatmap
interaction_matrix <- interaction_results$interaction_matrix
# Convert to percentage contribution
interaction_matrix_pct <- interaction_matrix / sum(interaction_matrix) * 100

# Plot heatmap of interaction effects
p_heatmap <- ggplot(reshape2::melt(interaction_matrix_pct), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue", midpoint = 0,
                       name = "Risk Contribution\n(% of Total Variance)",
                       labels = function(x) paste0(round(x, 1), "%")) +
  labs(title = "Asset Class Interaction Effects",
       subtitle = "Contribution to Active Risk Variance\n(Diagonal = Direct Effects, Off-diagonal = Interaction Effects)",
       x = "Asset Class", y = "Asset Class") +
  theme_minimal() +
  theme(text = element_text(family = plot_font, size = 12),
        plot.title = element_text(family = plot_font, size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = plot_font, size = 11, hjust = 0.5),
        axis.title = element_text(family = plot_font, size = 12, face = "bold"),
        axis.text = element_text(family = plot_font, size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, family = plot_font, size = 11),
        axis.text.y = element_text(family = plot_font, size = 11),
        legend.title = element_text(family = plot_font, size = 11, face = "bold"),
        legend.text = element_text(family = plot_font, size = 10),
        panel.grid = element_blank()) +
  coord_fixed()  # Make squares actually square

#' Display Heatmap and Font Verification
#' 
#' @description 
#' Shows the interaction heatmap and verifies font usage
#' 
#' @return NULL (displays plot and messages)
print(p_heatmap)

# Verify font usage
message("\nPlot font being used: ", plot_font)
if(plot_font == "") {
  message("Warning: Times New Roman not available. Using default font.")
  message("To enable Times New Roman:")
  message("1. Run: extrafont::font_import()")
  message("2. Restart R")
  message("3. Re-run the script")
} else {
  message("Confirmed: Using Times New Roman font for all plot elements.")
}