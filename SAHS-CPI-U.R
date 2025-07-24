#' CPI-U South Region Analysis
#'
#' @title Analysis of Consumer Price Index for Urban Consumers (CPI-U) - South Region
#' @description This script downloads, processes, and visualizes CPI-U data for the South region
#'   from the Federal Reserve Economic Data (FRED) database. It calculates cumulative returns
#'   for various CPI components and creates visualizations showing price growth patterns.
#' @details
#'   The analysis covers five main CPI components:
#'   \itemize{
#'     \item All items less food and energy (Core CPI)
#'     \item Shelter
#'     \item Medical care
#'     \item All items less medical care
#'     \item All items (Headline CPI)
#'   }
#'   
#'   The workflow includes:
#'   \enumerate{
#'     \item Data download from FRED API using quantmod
#'     \item Data cleaning and missing value handling
#'     \item Calculation of month-over-month and cumulative returns
#'     \item Statistical summary generation
#'     \item Professional visualization creation
#'     \item Data export to CSV format
#'   }
#'   
#' @section Dependencies:
#'   This script requires the following R packages:
#'   \itemize{
#'     \item tidyverse: Data manipulation and visualization
#'     \item quantmod: Financial data downloading
#'     \item assertr: Data validation
#'     \item lubridate: Date handling
#'     \item glue: String interpolation
#'     \item scales: Plot formatting
#'   }
#'   
#' @section FRED Series IDs:
#'   \describe{
#'     \item{CUUR0300SA0L1E}{All items less food and energy - South Region}
#'     \item{CUUR0300SAH1}{Shelter - South Region}
#'     \item{CUUR0300SAM}{Medical care - South Region}
#'     \item{CUUR0300SA0L5}{All items less medical care - South Region}
#'     \item{CUUR0300SA0}{All items - South Region}
#'   }
#'   
#' @author Craigile J. Brumfield, CFA
#' @contact cbrum@mac.com
#' @date 2025-07-23
#' @version 2.0.0
#' @license MIT
#' @keywords CPI inflation economics FRED data-analysis
#' @references
#'   Federal Reserve Economic Data (FRED): \url{https://fred.stlouisfed.org/}
#'   
#'   Bureau of Labor Statistics CPI Data: \url{https://www.bls.gov/cpi/}
#' @seealso
#'   \itemize{
#'     \item \code{\link[quantmod]{getSymbols}} for data downloading
#'     \item \code{\link[tidyverse]{tidyverse}} for data manipulation
#'     \item \code{\link[ggplot2]{ggplot}} for visualization
#'   }
#' @import tidyverse quantmod assertr lubridate glue scales
#' @export
#' @examples
#' \dontrun{
#' # Run the complete CPI analysis workflow
#' source("SAHS-CPI-U.R")
#' 
#' # Or run individual components:
#' metadata <- create_series_metadata()
#' cpi_data <- download_cpi_data(metadata)
#' processed_data <- process_cpi_data(cpi_data)
#' plot_data <- prepare_plot_data(processed_data, metadata)
#' cpi_plot <- create_cpi_plot(plot_data)
#' print(cpi_plot)
#' }
NULL

# Load required packages with explicit tidyverse approach
#' @importFrom tidyverse tidyverse
#' @importFrom quantmod getSymbols
#' @importFrom assertr verify has_all_names
#' @importFrom lubridate year month quarter
#' @importFrom glue glue
#' @importFrom scales percent_format pretty_breaks
library(tidyverse)
library(quantmod)
library(assertr)
library(lubridate)

#' CPI Series Metadata
#'
#' @description Creates a tibble containing metadata for CPI-U series from FRED database
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{FRED series identifier}
#'     \item{names}{Human-readable series name}
#'     \item{order}{Plotting order}
#'     \item{category}{Component category (Core, Housing, Healthcare, Overall, Other)}
#'   }
#' @details This function defines the CPI-U series to be analyzed for the South region,
#'   including validation checks and categorical assignments for visualization purposes.
#'   The function performs several data validation steps:
#'   \itemize{
#'     \item Ensures at least one series is defined
#'     \item Checks for required column names
#'     \item Validates uniqueness of series IDs and names
#'     \item Assigns categorical classifications for analysis
#'   }
#' @note The series order affects legend display in visualizations
#' @examples
#' \dontrun{
#' metadata <- create_series_metadata()
#' print(metadata)
#' 
#' # View series categories
#' table(metadata$category)
#' 
#' # Check series count
#' nrow(metadata)
#' }
#' @seealso \code{\link{download_cpi_data}} for using this metadata to download data
#' @export
create_series_metadata <- function() {
  series_metadata <- tibble(
    id = c(
      "CUUR0300SA0L1E",  # All items less food and energy
      "CUUR0300SAH1",    # Shelter
      "CUUR0300SAM",     # Medical
      "CUUR0300SA0L5",   # All items less medical care
      "CUUR0300SA0"      # All items
    ),
    names = c(
      "All minus food & energy",
      "Shelter", 
      "Medical",
      "All minus medical",
      "All"
    )
  ) %>%
    # Enhanced validation with dplyr operations
    verify(n() >= 1) %>%
    verify(has_all_names("id", "names")) %>%
    verify(n_distinct(id) == n()) %>%
    verify(n_distinct(names) == n()) %>%
    # Add series order for consistent plotting
    mutate(
      order = row_number(),
      category = case_when(
        str_detect(names, "food|energy") ~ "Core",
        str_detect(names, "Shelter") ~ "Housing",
        str_detect(names, "Medical") ~ "Healthcare", 
        str_detect(names, "All$") ~ "Overall",
        TRUE ~ "Other"
      )
    )
  
  return(series_metadata)
}

# Create series metadata
series_metadata <- create_series_metadata()

#' Download FRED Economic Data Series
#'
#' @description Safely downloads economic time series data from the Federal Reserve Economic Data (FRED) database
#' @param series_id Character string. The FRED series identifier (e.g., "CUUR0300SA0")
#' @param series_name Character string. Human-readable name for the series
#' @param start_date Character string or Date. Start date in "YYYY-MM-DD" format. Default is "2013-01-01"
#' @param end_date Character string or Date. End date in "YYYY-MM-DD" format. Default is "2024-12-31"
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date of observation}
#'     \item{[series_name]}{Numeric values for the requested series}
#'   }
#'   Returns empty tibble with proper structure if download fails
#' @details This function uses the quantmod package to download data from FRED and converts
#'   the result to a tibble format. It includes comprehensive error handling and informative warnings.
#'   
#'   The function handles several scenarios:
#'   \itemize{
#'     \item Successful data download and conversion
#'     \item Network connectivity issues
#'     \item Invalid series IDs
#'     \item Date range issues
#'     \item API rate limiting
#'   }
#' @note Requires internet connection and valid FRED series ID
#' @section Error Handling:
#'   If download fails, the function:
#'   \itemize{
#'     \item Issues a warning with detailed error message
#'     \item Returns empty tibble with correct column structure
#'     \item Allows analysis workflow to continue with other series
#'   }
#' @examples
#' \dontrun{
#' # Download single series
#' cpi_data <- download_fred_series("CUUR0300SA0", "All Items CPI")
#' 
#' # Download with custom date range
#' shelter_data <- download_fred_series(
#'   "CUUR0300SAH1", 
#'   "Shelter CPI", 
#'   start_date = "2020-01-01",
#'   end_date = "2024-12-31"
#' )
#' 
#' # Check for successful download
#' if (nrow(cpi_data) > 0) {
#'   print("Download successful")
#'   head(cpi_data)
#' }
#' }
#' @importFrom quantmod getSymbols
#' @importFrom glue glue
#' @seealso \code{\link{download_cpi_data}} for downloading multiple series
#' @export
download_fred_series <- function(series_id, series_name, start_date = "2013-01-01", end_date = "2024-12-31") {
  tryCatch({
    data <- getSymbols(series_id, src = "FRED", auto.assign = FALSE,
                       from = start_date, to = end_date)
    
    # Convert to tibble with proper column naming
    tibble(
      date = index(data),
      !!series_name := as.numeric(data[,1])
    )
  }, error = function(e) {
    warning(glue::glue("Failed to download {series_name} ({series_id}): {e$message}"))
    tibble(date = as.Date(character(0)), !!series_name := numeric(0))
  })
}

#' Download and Combine CPI Data
#'
#' @description Downloads CPI data for all series defined in series_metadata and combines into single dataset
#' @param metadata A tibble containing series metadata with 'id' and 'names' columns
#' @return A tibble with date column and CPI values for all series
#' @details Uses pmap_dfr to download multiple series and combines them with proper missing value handling
#' @examples
#' \dontrun{
#' metadata <- create_series_metadata()
#' cpi_data <- download_cpi_data(metadata)
#' }
#' @export
download_cpi_data <- function(metadata) {
  cpi_data <- metadata %>%
    # Use pmap to apply function with multiple arguments
    pmap_dfr(~ download_fred_series(..1, ..2)) %>%
    # Group by date and summarise to handle any duplicates
    group_by(date) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    # Filter for complete date range and fill missing values
    filter(date >= as.Date("2013-01-01"), date <= as.Date("2024-12-31")) %>%
    # Enhanced missing value handling with dplyr
    arrange(date) %>%
    fill(everything(), .direction = "down") %>%
    fill(everything(), .direction = "up")
  
  return(cpi_data)
}

# Download and combine data using enhanced dplyr operations
cpi_data <- download_cpi_data(series_metadata)

#' Process CPI Data for Analysis
#'
#' @description Calculates returns and cumulative changes for CPI data
#' @param data A tibble containing CPI data with date column and numeric series
#' @param analysis_start_date Character string or Date. Start date for analysis period. Default is "2023-12-31"
#' @return A tibble with original data plus calculated returns, cumulative returns, and time variables
#' @details Calculates month-over-month returns, cumulative returns normalized to start at zero,
#'   and adds analytical time variables (year, month, quarter, days since start)
#' @examples
#' \dontrun{
#' processed_data <- process_cpi_data(cpi_data)
#' custom_period <- process_cpi_data(cpi_data, analysis_start_date = "2022-01-01")
#' }
#' @export
process_cpi_data <- function(data, analysis_start_date = "2023-12-31") {
  processed_data <- data %>%
    arrange(date) %>%
    # Calculate month-over-month returns for all series
    mutate(
      across(
        -date, 
        ~ (.x / lag(.x, 1) - 1),
        .names = "{.col}_return"
      )
    ) %>%
    # Filter to desired analysis period
    filter(date >= as.Date(analysis_start_date)) %>%
    # Calculate cumulative returns using enhanced dplyr approach
    mutate(
      across(
        ends_with("_return"),
        ~ cumsum(replace_na(.x, 0)),
        .names = "{str_remove(.col, '_return')}_cum_return"
      )
    ) %>%
    # Normalize cumulative returns to start from zero
    mutate(
      across(
        ends_with("_cum_return"),
        ~ .x - first(.x, na.rm = TRUE)
      )
    ) %>%
    # Add additional analytical columns
    mutate(
      year = year(date),
      month = month(date, label = TRUE),
      quarter = quarter(date),
      days_since_start = as.numeric(date - min(date, na.rm = TRUE))
    )
  
  return(processed_data)
}

# Calculate returns and cumulative changes with enhanced dplyr operations
cpi_processed <- process_cpi_data(cpi_data)

#' Generate Summary Statistics for CPI Data
#'
#' @description Creates summary statistics for cumulative returns of CPI components
#' @param processed_data A tibble containing processed CPI data with cumulative return columns
#' @return A tibble with summary statistics (min, max, final return, volatility, observations) by component
#' @details Calculates key descriptive statistics for each CPI component's cumulative returns,
#'   ordered by final return performance
#' @examples
#' \dontrun{
#' stats <- generate_summary_stats(cpi_processed)
#' print(stats)
#' }
#' @export
generate_summary_stats <- function(processed_data) {
  summary_stats <- processed_data %>%
    select(date, ends_with("_cum_return")) %>%
    pivot_longer(
      cols = ends_with("_cum_return"),
      names_to = "component", 
      values_to = "cum_return",
      names_pattern = "(.+)_cum_return"
    ) %>%
    group_by(component) %>%
    summarise(
      min_return = min(cum_return, na.rm = TRUE),
      max_return = max(cum_return, na.rm = TRUE),
      final_return = last(cum_return, na.rm = TRUE),
      volatility = sd(cum_return, na.rm = TRUE),
      observations = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(final_return))
  
  return(summary_stats)
}

# Create summary statistics using dplyr
summary_stats <- generate_summary_stats(cpi_processed)

#' Prepare Plot Data for Visualization
#'
#' @description Prepares CPI data for plotting by reshaping and joining with metadata
#' @param processed_data A tibble containing processed CPI data with cumulative return columns
#' @param metadata A tibble containing series metadata for joining plot information
#' @return A tibble in long format ready for ggplot visualization with proper factor ordering
#' @details Transforms wide data to long format, joins with metadata, and creates proper factor ordering for consistent legends
#' @examples
#' \dontrun{
#' plot_data <- prepare_plot_data(cpi_processed, series_metadata)
#' }
#' @export
prepare_plot_data <- function(processed_data, metadata) {
  plot_data <- processed_data %>%
    select(date, ends_with("_cum_return")) %>%
    pivot_longer(
      cols = ends_with("_cum_return"),
      names_to = "component",
      values_to = "value", 
      names_pattern = "(.+)_cum_return"
    ) %>%
    # Join with metadata for enhanced plotting
    left_join(
      metadata %>% select(names, category, order),
      by = c("component" = "names")
    ) %>%
    # Arrange for consistent legend ordering
    arrange(order) %>%
    mutate(component = fct_reorder(component, order))
  
  return(plot_data)
}

# Enhanced visualization using dplyr for data preparation
plot_data <- prepare_plot_data(cpi_processed, series_metadata)

#' Create CPI Visualization Plot
#'
#' @description Creates a line plot showing cumulative CPI growth by component
#' @param plot_data A tibble in long format with date, component, and value columns
#' @return A ggplot object showing CPI component growth over time
#' @details Creates a professional visualization with proper formatting, colors, and annotations
#'   showing cumulative percentage changes in CPI components
#' @examples
#' \dontrun{
#' plot_data <- prepare_plot_data(cpi_processed, series_metadata)
#' cpi_plot <- create_cpi_plot(plot_data)
#' print(cpi_plot)
#' }
#' @import ggplot2
#' @importFrom scales percent_format pretty_breaks
#' @export
create_cpi_plot <- function(plot_data) {
  cpi_plot <- plot_data %>%
    ggplot(aes(x = date, y = value, color = component)) +
    geom_line(linewidth = 0.75, alpha = 0.8) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      breaks = scales::pretty_breaks(n = 8)
    ) +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y",
      expand = c(0.02, 0.02)
    ) +
    scale_color_viridis_d(name = "CPI Component") +
    labs(
      title = "CPI-U Components Growth in South Region",
      subtitle = glue::glue("Cumulative Growth Since {format(min(plot_data$date), '%B %Y')}"),
      x = "Date",
      y = "Cumulative Change (%)",
      caption = glue::glue("Source: Federal Reserve Economic Data (FRED)\nLast updated: {format(Sys.Date(), '%B %d, %Y')}")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15)),
      plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", size = 0.3),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
  
  return(cpi_plot)
}

# Create enhanced visualization
cpi_plot <- create_cpi_plot(plot_data)

#' Export Processed CPI Data
#'
#' @description Exports processed CPI data to CSV file with organized column structure
#' @param processed_data A tibble containing processed CPI data
#' @param filename Character string. Optional custom filename. If NULL, generates timestamped filename
#' @return Character string of the output filename
#' @details Selects and organizes columns for export including original values, returns, and cumulative returns
#' @examples
#' \dontrun{
#' filename <- export_cpi_data(cpi_processed)
#' custom_export <- export_cpi_data(cpi_processed, "my_cpi_analysis.csv")
#' }
#' @export
export_cpi_data <- function(processed_data, filename = NULL) {
  if (is.null(filename)) {
    filename <- glue::glue("cpi_south_processed_{format(Sys.Date(), '%Y%m%d')}.csv")
  }
  
  processed_data %>%
    # Select and arrange columns for export
    select(
      date, year, month, quarter,
      # Original values
      starts_with("All"), Medical, Shelter,
      # Returns
      ends_with("_return"),
      # Cumulative returns  
      ends_with("_cum_return")
    ) %>%
    write_csv(filename)
  
  return(filename)
}

# Display results
print("=== Summary Statistics ===")
print(summary_stats)
print("\n=== Plot ===")
print(cpi_plot)

# Enhanced data export with dplyr operations
output_filename <- export_cpi_data(cpi_processed)

cat(glue::glue("\n=== Data exported to: {output_filename} ===\n"))

#' Generate Analysis Summary Report
#'
#' @description Creates a comprehensive summary report of the CPI analysis
#' @param metadata A tibble containing series metadata
#' @param statistics A tibble containing summary statistics
#' @param processed_data A tibble containing processed CPI data
#' @return A list containing analysis metadata and summary information
#' @details Compiles key information about the analysis including date ranges, observations, and series analyzed
#' @examples
#' \dontrun{
#' report <- generate_analysis_report(series_metadata, summary_stats, cpi_processed)
#' }
#' @export
generate_analysis_report <- function(metadata, statistics, processed_data) {
  summary_report <- list(
    metadata = metadata,
    statistics = statistics,
    date_range = range(processed_data$date),
    observations = nrow(processed_data),
    series_count = nrow(metadata),
    analysis_date = Sys.Date()
  )
  
  return(summary_report)
}

# Create comprehensive summary report
summary_report <- generate_analysis_report(series_metadata, summary_stats, cpi_processed)

#' Print Analysis Summary
#'
#' @description Prints a formatted summary of the CPI analysis results
#' @param report A list containing analysis summary information
#' @export
print_analysis_summary <- function(report) {
  cat("=== CPI-U South Region Analysis Complete ===\n")
  cat(glue::glue("Analysis Date: {format(report$analysis_date, '%B %d, %Y')}\n"))
  cat(glue::glue("Date Range Analyzed: {report$date_range[1]} to {report$date_range[2]}\n"))
  cat(glue::glue("Total Observations: {report$observations}\n"))
  cat(glue::glue("CPI Series Analyzed: {report$series_count}\n"))
  cat("=====================================\n")
}

# Print final summary
print_analysis_summary(summary_report)

#' @section Package Structure:
#'   If converting this script to a package, the following structure is recommended:
#'   \preformatted{
#'   SAHS-CPI-Package/
#'   ├── DESCRIPTION
#'   ├── NAMESPACE
#'   ├── R/
#'   │   ├── data_download.R
#'   │   ├── data_processing.R
#'   │   ├── visualization.R
#'   │   └── utils.R
#'   ├── man/
#'   │   └── [auto-generated .Rd files]
#'   ├── tests/
#'   │   └── testthat/
#'   ├── vignettes/
#'   └── README.md
#'   }
#'   
#' @section NAMESPACE Generation:
#'   To generate NAMESPACE file, use:
#'   \code{devtools::document()}
#'   
#'   Expected exports:
#'   \itemize{
#'     \item create_series_metadata()
#'     \item download_fred_series()
#'     \item download_cpi_data()
#'     \item process_cpi_data()
#'     \item generate_summary_stats()
#'     \item prepare_plot_data()
#'     \item create_cpi_plot()
#'     \item export_cpi_data()
#'     \item generate_analysis_report()
#'     \item print_analysis_summary()
#'   }
#'   
#' @section Future Enhancements:
#'   Potential improvements for package development:
#'   \itemize{
#'     \item Add unit tests with testthat
#'     \item Create vignettes for usage examples
#'     \item Add parameter validation functions
#'     \item Implement caching for downloaded data
#'     \item Add support for multiple geographic regions
#'     \item Create Shiny dashboard interface
#'     \item Add forecasting capabilities
#'     \item Implement automated report generation
#'   }
#'   
#' @section Version History:
#'   \describe{
#'     \item{v1.0.0}{Initial script version with basic functionality}
#'     \item{v2.0.0}{Enhanced with tidyverse/dplyr and roxygen2 documentation}
#'   }

#' CPI Analysis Workflow
#' 
#' @description Complete workflow function that executes the entire CPI analysis pipeline
#' @param analysis_start_date Character string or Date. Start date for analysis period. Default is "2023-12-31"
#' @param export_data Logical. Whether to export processed data to CSV. Default is TRUE
#' @param show_plot Logical. Whether to display the plot. Default is TRUE
#' @param custom_filename Character string. Optional custom filename for data export
#' @return A list containing all analysis components:
#'   \describe{
#'     \item{metadata}{Series metadata tibble}
#'     \item{raw_data}{Downloaded CPI data}
#'     \item{processed_data}{Processed data with returns and cumulative metrics}
#'     \item{summary_stats}{Summary statistics}
#'     \item{plot}{ggplot object}
#'     \item{report}{Analysis summary report}
#'   }
#' @details This convenience function runs the complete analysis workflow in a single call,
#'   making it easy to reproduce the analysis with different parameters or integrate
#'   into automated reporting systems.
#' @examples
#' \dontrun{
#' # Run complete analysis with defaults
#' results <- run_cpi_analysis()
#' 
#' # Custom analysis period
#' results <- run_cpi_analysis(
#'   analysis_start_date = "2022-01-01",
#'   export_data = FALSE,
#'   show_plot = TRUE
#' )
#' 
#' # Access specific components
#' print(results$summary_stats)
#' print(results$plot)
#' }
#' @export
run_cpi_analysis <- function(analysis_start_date = "2023-12-31", 
                             export_data = TRUE, 
                             show_plot = TRUE,
                             custom_filename = NULL) {
  
  # Execute the workflow
  metadata <- create_series_metadata()
  raw_data <- download_cpi_data(metadata)
  processed_data <- process_cpi_data(raw_data, analysis_start_date)
  summary_stats <- generate_summary_stats(processed_data)
  plot_data <- prepare_plot_data(processed_data, metadata)
  cpi_plot <- create_cpi_plot(plot_data)
  report <- generate_analysis_report(metadata, summary_stats, processed_data)
  
  # Optional outputs
  if (show_plot) {
    print(cpi_plot)
  }
  
  if (export_data) {
    filename <- export_cpi_data(processed_data, custom_filename)
    cat(glue::glue("Data exported to: {filename}\n"))
  }
  
  print_analysis_summary(report)
  
  # Return comprehensive results
  return(list(
    metadata = metadata,
    raw_data = raw_data,
    processed_data = processed_data,
    summary_stats = summary_stats,
    plot = cpi_plot,
    report = report
  ))
}