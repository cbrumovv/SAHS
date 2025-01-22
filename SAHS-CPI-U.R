##
## last edit:  21 jan 2025
##

# Load required packages with pac.man pattern
required_packages <- c("tidyverse", "quantmod", "assertr", "lubridate")
invisible(lapply(required_packages, library, character.only = TRUE))

# Define series metadata using tibble for better printing and handling
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
  # Add validation
  verify(nrow(.) >= 1) %>%
  verify(has_all_names("id", "names")) %>%
  verify(!duplicated(id)) %>%
  verify(!duplicated(names))

# Function to safely download FRED data
safe_get_fred_data <- safely(function(id, start_date, end_date) {
  getSymbols(id, src = "FRED", auto.assign = FALSE,
             from = start_date, to = end_date)
})

# Download and process data
# Improved data download and combination
cpi_data <- series_metadata$id %>%
  set_names(series_metadata$names) %>%
  map_dfc(~{
    result <- safe_get_fred_data(.x, "2013-01-01", "2024-12-31")
    
    if (!is.null(result$error)) {
      warning(sprintf("Error downloading %s: %s", .x, result$error))
      return(NULL)
    }
    
    # Extract just the values
    as.numeric(result$result[,1])
  }) %>%
  # Add date column
  mutate(date = seq(from = as.Date("2013-01-01"), 
                    to = as.Date("2024-12-31"), 
                    by = "month")) %>%
  # Reorder columns to put date first
  select(date, everything())


# Process and calculate returns
cpi_processed <- cpi_data %>%
  # Handle any remaining missing values
  arrange(date) %>%
  mutate(across(-date, ~if_else(is.na(.), lead(.), .))) %>%
  # Calculate simple returns
  mutate(
    across(-date, ~./lag(.) - 1, .names = "{.col}_return")
  ) %>%
  # Filter for desired date range
  filter(date >= "2021-12-31") %>%
  # Calculate cumulative returns correctly
  mutate(
    # For each component, calculate cumulative return
    `All minus food & energy_cum_return` = cumprod(1 + `All minus food & energy_return`) - 1,
    `Shelter_cum_return` = cumprod(1 + Shelter_return) - 1,
    `Medical_cum_return` = cumprod(1 + Medical_return) - 1,
    `All minus medical_cum_return` = cumprod(1 + `All minus medical_return`) - 1,
    `All_cum_return` = cumprod(1 + All_return) - 1
  ) %>%
  # Normalize cumulative returns to start at 0
  mutate(across(matches("cum_return$"), 
                ~. - first(., na.rm = TRUE)))



# Create enhanced visualization
cpi_plot <- cpi_processed %>%
  select(date, matches("cum_return$")) %>%
  pivot_longer(-date, 
               names_to = "component",
               values_to = "value",
               names_pattern = "(.+)_cum_return") %>%
  ggplot(aes(x = date, y = value, color = component)) +
  geom_line(linewidth = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d() +
  labs(
    title = "CPI-U Components Growth in South Region (3-Year)",
    subtitle = "Cumulative Growth Since December 2021",
    x = "Date",
    y = "Cumulative Change",
    color = "Component",
    caption = sprintf("Source: FRED, Last updated: %s", 
                      format(Sys.Date(), "%B %d, %Y"))
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display plot
print(cpi_plot)

# Save processed data
write_csv(cpi_processed, 
          sprintf("cpi_south_processed_%s.csv", 
                  format(Sys.Date(), "%Y%m%d")))