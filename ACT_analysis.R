## create date: 14 dec 2024
## last edit:  8 jan 2025
##

# Load necessary libraries
library(dplyr)
library(tidyr)

act_benchmarks <- data.frame(
  English = 18,
  Math = 22,
  Science = 23,
  STEM = 26,
  `Social Sciences`  = 22
)

# Assuming your dataframe is named 'stpop' and the date column is named 'Last Updated (CT)'
df_original <- stpop %>%
  rename(last_date = `Last Updated (CT)`) %>%
  mutate(last_date = as.Date(last_date, format = "%m/%d/%Y")) %>%
  mutate(Grade = as.numeric(str_remove(Grade, "th")))
  # filter(Grade < 11) 

df_honors <- honors %>%
  rename_with(~ paste0(.x, " Score"), !matches("Test Date|Count")) %>%
  left_join(
    .,
    df_original,
    by = c(
      "Test Date",
      "Composite Score",
      "Math Score",
      "Science Score",
      "STEM Score",
      "English Score",
      "Reading Score"
    )
  ) %>%
  select(-Count.y) %>%
  rename( Count = Count.x)

df_original_ex_honors <- anti_join(
  df_original,
  df_honors,
  by = c(
    "Test Date",
    "Composite Score",
    "Math Score",
    "Science Score",
    "STEM Score",
    "English Score",
    "Reading Score"
  )
)



# Function to normalize numeric columns
normalize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Function to create and print correlation matrix
correlation_matrix <- function(df) {
  cor_matrix <- cor(df %>% select(where(is.numeric)))
  # print(cor_matrix)
  return(cor_matrix)
}


## Scenario 1 -- Full dataset including repeat tests
df_1 <- df_original %>% mutate(across(where(is.numeric), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))
  
cor_matrix <- df_1 %>%
  select(-Grade, -Count, -`Composite Score`) %>%
  mutate(across(where(is.numeric), normalize)) %>%
  correlation_matrix()

model_1 <-
  lm(
    `Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`,
    data = df_1 %>% na.omit() %>% mutate(across(where(is.numeric), normalize))
  )


## Scenario 2: Full dataset adjusted for repeat tests -- use best composite score
df_2 <- df_original %>%
  group_by(`Last Name`) %>%
  arrange(desc(`Composite Score`)) %>%
  slice_max(`Composite Score`, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

cor_matrix_2 <- df_2 %>%
  select(-Grade, -Count, -`Composite Score`) %>%
  mutate(across(where(is.numeric), normalize)) %>%
  correlation_matrix()

model_2 <-
  lm(
    `Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`,
    data = df_2 %>% na.omit() %>% mutate(across(where(is.numeric), normalize))
  )


## Scenario 3: Full dataset adjusted for repeat tests -- use worst composite score
df_3 <- df_original %>%
  group_by(`Last Name`) %>%
  arrange(`Composite Score`) %>% 
  slice_min(`Composite Score`, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

cor_matrix_3 <- df_3 %>%
  select(-Grade, -Count, -`Composite Score`) %>%
  mutate(across(where(is.numeric), normalize)) %>%
  correlation_matrix()

model_3 <-
  lm(
    `Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`,
    data = df_3 %>% na.omit() %>% mutate(across(where(is.numeric), normalize))
  )


## Scenario 4: Full dataset adjusted for repeat tests -- use median scores
df_4 <- df_original %>%
  group_by(`Last Name`) %>%
  summarize(across(contains("Score"), median, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_original %>% select(`Last Name`, `Average GPA`) %>% distinct(`Last Name`, .keep_all = TRUE),by="Last Name") %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

cor_matrix_4 <- df_4 %>%
  select(-`Composite Score`) %>%
  mutate(across(where(is.numeric), normalize)) %>%
  correlation_matrix()

model_4 <-
  lm(
    `Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`,
    data = df_4 %>% na.omit() %>% mutate(across(where(is.numeric), normalize))
  ) 


## Scenario 5: Repeat tests -- change in scores; senstivity to test frequency
df_5 <- df_original %>%
  group_by(`Last Name`) %>%
  arrange(`Last Name`, last_date) %>%
  filter(n() > 1) %>%
  mutate(Attempts = row_number() %>% as.numeric()) %>% 
  filter(row_number() == 1 | row_number() == n()) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
  group_by(`Last Name`) %>%
  mutate(across(contains("Score"), ~ . - lag(.), .names = "diff_{.col}")) %>%
  ungroup() %>%
  select(contains("diff_"), `Last Name`, Attempts,`Average GPA`) %>%
  drop_na()

cor_matrix_5 <- df_5 %>%
  mutate(across(where(is.numeric), normalize)) %>%
  correlation_matrix()

model_5 <-
  lm(
    `diff_Composite Score` ~ Attempts + `diff_Math Score` + `diff_Science Score` + `diff_English Score` + `diff_Reading Score`,
    data = df_5 %>% mutate(across(where(is.numeric), normalize))
  )


## Scenario 6:  Senior Honors including repeat tests

df_6 <- df_honors %>%   mutate(across(where(is.numeric), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) 

cor_matrix_6 <- df_6 %>%
  select(-Grade, -Count, -`Composite Score`) %>%
  na.omit() %>%
  mutate(across(where(is.numeric), normalize)) %>%
  correlation_matrix()

model_6 <-
  lm(
    `Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score`,
    data = df_6 %>% na.omit() %>% mutate(across(where(is.numeric), normalize))
  )

## Scenario 6.5:  Senior Honors adjusted for repeat tests; sensitivity to test frequency
df_honors_mult_attempts <- df_honors %>%
  select(-Grade) %>%
  group_by(`Last Name`) %>%
  arrange(`Last Name`, last_date) %>%
  mutate(Attempts = row_number() %>% as.numeric()) %>% 
  filter(Attempts > 1) %>%
  slice_max(Attempts, n = 1, with_ties = FALSE) %>%
  ungroup()

df_6.5 <- df_honors %>%
  group_by(`Last Name`) %>%
  arrange(`Last Name`, last_date) %>%
  mutate(Attempts = row_number() %>% as.numeric()) %>% 
  ungroup() %>%
  anti_join(df_honors_mult_attempts, by = "Last Name") %>%
  bind_rows(df_honors_mult_attempts) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

cor_matrix_6.5 <- df_6.5 %>%
  select(-Grade, -Count,-`Composite Score`) %>%
  na.omit() %>%
  mutate(across(where(is.numeric), normalize)) %>%
  correlation_matrix()

model_6.5 <-
  lm(
    `Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score` + Attempts,
    data = df_6.5 %>% na.omit() %>% mutate(across(where(is.numeric), normalize))
  )


## Scenario 7:  Full dataset adjusted for repeat tests; final composite score used; sensitivity to multiple attempts
df_mult_attempts <- df_original %>%
  group_by(`Last Name`) %>%
  arrange(`Last Name`, last_date) %>%
  mutate(Attempts = row_number() %>% as.numeric()) %>%
  filter(Attempts > 1) %>%
  slice_max(Attempts, n = 1, with_ties = FALSE) %>%
  ungroup()

df_7 <- df_original %>%
  group_by(`Last Name`) %>%
  arrange(`Last Name`, last_date) %>%
  mutate(Attempts = row_number() %>% as.numeric()) %>%
  ungroup() %>%
  anti_join(df_mult_attempts, by = "Last Name") %>%
  bind_rows(df_mult_attempts) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

cor_matrix_7 <- df_7 %>%
  select(-Grade, -Count, -`Composite Score`) %>%
  na.omit() %>%
  mutate(across(where(is.numeric), normalize)) %>%
  correlation_matrix()

model_7 <-
  lm(
    `Composite Score` ~ `Average GPA` + `Math Score` + `Science Score` + `English Score` + `Reading Score` + Attempts,
    data = df_7 %>% na.omit() %>% mutate(across(where(is.numeric), normalize))
  )



## Create tables

# Assuming your regression models are stored in separate objects 
# like model_1, model_2, ..., model_7

# Coefficient names
coef_names <- c("Math Score", "Attempts", "Average GPA", "Science Score", "STEM Score", "English Score", "Reading Score")

# Create an empty list to store the dataframes
df_list <- list()

# Outer loop for each coefficient name
for (coef_name in coef_names) {
  # Create an empty list to store data for the current coefficient
  coef_data <- list()
  
  # Get all model objects in the environment
  model_names <- ls(pattern = "^model_")
  
  # Remove "model_names" from the model_names array
  model_names <- model_names[model_names != "model_names"] 
  
  # Inner loop through each model
  for (i in 1:length(model_names)) {
    # Get the model object
    model <- get(model_names[i])
    
    # Extract the coefficients and t-statistics
    coefs <- coef(summary(model))
    
    # Adjust chef_name for model 5
    if (model_names[i] == "model_5") {
      if(coef_name == "Attempts") {
        #do nothing
      } else {
      backticked_cname <- paste0("`diff_", coef_name, "`") 
      }
    } else {
      if(coef_name == "Attempts") {
        #do nothing
      } else {
      # Add backticks to chef_name
      backticked_cname <- paste0("`", coef_name, "`")      
      }
    }

    # Check if the coefficient exists in the model
    if (backticked_cname %in% rownames(coefs)) {
      coef_data[[i]] <- data.frame(
        Model_No = model_names[i],
        Regression_Estimate = coefs[backticked_cname, "Estimate"],
        T_stat = coefs[backticked_cname, "t value"]
      )
    } else {
      # Handle the case where the coefficient is missing
      cat("Warning: Coefficient", backticked_cname, "not found in model", model_names[i], "\n")
      coef_data[[i]] <- data.frame(
        Model_No = model_names[i],
        Regression_Estimate = NA,
        T_stat = NA
      )
    }
  }
  
  # Combine the data for the current coefficient into a single dataframe
  df_list[[coef_name]] <- do.call(rbind, coef_data)
}


# Add a "Coefficient_Name" column to each dataframe in df_list
for (coef_name in names(df_list)) {
  df_list[[coef_name]]$Coefficient_Name <- coef_name
}

# Combine all dataframes in df_list into a single dataframe
combined_df <- do.call(rbind, df_list)

# Write the combined dataframe to a CSV file
write.csv(combined_df, file = "~/Downloads/regression_coefficients.csv", row.names = FALSE)

# Access the dataframes using df_list[["Math"]], df_list[["Attempts"]], etc.

##GRAPHICS

# 
# ## histograms
# ggplot(df_honors) +
#   geom_density(aes(x = `Composite Score`, fill = "Composite"), alpha = 0.5) +
#   geom_density(aes(x = `Math Score`, fill = "Math"), alpha = 0.5) +
#   geom_density(aes(x = `Science Score`, fill = "Science"), alpha = 0.5) +
#   geom_density(aes(x = `Reading Score`, fill = "Reading"), alpha = 0.5) +
#   geom_density(aes(x = `English Score`, fill = "English"), alpha = 0.5) +
#   labs(
#     title = "Smooth Histograms of Subject Matter Scores",
#     x = "Score",
#     y = "Density",
#     fill = "Subject"
#   ) +
#   theme_minimal()
# 
# 
# # histograms
# ggplot(df_original) +
#   geom_density(aes(x = `Composite Score`, fill = "Composite"), alpha = 0.5) +
#   geom_density(aes(x = `English Score`, fill = "English"), alpha = 0.5) +
#   geom_density(aes(x = `Math Score`, fill = "Math"), alpha = 0.5) +
#   geom_density(aes(x = `Reading Score`, fill = "Reading"), alpha = 0.5) +
#   geom_density(aes(x = `Science Score`, fill = "Science"), alpha = 0.5) +
#   labs(
#     title = "Distributions of Composite and Subject scores",
#     x = "Score",
#     y = "Density",
#     fill = "Subject"
#   ) +
#   theme_minimal()
# 
# 
#Graph eigenvalues relative to largest contributors to variance -- Principal Components Analysis (PCA)

# df_original_ex_honors %>%
#   select(.,-`Test Date`,-`Composite Score`,-Grade,-Count,-`Last Name`,-last_date,-`STEM Score`) %>%
#   FactoMineR::PCA(., graph = FALSE) %>%
#   factoextra::fviz_pca_var(., repel = TRUE, select.var = list(contrib =
#                                                                 5))
# 
# # Reshape the correlation matrix for ggplot2
# # cor_df <- as.data.frame(cor_matrix_5) %>%
# #   tibble::rownames_to_column("var1") %>%
# #   tidyr::pivot_longer(-var1, names_to = "var2", values_to = "cor")
# # 
# # # Generate heatmap using ggplot2
# # ggplot(cor_df, aes(x = var1, y = var2, fill = cor)) +
# #   geom_tile() +
# #   scale_fill_gradient2(low = "red",
# #                        mid = "white",
# #                        high = "green") +
# #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# #   labs(title = "Correlation Matrix Heatmap - scenario 5", x = "", y = "")
# 
# 
# # library(plotly)
# # 
# # # Create a 3D scatter plot
# # plot_ly(df_honors,
# #         x = ~`Composite Score`,
# #         y = ~`Reading Score`,
# #         z = ~`Math Score`,
# #         type = "scatter3d",
# #         mode = "markers",
# #         color = ~`Composite Score`,
# #         size = ~`Math Score`) %>%
# #   layout(title = "3D Scatter Plot of Student Performance",
# #          scene = list(xaxis = list(title = "Composite Score"),
# #                       yaxis = list(title = "Reading Score"),
# #                       zaxis = list(title = "Math Score")))
# 
# # # Create the 3D scatter plot w/ regression line
# # plot_ly(df_honors %>% na.omit(),         
# #         x = ~`Composite Score`,
# #         y = ~`Reading Score`,
# #         z = ~`Math Score`,
# #         type = "scatter3d", mode = "markers",
# #         marker = list(size = 3),  # Set marker size here
# #         name = "Data Points") %>%
# #   
# #   # Add the regression plane (using fitted values)
# #   add_trace(z = ~fitted(model_6), 
# #             x = ~`Composite Score`, y = ~`Reading Score`, 
# #             type = "mesh3d",  # Use mesh3d for a filled plane
# #             opacity = 0.5, 
# #             intensity = ~fitted(model), # Color intensity based on fitted values
# #             colorscale = "Viridis", 
# #             name = "Regression Plane") %>%
# #   
# #   layout(scene = list(xaxis = list(title = "Composite"),
# #                       yaxis = list(title = "Reading"),
# #                       zaxis = list(title = "Math")))
# 
# # Assuming the data is stored in a data frame called 'data'
# # Perform PCA analysis
# 
# # Create the plot with the specified title and colors
# # plot(res.pca,choix="ind",habillage=6,main="")
# # title(main="Senior Honors Scores")