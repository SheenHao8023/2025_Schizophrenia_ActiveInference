#correlation analyses 7th Feb 2025
# Load data
library(readr)
library(corrplot)
library(dplyr)
library(tidyverse)

nd_data <- read_csv("Documents/Academic/HKU/Projects/ZJU_SczAI/analyses_LZ/nd_simulations_nov/23Jan/nd_sim_23jan.csv")

group_colors <- c(
  'HC' = '#66c2a5',
  'PS' = '#fc8d62',
  'NS' = '#8da0cb',
  'BA' = '#e78ac3',
  'WD' = '#a6d854'
)

# re-run correlation analyses 7th Feb 2025
# Split the data by 'Group'
grouped_data <- split(nd_data, nd_data$Group)
# Initialize lists to store results
correlation_results <- list()
p_value_results <- list()

# Loop through each group to calculate correlations
for (group in names(grouped_data)) {
  data_subset <- grouped_data[[group]]
  
  # Remove the 'Group' column for correlation analysis
  data_subset <- data_subset %>% select(-Group)
  
  # Calculate correlation matrix
  cor_matrix <- cor(data_subset, use = "pairwise.complete.obs")
  
  # Calculate p-values
  p_matrix <- cor.mtest(data_subset)$p
  
  # Adjust p-values using Bonferroni method
  adjusted_p_values <- p.adjust(as.vector(p_matrix), method = "bonferroni")
  
  # Reshape adjusted p-values back into a matrix
  adjusted_p_matrix <- matrix(adjusted_p_values, nrow = ncol(data_subset), ncol = ncol(data_subset))
  colnames(adjusted_p_matrix) <- rownames(adjusted_p_matrix) <- colnames(data_subset)
  
  # Store results in lists
  correlation_results[[group]] <- cor_matrix
  p_value_results[[group]] <- adjusted_p_matrix
}

# Convert lists to data frames for easier viewing
correlation_dfs <- lapply(correlation_results, as.data.frame)
p_value_dfs <- lapply(p_value_results, as.data.frame)

# Optionally, name the data frames for easier access
names(correlation_dfs) <- names(p_value_dfs) <- names(grouped_data)

# View results 
correlation_dfs[["HC"]]
p_value_dfs[["HC"]]

# corrplot
# Loop through each group to create correlation plots
for (group_name in names(correlation_dfs)) {
  # Retrieve the correlation matrix and p-value matrix for the current group
  corr_matrix <- correlation_dfs[[group_name]]
  p_matrix <- p_value_dfs[[group_name]]
  
  # Check the structure of the data for the current group
  data_subset <- grouped_data[[group_name]] %>% select(-Group) # Exclude 'Group' column
  print(paste("Data structure for group:", group_name))
  print(str(data_subset)) # Print the structure of the data
  
  # Ensure that the data is numeric
  data_subset <- data_subset %>% mutate(across(everything(), as.numeric)) # Convert all columns to numeric
  
  # Recalculate the correlation matrix
  corr_matrix <- cor(data_subset, use = "pairwise.complete.obs")
  
  # Calculate p-values
  p_matrix <- cor.mtest(data_subset)$p
  
  # Ensure that the matrices are numeric
  if (!is.matrix(corr_matrix) || !is.numeric(corr_matrix)) {
    stop(paste("Correlation matrix for group", group_name, "is not a numeric matrix."))
  }
  
  if (!is.matrix(p_matrix) || !is.numeric(p_matrix)) {
    stop(paste("P-value matrix for group", group_name, "is not a numeric matrix."))
  }
  
  # Set up plotting parameters
  par(mar = c(5, 5, 4, 2)) # Adjust margins for better visibility
  
  # Set up the color palette
  color_palette <- colorRampPalette(c("#1E3A78", "white", "#B22222"))(200)
  
  # Plot the correlation matrix
  corrplot(corr_matrix,
           p.mat = p_matrix, 
           method = 'color', 
           type = 'lower',
           col = color_palette, 
           addCoef.col = "black",
           tl.col = "black", 
           tl.srt = 45, 
           number.cex = 0.8, 
           order = 'original', 
           diag = FALSE,
           title = paste("Correlation Heatmap for Group:", group_name),
           mar = c(0, 0, 1, 0), # Title margin
           addgrid.col = "grey" # Color for grid lines
  )
}