library(Hmisc)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the diabetes dataset from a specified path
diabetes <- read.csv("Downloads/diabetes.csv")

# Commentary: The dataset details and column meanings are derived from an analysis in a separate Jupyter notebook.
# Additional note: Encountered difficulties with the R package manager while attempting to install a specific package.

# Function to detect and replace outliers based on the Interquartile Range (IQR)
detect_replace_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)  # Calculate the first quartile
  Q3 <- quantile(x, 0.75)  # Calculate the third quartile
  IQR <- Q3 - Q1  # Interquartile range calculation
  lower_bound <- Q1 - 1.5 * IQR  # Lower bound for outlier detection
  upper_bound <- Q3 + 1.5 * IQR  # Upper bound for outlier detection
  
  # Replace outliers with NA based on calculated bounds
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}

# Apply the outlier detection function to numerical columns of the dataset
num_cols <- sapply(diabetes, is.numeric)  # Identify numerical columns
diabetes[num_cols] <- lapply(diabetes[num_cols], detect_replace_outliers)

# Identifying and replacing zeros in specific columns where zero values are not meaningful
# This knowledge is based on prior data analysis documented in a Jupyter notebook
cols_with_zeros <- c("plas", "pres", "skin", "insu", "mass")
diabetes[cols_with_zeros] <- lapply(diabetes[cols_with_zeros], function(x) replace(x, x == 0, NA))

# Remove rows containing NA values to clean the dataset
diabetes_clean <- diabetes %>% drop_na()

# Generate descriptive statistics of the cleaned dataset
describe(diabetes_clean) 


