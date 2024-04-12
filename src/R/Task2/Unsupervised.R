# Load necessary libraries
library(ggplot2)

# 1. Load dataset
food_data <- read.csv("src/R/food.csv")

# 2. Get an overview
# Print dimension of the dataset
print(dim(food_data))

# Check for missing values in each column
print(colSums(is.na(food_data)))

# Print the first few rows of the dataset
print(head(food_data))

# 3. Preprocess data using z-transformation
# Exclude the 'X' column which contains the country names
numerical_data <- food_data[, -1]  # Assumes 'X' is the first column
scaled_data <- scale(numerical_data)

# 4. Perform principal component analysis
pca_result <- princomp(scaled_data, cor = TRUE)

# Extract the scores for the first two principal components
scores <- data.frame(pca_result$scores[, 1:2])
colnames(scores) <- c("PC1", "PC2")
scores$Country <- food_data[, "X"]  # Use 'X' as the country name

# 5. Create a score plot using PC1 and PC2
# Plotting with qplot
qplot(PC1, PC2, data = scores, color = Country, main = "PCA Score Plot", xlab = "Principal Component 1", ylab = "Principal Component 2") +
  geom_text(aes(label = Country), hjust = 0.5, vjust = 1.5, check_overlap = TRUE)
