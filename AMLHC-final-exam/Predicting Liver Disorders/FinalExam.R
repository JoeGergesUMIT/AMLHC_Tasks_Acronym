# Install necessary packages if not already installed
install.packages("tidyverse")
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("xgboost")
install.packages("ggplot2")

# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(xgboost)
library(ggplot2)

# Load the dataset
file_path <- "~/Downloads/liver+disorders/bupa.data"
liver_data <- read.csv(file_path, header = FALSE)

# Inspect the data
head(liver_data)
str(liver_data)
summary(liver_data)

# Check for missing values
sum(is.na(liver_data))

# Rename columns for better readability if necessary
colnames(liver_data) <- c("mcv", "alkphos", "sgpt", "sgot", "gammagt", "drinks", "selector")

# Convert the target variable to a factor
liver_data$selector <- as.factor(liver_data$selector)

# Standardize the data
preProcValues <- preProcess(liver_data[, -7], method = c("center", "scale"))
liver_data_scaled <- predict(preProcValues, liver_data[, -7])
liver_data_scaled$selector <- liver_data$selector

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(liver_data_scaled$selector, p = .8, list = FALSE, times = 1)
liver_train <- liver_data_scaled[trainIndex,]
liver_test <- liver_data_scaled[-trainIndex,]

# Train a Random Forest model with 10-fold cross-validation and hyperparameter tuning
tuneGrid <- expand.grid(.mtry = c(1:6))
rf_control <- trainControl(method = "cv", number = 10)
rf_model <- train(selector ~ ., data = liver_train, method = "rf", tuneGrid = tuneGrid, trControl = rf_control)
print(rf_model)

# Train an XGBoost model
xgb_model <- train(selector ~ ., data = liver_train, method = "xgbTree", trControl = trainControl(method = "cv", number = 10))
print(xgb_model)

# Make predictions
rf_predictions <- predict(rf_model, newdata = liver_test)
xgb_predictions <- predict(xgb_model, newdata = liver_test)

# Evaluate the models
rf_confMatrix <- confusionMatrix(rf_predictions, liver_test$selector)
xgb_confMatrix <- confusionMatrix(xgb_predictions, liver_test$selector)

print(rf_confMatrix)
print(xgb_confMatrix)

# Extract and print feature importance from Random Forest
importance <- varImp(rf_model)
print(importance)

# Create a data frame for plotting
importance_df <- data.frame(
  Feature = rownames(importance$importance),
  Importance = importance$importance[, 1]
)

# Check the data frame
print(importance_df)  # Print to ensure the data frame is correctly created

# Plotting the feature importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance for Liver Disorder Prediction",
       x = "Feature",
       y = "Importance") +
  theme_minimal()
