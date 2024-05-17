# Install necessary packages if not already installed
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Load necessary packages
library(caret)

# Load the GermanCredit dataset (for classification)
data(GermanCredit, package = "caret")

# Set seed for reproducibility
set.seed(123)

# Split the data into training (75%) and test (25%) sets
trainIndex <- createDataPartition(GermanCredit$Class, p = 0.75, list = FALSE)
trainData <- GermanCredit[trainIndex, ]
testData <- GermanCredit[-trainIndex, ]

# Define the tuning grid for the random forest classifier by varying mtry from 2 to 8
tuneGrid <- expand.grid(mtry = 2:8)

# Train the model using 10-fold cross-validation
control <- trainControl(method = "cv", number = 10)
rfModel <- train(Class ~ ., data = trainData, method = "rf", trControl = control, tuneGrid = tuneGrid)

# Predict on the test set
predictions <- predict(rfModel, newdata = testData)

# Calculate confusion matrix and performance measures
confMatrix <- confusionMatrix(predictions, testData$Class)
print(confMatrix)
