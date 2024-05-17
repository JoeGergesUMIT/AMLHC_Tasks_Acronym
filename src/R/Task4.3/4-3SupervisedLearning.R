# Load necessary package
library(caret)

# Load the Blood-Brain Barrier dataset
data(BloodBrain)

# Combine the datasets into a single data frame
bbbData <- data.frame(logBBB = logBBB, bbbDescr)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (75%) and test (25%) sets
trainIndex <- createDataPartition(bbbData$logBBB, p = 0.75, list = FALSE)
trainData <- bbbData[trainIndex, ]
testData <- bbbData[-trainIndex, ]

# Define the tuning grid for the random forest classifier by varying mtry from 2 to 8
tuneGrid <- expand.grid(mtry = 2:8)

# Train the model using 10-fold cross-validation
control <- trainControl(method = "cv", number = 10)
rfModel <- train(logBBB ~ ., data = trainData, method = "rf", trControl = control, tuneGrid = tuneGrid)

# Predict on the test set
predictions <- predict(rfModel, newdata = testData)

# Calculate RMSE
rmse <- RMSE(predictions, testData$logBBB)
print(paste("RMSE:", rmse))
