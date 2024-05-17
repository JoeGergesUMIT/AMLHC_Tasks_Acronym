# Load necessary package
library(caret)

# Load the dataset components
data(cox2Class, package = "caret")
data(cox2Descr, package = "caret")

# Combine the datasets into a single data frame
cox2 <- data.frame(Class = cox2Class, cox2Descr)

# Inspect the structure of the dataset
str(cox2)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (75%) and test (25%) sets
trainIndex <- createDataPartition(cox2$Class, p = 0.75, list = FALSE)
trainData <- cox2[trainIndex, ]
testData <- cox2[-trainIndex, ]

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
