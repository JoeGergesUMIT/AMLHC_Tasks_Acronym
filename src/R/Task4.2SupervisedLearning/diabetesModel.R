library(foreign)
library(caret)

# Load the ARFF file
diabetes <- read.arff("Downloads/diabetes.arff")

# View the structure and summary of the data
str(diabetes)
summary(diabetes)

# Convert the target variable 'class' to a factor if it is not already
diabetes$class <- as.factor(diabetes$class)

# Fit a Generalized Linear Model (GLM) for binary classification
diabetes.glm <- glm(class ~ ., data = diabetes, family = binomial(link = "logit"))

# Evaluate the model using summary and diagnostic plots
summary(diabetes.glm)
par(mfrow=c(2,2))  # Set up the graphics layout to show 4 plots at once
plot(diabetes.glm)

# Set up training control for model training using caret
train_control <- trainControl(method = "cv", number = 10)  # Use 10-fold cross-validation

# Train the model using caret
diabetes_model <- train(class ~ ., data = diabetes, method = "glm",
                        family = binomial(link = "logit"), trControl = train_control)

# Print the training results
print(diabetes_model)
summary(diabetes_model)
