# Install necessary libraries if not already installed
if (!require(readr)) install.packages('readr')
if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(caret)) install.packages('caret')
if (!require(randomForest)) install.packages('randomForest')
if (!require(corrplot)) install.packages('corrplot')
if (!require(GGally)) install.packages('GGally')

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(corrplot)
library(GGally)


# Load the dataset
df <- read_csv("~/Diabetes-Health-Indicators/diabetes_binary_5050split_health_indicators_BRFSS2015.csv")

# Display the first few rows of the dataset
head(df)

# Check for missing values
sum(is.na(df))

# Assuming there are no missing values as the dataset seems to be clean

# Convert appropriate columns to factors if needed
df$Diabetes_binary <- as.factor(df$Diabetes_binary)


# Normalize the data if necessary
preProcValues <- preProcess(df, method = c("center", "scale"))
df_norm <- predict(preProcValues, df)

# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(df_norm$Diabetes_binary, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dfTrain <- df_norm[ trainIndex,]
dfTest  <- df_norm[-trainIndex,]

# Exploratory Data Analysis (EDA)
# Distribution of the target variable
ggplot(df, aes(x = Diabetes_binary)) + 
  geom_bar() + 
  ggtitle('Distribution of Diabetes Status')

# Correlation heatmap
corrplot(cor(df %>% select(-Diabetes_binary)), method = "circle")

# Pairplot using GGally package
ggpairs(df, aes(color = Diabetes_binary))

# Train a Random Forest model
model <- randomForest(Diabetes_binary ~ ., data = dfTrain, importance = TRUE)

# Make predictions
predictions <- predict(model, dfTest)

# Evaluate the model
confusionMatrix(predictions, dfTest$Diabetes_binary)

# Variable importance
importance(model)
varImpPlot(model)



# Propensity Score Matching
if (!require(MatchIt)) install.packages('MatchIt')
library(MatchIt)
ps_model <- glm(Diabetes_binary ~ BMI + GenHlth + HighBP + Age + PhysHlth, data = df, family = binomial())
matched <- matchit(Diabetes_binary ~ BMI + GenHlth + HighBP + Age + PhysHlth, method = "nearest", data = df)
matched_data <- match.data(matched)
glm_outcome <- glm(Diabetes_binary ~ BMI + GenHlth + HighBP + Age + PhysHlth, data = matched_data, family = binomial())
summary(glm_outcome)


