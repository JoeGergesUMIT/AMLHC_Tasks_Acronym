# Load libraries
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(caret)

# ---- Data Loading and Inspection ----
heartdata <- read.csv("Downloads/heartdata.csv")

summary(heartdata)

# ---- Assumption Checks ----
# Independence - Assumed for this example (no repeated measures)
# Normality
hist(heartdata$heartdisease) 

# Linearity
par(mfrow = c(1, 2))  # Arrange plots side-by-side
plot(heartdisease ~ biking, data = heartdata)
plot(heartdisease ~ smoking, data = heartdata)
par(mfrow = c(1, 1))  # Reset plot arrangement

# Multicollinearity (between predictors)
cor.test(heartdata$biking, heartdata$smoking) 

# ---- Model Fitting and Evaluation ----
heart.disease.lm <- lm(heartdisease ~ biking + smoking, data = heartdata)
summary(heart.disease.lm)

# Homoscedasticity
par(mfrow = c(2, 2))
plot(heart.disease.lm)
par(mfrow = c(1, 1))

# ---- Model Training (caret) ----
train_control <- trainControl(method = "cv", number = 10)
trained_model <- train(heartdisease ~ biking + smoking, data = heartdata, method = "lm", trControl = train_control)
print(trained_model)
