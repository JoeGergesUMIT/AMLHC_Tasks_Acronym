
# Load libraries
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

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
cor(heartdata$biking, heartdata$smoking) 

# ---- Model Fitting and Evaluation ----
heart.disease.lm <- lm(heart.disease ~ biking + smoking, data = heartdata)
summary(heart.disease.lm)

# Homoscedasticity
par(mfrow = c(2, 2))
plot(heart.disease.lm)
par(mfrow = c(1, 1))

# ---- Model Training (caret) ----
# install.packages("caret")  # Install if needed
library(caret)
train_control <- trainControl(method = "cv", number = 10)
trained_model <- train(heartdisease ~ biking + smoking, data = heartdata, method = "lm", trControl = train_control)
print(trained_model)

# ---- Visualization (adapted from guide) ----
plotting.data <- expand.grid(
  biking = seq(min(heartdata$biking), max(heartdata$biking), length.out = 30),
  smoking = c(min(heartdata$smoking), mean(heartdata$smoking), max(heartdata$smoking))
)
plotting.data$predicted.y <- predict(heart.disease.lm, newdata = plotting.data)
plotting.data$smoking <- round(plotting.data$smoking, 2) %>% as.factor()

heart.plot <- ggplot(heartdata, aes(x = biking, y = heartdisease)) +
  geom_point() +
  geom_line(data = plotting.data, aes(x = biking, y = predicted.y, color = smoking), size = 1.25) +
  theme_bw() +
  labs(title = "Heart Disease vs. Biking, with Smoking Levels",
       x = "Biking to Work (%)", y = "Heart Disease (%)", color = "Smoking (%)") +
  annotate(geom = "text", x = 30, y = 1.75, label = "y = 15 - 0.2*biking + 0.178*smoking")

heart.plot
