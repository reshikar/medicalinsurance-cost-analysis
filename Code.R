# Medical Insurance Cost Analysis
# Author: Reshika Rimal
# Program: MS Biostatistics, University of Nevada Reno
#
# This script performs:
# 1. Exploratory Data Analysis (EDA)
# 2. Train/Test Data Split
# 3. Linear Regression Modeling
# 4. Model Performance Evaluation using Mean Squared Error

# 1. Load the dataset

# Read the dataset containing medical insurance information
data <- read.csv("data/medical_insurance.csv")

# Convert region variable into a categorical variable (factor)
data$region <- as.factor(data$region)

# Check dataset structure
str(data)

# 2. Exploratory Data Analysis
# Plot 1: Age vs Medical Insurance Cost
# This plot helps visualize whether insurance cost increases with age

plot(data$age, data$charges,
     xlab = "Age",
     ylab = "Medical Insurance Costs",
     main = "Age vs Medical Insurance Costs",
     pch = 20,
     col = "gray")

# Add a LOWESS smoothing line to observe the trend
lines(lowess(data$age, data$charges),
      col = "blue",
      lwd = 2)



# Plot 2: Region vs Medical Insurance Cost
# This plot shows how costs vary across different U.S. regions

plot(data$region, data$charges,
     xlab = "US Region",
     ylab = "Medical Insurance Costs",
     main = "Region vs Medical Insurance Costs",
     col = c("steelblue", "green", "gold", "lightblue"),
     pch = 19)

# 3. Split Data into Training and Test Sets
# Set seed for reproducibility
set.seed(123)

# Randomly select 1070 observations for the training dataset
train.index <- sample(nrow(data), 1070)

# Create training and test datasets
train.set <- data[train.index, ]
test.set <- data[-train.index, ]

# 4. Fit Linear Regression Model
# Fit a linear regression model predicting insurance cost
# using age and region as predictors

m <- lm(charges ~ age + region, data = train.set)

# Display regression results
summary(m)

# Extract the coefficient for age
coef(m)["age"]

# 5. Model Performance Evaluation
# Calculate Training Mean Squared Error (MSE)
train.mse <- mean((train.set$charges - predict(m))^2)

# Calculate Test Mean Squared Error (MSE)
test.mse <- mean((test.set$charges - predict(m, newdata = test.set))^2)

# Display results
train.mse
test.mse

# 6. Training MSE Using Residuals
# Another way to compute training MSE using model residuals
training.mse.res <- mean(m$residuals^2)
training.mse.res
