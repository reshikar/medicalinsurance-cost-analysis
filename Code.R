# Medical Insurance Cost Analysis
# Author: Reshika Rimal

# Load dataset
data <- read.csv("data/medical_insurance.csv")

# Exploratory Data Analysis
plot(data$age, data$charges,
     xlab="Age",
     ylab="Medical Insurance Costs",
     pch=20,
     col="gray")

lines(lowess(data$age, data$charges), col="blue", lwd=2)

plot(as.factor(data$region), data$charges,
     xlab="Region",
     ylab="Medical Insurance Costs")

# Split dataset
set.seed(123)
train.index <- sample(nrow(data),1070)

train.set <- data[train.index,]
test.set <- data[-train.index,]

# Linear regression model
m <- lm(charges ~ age + region, data=train.set)

summary(m)

# Training MSE
train.mse <- mean((train.set$charges - predict(m))^2)

# Test MSE
test.mse <- mean((test.set$charges - predict(m, newdata=test.set))^2)

train.mse
test.mse
