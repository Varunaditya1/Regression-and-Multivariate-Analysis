# Load necessary libraries
library("dplyr")
library("car")
library("readr")
library("corrplot")
library("lmtest")

# To reproduce the same sequence of random numbers every time
# set.seed(110)

setwd('D:/IITH MnC/Sem 6/Regression')

# Load data-set
data <- read_delim("D:/IITH MnC/Sem 6/Regression/cities.csv", show_col_types = FALSE)
data <- data[,-1]
names(data) <- c("Income",	"Commute","Literacy","Job_Growth",	"Physicians",	"Rape_Rate","Restaurants","Housing","Median_Age",	"Household_Income")
View(data)
# Summary statistics
summary(data)
# Correlation matrix and plot
corr_matrix <- cor(data)
corrplot(corr_matrix, method = "color")

# Histogram plot of dependent variable
hist(data$Housing, main="Histogram Plot of Dependent Variable", 
     xlab="Dependent Variable - Housing", ylab="Frequency")

# Split data into train and test sets
# train_idx <- sample(1:nrow(data), nrow(data) * 0.7)
# train_data <- data[train_idx, ]
# test_data <- data[-train_idx, ]

# Create matrices of independent variables and dependent variable
X_train <- model.matrix(Housing ~ ., data = data)[, -1]
y_train <- data$Housing
# X_test <- model.matrix(Strength ~ ., data = test_data)[, -1]
# y_test <- test_data$Strength

# Fitting linear regression model on the train data-set
model <- lm(Housing ~ ., data = data)

# Summary of the model
summary(model)

# Checking the correctness of the assumptions
# 1. Linearity
# 2. Independence of errors
# 3. Homoscedasticity
# 4. Normality of Errors
# 5. Multicollinearity

# 1. Linearity
plot(model, which=1)

# 2. Independence of errors (Auto correlation)
# Performing Durbin-Watson test for Auto correlation
# Durbin-Watson statistic close to 2 implies no auto correlation
dwtest(model)

# 3. Homoscedasticity
# Perform the Breusch-Pagan test for heteroscedasticity
# The p-value far less than 0.05 indicates evidence of heteroscedasticity
bptest(model)
plot(model, which=3)

# Using WLS Method to correct heteroscedasticity
# weights <- 1/sqrt(abs(model$residuals^2))
# wls_model <- lm(Housing ~ Income + Commute + Literacy + Job_Growth 
#                 + Physicians + Rape_Rate  + Restaurants + Median_Age + Household_Income, 
#                 data=data, weights=weights)

# Summary of the new model
summary(model)

# Check for heteroscedasticity for the new model
bptest(model)

# Check for autocorrelation in the residuals for the new model
durbinWatsonTest(model)
# NO auto correlation 
# 4. Normality of Errors
plot(model, which=2)
shapiro.test(data$Housing)
shapiro.test(resid(model))

#Followes Normal Distribution
# 5. Multicollinearity
# VIF values more than 5 or 10 indicate problem with multicollinearity
vif(model)

# Removing the variable with highest VIF Value
new_data = data[, -1]

# Training the new model
final_model <- lm(Housing ~., data=new_data)

# Check for multicollinearity after removing the highest VIF variable
vif(final_model)

#Outliners 
plot(model, which=4)

# Summary of the final model
summary(final_model)

# Evaluating performance by computing various metrics
# Predict the response variable using the final model
y_pred <- predict(final_model, newdata = test_data)

# Calculating MSE (Mean Squared Error)
mse <- mean((test_data$Strength - y_pred)^2)

# Calculating R-Squared
rsq <- summary(final_model)$r.squared

# Calculating MAE (Mean Absolute Error)
mae <- mean(abs(test_data$Strength - y_pred))

# Calculating RMSE (Root Mean Square Error)
rmse <- sqrt(mse)

# Calculating Adjusted R-Squared
n <- nrow(test_data)
p <- length(coef(final_model)) - 1
adj_rsq <- 1 - ((1 - rsq) * (n - 1) / (n - p - 1))

# Printing the calculating metrics
print(paste0("Mean Squared Error: ", mse))
print(paste0("Mean Absolute Error: ", mae))
print(paste0("Root Mean Square Error: ", rmse))
print(paste0("R-Squared: ", rsq))
print(paste0("Adjusted R-Squared: ", adj_rsq))

