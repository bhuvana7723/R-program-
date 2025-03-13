#1.one way anova 
# Create the dataset
method_A <- c(80, 85, 88, 92, 95)
method_B <- c(75, 78, 82, 87, 90)
method_C <- c(70, 72, 75, 80, 85)

# Combine the data into a single dataframe
data <- data.frame(
  method = factor(rep(c("A", "B", "C"), each = 5)),
  score = c(method_A, method_B, method_C)
)

# Check the structure of the dataset
str(data)

# Perform one-way ANOVA
anova_result <- aov(score ~ method, data = data)

# Summary of the ANOVA results
summary(anova_result)

# Post-hoc tests (if needed)
# For example, Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

#2.sign test
# Load necessary libraries
library(stats)


# Generate example data
group1 <- c(12, 14, 16, 18, 20)
group2 <- c(11, 13, 15, 17, 19)

# Sign Test
sign_test <- binom.test(sum(group1 > group2), length(group1), p = 0.5, alternative = "greater")

# Print Sign Test results
cat("Sign Test:\n")
print(sign_test)

# Signed Rank Test (Wilcoxon Signed Rank Test for paired samples)
signed_rank_test <- wilcox.test(group1, group2, paired = TRUE)
# Print Signed Rank Test results
cat("\nSigned Rank Test (Wilcoxon Signed Rank Test for paired samples):\n")
print(signed_rank_test)



# Mann-Whitney-Wilcoxon Test for two samples
mw_w_test <- wilcox.test(group1, group2)

# Print Mann-Whitney-Wilcoxon Test results
cat("\nMann-Whitney-Wilcoxon Test for two samples:\n")
print(mw_w_test)

#3.time series 
# Load necessary libraries
#install.packages("forecast")
library(forecast)
# Read the data
data <- read.csv("data.csv")

# Convert Date column to Date type
data$date <- as.Date(data$date)

# Visualize the Time Series
plot(data$date, data$min_temperature, type = "l", xlab = "Date", ylab = "Minimum Temperature", main = "Time Series Plot")

# Fit an ARIMA model
arima_model <- auto.arima(data$min_temperature)
summary(arima_model)

# Forecast Future Values
future_forecast <- forecast(arima_model, h = 30) # Forecasting 30 future values (about a month)
plot(future_forecast, main = "Forecasted Minimum Temperatures")

#4.linear regression 
car_age<-c(4,4,5,5,7,7,8,9,10,11,12)

price <- c(6300,5800,5700,4500,4500,4200,4200,3100,2100,2500,2200)

# Create a data frame
data <- data.frame(x = car_age, y = price)

# Perform simple linear regression
linear_model <- lm(y ~ x, data = data)

# Print summary of the regression
summary(linear_model)

# Plot the data and regression line
plot(data$x, data$y, main = "Simple Linear Regression", xlab = "X", ylab = "Y")
abline(linear_model, col = "red")



# New data for prediction
new_car_age <- c(6, 8, 10)

# Create a data frame for the new data
new_data <- data.frame(x = new_car_age)

# Use the linear model to make predictions
predictions <- predict(linear_model, newdata = new_data)

# Print the predictions
cat("Predictions for new car ages:", predictions, "\n")

# Plot the original data, regression line, and predictions
plot(data$x, data$y, main = "Simple Linear Regression with Predictions", xlab = "Car Age", ylab = "Price")
abline(linear_model, col = "red")
points(new_data$x, predictions, col = "blue", pch = 16)

#5.residulas
library(ggplot2)

# Sample data
x <- c(1, 2, 3, 4, 5)
y <- c(2, 5, 3, 8, 7)

# Fit linear regression model
model <- lm(y ~ x)

# Extract residuals
residuals <- resid(model)

# Plot residuals vs fitted values
ggplot(data.frame(Fitted = fitted(model), Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

# Shapiro-Wilk normality test on residuals
shapiro_test <- shapiro.test(residuals)

# Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals)
