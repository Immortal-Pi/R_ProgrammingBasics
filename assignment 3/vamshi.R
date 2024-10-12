library(MASS)
library(ggplot2)
library(caret)
library(Metrics)
data <- read.csv("BostonHousing.csv")
#1b
model <- lm(MEDV ~ CRIM + CHAS + RM, data = data)
summary(model)
predicted_values <- predict(model)
plot(data$MEDV, predicted_values, main="Actual vs Predicted MEDV",
     xlab="Actual MEDV", ylab="Predicted MEDV", pch=18, col="blue", abline(0,1, 
      col="red"))
#1C
new_observation <- data.frame(CRIM = 0.1,CHAS = 0,RM = 6) 
predicted_value <- predict(model, new_observation)
print(paste("Predicted Median House Price:", predicted_value))

#1D
#i. 
subset_data <- data[c("INDUS", "NOX", "TAX")]
correlation_matrix <- cor(subset_data)
print(correlation_matrix)

#ii.
numerical_predictors <- data[c("CRIM", "ZN", "INDUS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "LSTAT")]
correlation_matrix <- cor(numerical_predictors)
print(correlation_matrix)
correlation_pairs <- as.data.frame(as.table(correlation_matrix))
highly_correlated <- subset(correlation_pairs, abs(Freq) > 0.7 & Var1 != Var2)
print(highly_correlated)
#removing variables with high correlation
clean_data <- subset(data, select = -c(AGE, RAD, NOX))
print(head(clean_data))


#iii.
set.seed(123)
train_index <- createDataPartition(data$MEDV, p = 0.8, list = FALSE)
train_set <- clean_data[train_index, ]
validation_set <- clean_data[-train_index, ]

# Stepwise regression (BACKWARD)
full_model <- lm(MEDV ~ ., data = train_set)
step_backward <- step(full_model, direction = "backward")

# Stepwise regression (FORWARD)
null_model <- lm(MEDV ~ 1, data = train_set)
step_forward <- step(null_model, scope = formula(full_model), direction = "forward")

# Stepwise regression (BOTH)
step_both <- step(full_model, direction = "both")

pred_backward <- predict(step_backward, newdata = validation_set)
pred_forward <- predict(step_forward, newdata = validation_set)
pred_both <- predict(step_both, newdata = validation_set)

mean_error <- function(actual, predicted) {
  return(mean(actual - predicted))
}
actual_values <- validation_set$MEDV

metrics <- data.frame(
  Model = c("Backward", "Forward", "Both"),
  
  RMSE = c(
    RMSE(pred_backward, actual_values),
    RMSE(pred_forward, actual_values),
    RMSE(pred_both, actual_values)
  ),
  
  MAPE = c(
    mape(actual_values, pred_backward),
    mape(actual_values, pred_forward),
    mape(actual_values, pred_both)
  ),
  
  Mean_Error = c(
    mean_error(actual_values, pred_backward),
    mean_error(actual_values, pred_forward),
    mean_error(actual_values, pred_both)
  )
)
print(metrics)

#chart for backward model
plot(sort(pred_backward), sort(actual_values), 
     main = "Lift Chart: Backward Model",
     xlab = "Predicted MEDV", ylab = "Actual MEDV",
     col = "blue", pch = 19)
lines(sort(actual_values), sort(actual_values), col = "red")

#chart for forward model
plot(sort(pred_forward), sort(actual_values), 
     main = "Lift Chart: Forward Model",
     xlab = "Predicted MEDV", ylab = "Actual MEDV",
     col = "blue", pch = 19)
lines(sort(actual_values), sort(actual_values), col = "red")

#chart for both model
plot(sort(pred_both), sort(actual_values), 
     main = "Lift Chart: Both Model",
     xlab = "Predicted MEDV", ylab = "Actual MEDV",
     col = "blue", pch = 19)
lines(sort(actual_values), sort(actual_values), col = "red")


