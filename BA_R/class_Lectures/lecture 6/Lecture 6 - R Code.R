#################################################
# Plot probability against odds and logit
#################################################
# Define a sequence of probabilities from 0 to 1
p <- seq(0.01, 0.99, by = 0.01)
p
# Calculate odds
odds <- p / (1 - p)
odds
# Calculate logit
logit <- log(odds)
logit
# Plot 1: Odds as a function of probability
plot(p, odds, type = "l", col = "blue", lwd = 2, 
     xlab = "Probability of Success (p)", ylab = "Odds", 
     main = "Odds as a function of Probability")


# Plot 3: Logit as a function of probability
plot(p, logit, type = "l", col = "red", lwd = 2, 
     xlab = "Probability of Success (p)", ylab = "Logit", 
     main = "Logit as a function of Probability")


#################################################
# Single Predictor Model
#################################################
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , c(4, 10)]  #Select just one predictor

# partition data
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 

#fitted.values in logit.reg is the predicted probabilities for the training data
options(scipen=999) #Scientific Notation Penalty
summary(logit.reg)

#################################################
# Define the constants
intercept <- -6.217130
coefficient_income <- 0.037568
x <- 500 #Try different values of x

# Calculate y
y <-  1 / (1 + exp(-(intercept + coefficient_income * x)))

# Print the result
y


#################################################
# Plot p vs income for single predictor model
#################################################
# Define the intercept and the coefficient of Income
intercept <- -6.217130
coefficient_income <- 0.037568

# Define a sequence of x values (Income values) from 0 to 250
income_values <- seq(0, 250, by = 1)

# Calculate the probability using the logistic function
calculate_prob <- function(x) {
  1 / (1 + exp(-(intercept + coefficient_income * x)))
}

# Apply the function to the income values
probabilities <- sapply(income_values, calculate_prob)

# Plot the graph
plot(income_values, probabilities, type = "l", col = "blue", lwd = 2,
     xlab = "Income (x)", ylab = "Probability P(Personal Loan = Yes)",
     main = "Logistic Regression: Probability vs. Income")


#################################################
# Build Full Model and Evaluate Performance
#################################################
#### Table 10.2
#### Train the Model
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
# treat Education as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# partition data
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
#fitted.values in logit.reg is the predicted probabilities for the training data
options(scipen=999)
summary(logit.reg)

# Evaluating the trained model
train.probs <- predict(logit.reg, type = "response")  # Probabilities for training data
train.preds <- ifelse(train.probs > 0.5, 1, 0) # Binary classification based on a cutoff 0.5
library(caret)
confusionMatrix(factor(train.preds), factor(train.df$Personal.Loan))



#### Validate the Model
#### Table 10.3

# use predict() with type = "response" to compute predicted probabilities for the validation data
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$Personal.Loan[1:5], predicted = logit.reg.pred[1:5])

valid.preds <- ifelse(logit.reg.pred > 0.5, 1, 0)
confusionMatrix(factor(valid.preds), factor(valid.df$Personal.Loan))



#### Figure 10.3

library(gains)
# sort in decreasing order of probability and group them into 10 groups
gain <- gains(valid.df$Personal.Loan, logit.reg.pred, groups=10)

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Personal.Loan))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Personal.Loan))~c(0, dim(valid.df)[1]), lty=2)

# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

library(pROC)
roc_curve <- roc(valid.df$Personal.Loan, logit.reg.pred)
plot(roc_curve)
auc(roc_curve)


########################################################
# Flight Delay Example

delays.df <- read.csv("FlightDelays.csv")

########################################################
# Data Preparation 
########################################################
# create matrix for plot
delays.df$isDelay <- 1 * (delays.df$Flight.Status == "delayed")
View(delays.df)
# transform variables and create bins
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK, levels = c(1:7), 
                             labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))


##################################################
# Visualization and Exploration
##################################################
par(mfrow = c(2, 3))

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$DAY_WEEK), 
                  mean, rm.na = T)[,2], xlab = "Day of Week", ylab = "Average Delay", 
        names.arg = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$DEST), 
                  mean, na.rm = TRUE)[,2], xlab = "Destination", ylab = "Average Delay", 
        names.arg = c("LGA", "EWR", "JFK"))

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$CRS_DEP_TIME), 
                  mean, na.rm = TRUE)[,2], xlab = "Departure Time", ylab = "Average Delay", 
        names.arg = sort(unique(delays.df$CRS_DEP_TIME)))

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$CARRIER), 
                  mean, na.rm = TRUE)[,2], xlab = "Carrier", ylab = "Average Delay", 
        names.arg = c("US", "CO", "DH", "DL", "MQ", "OH", "RU", "UA"))

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$ORIGIN), 
                  mean, na.rm = TRUE)[,2], xlab = "Origin", ylab = "Average Delay", 
        names.arg = c("IAD", "BWI", "DCA"))

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$Weather), 
                  mean, na.rm = TRUE)[,2], xlab = "Weather", ylab = "Average Delay", 
        names.arg = c("0", "1"))


agg <- aggregate(delays.df$isDelay, 
                 by = list(delays.df$DAY_WEEK, delays.df$CARRIER, delays.df$ORIGIN), 
                 FUN = mean, na.rm = TRUE)
m <- melt(agg)
names(m)[1:3] <- c("DAY_WEEK", "CARRIER", "ORIGIN")
install.packages('reshape2')
# plot with ggplot
# use facet_grid() with arguments scales = "free" and space = "free" to skip 
# missing values.
library(reshape)
library(ggplot2)
ggplot(m, aes(y = CARRIER, x = DAY_WEEK, fill = value)) +  geom_tile() + 
  facet_grid(ORIGIN ~ ., scales = "free", space = "free") + 
  scale_fill_gradient(low="white", high="black")



##################################################
# Train the Model
##################################################

# create reference categories
delays.df$ORIGIN <- relevel(factor(delays.df$ORIGIN), ref = "IAD")
delays.df$DEST <- relevel(factor(delays.df$DEST), ref = "LGA")
delays.df$CARRIER <- relevel(factor(delays.df$CARRIER), ref = "US")
delays.df$DAY_WEEK <- relevel(delays.df$DAY_WEEK, ref = "Wed")

# create training and validation sets
selected.var <- c(10, 1, 8, 4, 2, 9, 14)
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

# run logistic model, and show coefficients and odds
lm.fit <- glm(isDelay ~ ., data = train.df, family = "binomial")
data.frame(summary(lm.fit)$coefficients, odds = exp(coef(lm.fit))) 

round(data.frame(summary(lm.fit)$coefficients, odds = exp(coef(lm.fit))), 5)

summary(lm.fit)

#### Figure 10.6

library(gains)
pred <- predict(lm.fit, valid.df)
gain <- gains(valid.df$isDelay, pred, groups=100)

plot(c(0,gain$cume.pct.of.total*sum(valid.df$isDelay))~
       c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$isDelay))~c(0, dim(valid.df)[1]), lty=2)

confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(valid.df$isDelay))

#### Table 10.8

# fewer predictors
delays.df$Weekend <- delays.df$DAY_WEEK %in% c("Sun", "Sat")
delays.df$CARRIER_CO_MQ_DH_RU <- delays.df$CARRIER %in% c("CO", "MQ", "DH", "RU")
delays.df$MORNING <- delays.df$CRS_DEP_TIME %in% c(6, 7, 8, 9)
delays.df$NOON <- delays.df$CRS_DEP_TIME %in% c(10, 11, 12, 13)
delays.df$AFTER2P <- delays.df$CRS_DEP_TIME %in% c(14, 15, 16, 17, 18)
delays.df$EVENING <- delays.df$CRS_DEP_TIME %in% c(19, 20)


set.seed(1)  # Set the seed for the random number generator for reproducing the 
# partition.
train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(delays.df)[1]), train.index)  
train.df <- delays.df[train.index, ]
valid.df <- delays.df[valid.index, ]

lm.fit <- glm(isDelay ~ Weekend + Weather + CARRIER_CO_MQ_DH_RU + MORNING  +  NOON + 
                AFTER2P + EVENING, data = train.df, family = "binomial")
summary(lm.fit)

library(gains)
pred <- predict(lm.fit, valid.df)
gain <- gains(valid.df$isDelay, pred, groups=100)

plot(c(0,gain$cume.pct.of.total*sum(valid.df$isDelay))~
       c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$isDelay))~c(0, dim(valid.df)[1]), lty=2)

# evaluate
#pred <- predict(lm.fit, valid.df)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(valid.df$isDelay))

