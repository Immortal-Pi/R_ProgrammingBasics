

#################################################
# Plot probability against odds and logit
#################################################
# Define a sequence of probabilities from 0 to 1
p <- seq(0.01, 0.99, by = 0.01)

# Calculate odds
odds <- p / (1 - p)

# Calculate logit
logit <- log(odds)

# Plot 1: Odds as a function of probability
plot(p, odds, type = "l", col = "blue", lwd = 2, 
     xlab = "Probability of Success (p)", ylab = "Odds", 
     main = "Odds as a function of Probability")
 

# Plot 3: Logit as a function of probability
plot(p, logit, type = "l", col = "red", lwd = 2, 
     xlab = "Probability of Success (p)", ylab = "Logit", 
     main = "Logit as a function of Probability")



#################################################
# Plot sigmoid function for a range of values of z
#################################################
# Define the sigmoid (logistic) function
logistic_function <- function(z) {
  return(1 / (1 + exp(-z)))
}

# Create a range of z values, including both negative and positive values
z_values <- seq(-10, 10, by = 0.1)

# Calculate the corresponding probabilities using the logistic function
p_values <- logistic_function(z_values)

# Plot the logistic function
plot(z_values, p_values, type = "l", col = "blue", lwd = 2, 
     main = "Logistic (Sigmoid) Function", 
     xlab = "z (linear combination of predictors)", 
     ylab = "Probability (p)",
     ylim = c(0, 1))

# Add grid lines
grid()


#################################################
# Skewed sigmoid function for a range of values of z
#################################################
# Define the sigmoid (logistic) function
logistic_function <- function(z) {
  return(1 / (1 + exp(-z)))
}

# Create skewed input values for z, focusing on positive values
z_values_skewed_positive <- seq(2, 10, by = 0.1)

# Calculate the corresponding probabilities
p_values_skewed_positive <- logistic_function(z_values_skewed_positive)

# Plot the sigmoid function for skewed positive input values
plot(z_values_skewed_positive, p_values_skewed_positive, type = "l", col = "blue", lwd = 2, 
     main = "Sigmoid Function (Skewed Positive z values)", 
     xlab = "z (linear combination of predictors)", 
     ylab = "Probability (p)",
     ylim = c(0, 1))
grid()



#################################################
# Exhaustive search to select best predictors
#################################################
# Load necessary packages
library(leaps)

# Load your data
bank.df <- read.csv("Lecture 6/UniversalBank.csv")

# Remove columns that aren't needed, like ID or zip code
bank.df <- bank.df[, -c(1, 5)] # Modify this as per your dataset if needed

# Treat Education as a categorical variable
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# Perform exhaustive search using regsubsets()
exhaustive.search <- regsubsets(Personal.Loan ~ ., data = bank.df, nbest = 1, nvmax = NULL, method = "exhaustive")

# Summarize the results
exhaustive.summary <- summary(exhaustive.search)

# View the models selected
exhaustive.summary$which

# View the R-squared for each model
exhaustive.summary$rsq

# View the adjusted R-squared for each model
exhaustive.summary$adjr2

# View Mallows' Cp for each model
exhaustive.summary$cp


#################################################
# VIF Calculation
#################################################
# Load necessary packages
install.packages("car")  # Install 'car' package if not already installed
library(car)             # Load the package

# Load UniversalBank dataset (assuming the file is in your working directory)
universal_bank <- read.csv("Lecture 6/UniversalBank.csv")

# View the first few rows of the dataset to understand its structure
head(universal_bank)

# Remove irrelevant columns (e.g., ID, ZIP Code, etc.) for the regression model
# For example, let's assume that 'ID' and 'ZIP Code' are irrelevant for modeling
universal_bank <- universal_bank[, !names(universal_bank) %in% c("ID", "ZIP.Code")]

# Fit a multiple linear regression model (select dependent and independent variables)
# Example: Let's say we're predicting 'Personal.Loan' and using all other columns as predictors
# (In practice, logistic regression would be used for a binary outcome like Personal.Loan, but we can demonstrate VIF with a linear model)
model <- lm(Personal.Loan ~ ., data = universal_bank)

# Calculate VIF for the model
vif_values <- vif(model)

# Print the VIF values for all predictors
print(vif_values)

# Optional: To see predictors with VIF > 5 or 10 (significant multicollinearity)
vif_values[vif_values > 5]  # Adjust the threshold if needed



#################################################
# Add interaction terms to the model
#################################################

model_with_interactions <- glm(Personal.Loan ~ Income * Education + Age * Experience + 
                                 Family * Income + Securities.Account * Income +
                                 CD.Account * Income + Education * Online +
                                 CreditCard * Income, 
                               data = universal_bank, family = "binomial")

# View summary of the model
summary(model_with_interactions)



#################################################
# Sigmoid Curve
#################################################
# Define the logistic function
logistic_function <- function(z) {
  return(1 / (1 + exp(-z)))
}

# Create a range of income values
income <- seq(0, 250, length.out = 100)

# Logistic regression coefficients
beta_0 <- -6.217130
beta_income <- 0.037568

# Calculate the linear combination (z)
z <- beta_0 + beta_income * income

# Calculate probabilities using the logistic function
p_values <- logistic_function(z)

# Calculate odds (odds = p / (1 - p))
odds <- p_values / (1 - p_values)

# Plot sigmoid curve (Probability vs Income)
par(mfrow = c(1, 2))

plot(income, p_values, type = "l", col = "blue", lwd = 2,
     main = "Sigmoid Curve: Probability vs. Income",
     xlab = "Income (in $000s)", ylab = "P(Personal Loan = Yes)")

# Plot odds curve (Odds vs Income)
plot(income, odds, type = "l", col = "green", lwd = 2,
     main = "Odds as a Function of Income",
     xlab = "Income (in $000s)", ylab = "Odds")


