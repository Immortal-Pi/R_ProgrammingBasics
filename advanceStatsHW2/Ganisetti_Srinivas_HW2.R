#rm(list=ls())

#print("NetID: sxg230253, LastName_FirstName: Ganisetti_Srinivas")
name1 <- "Ganisetti_Srinivas";

#install.packages("moments")
#library(moments)
#setwd("D:/UTDALLAS/Semester1/ADVSTATS_AS/HW2")

# Part-A 

csvfile <- paste(name1,"_HW2.csv",sep=""); csvfile
sink(csvfile)
cat("NAME",  sep = ","   ,  "Srinivas Ganisetti", "\n")
cat("NETID" ,  sep = ","   , "SXG230253", "\n")
cat("--------------------------------", "\n")

#PART-A
#1 Probability that exactly 12 students will start on the last day.
# Number of students in the class
na <- 67 
# Probability that a student starts the homework on the last day
pa <- 0.23 
# Number of students we want to find the probability for
ka <- 12 
# Calculate the probability of exactly 12 students starting on the last day
prob_exactly_12 <- dbinom(ka, na, pa)
# Print the result
cat("1",sep=",",prob_exactly_12,"\n")

#2 Probability that between 15 to 18 will start the HW on the last day.
# the cumulative probability up to 14
prob_15_to_18 <- pbinom(18, na, pa) - pbinom(14, na, pa)

# Print the result
cat("2",sep=",",prob_15_to_18,"\n")

#PART-B

# Define the mean and standard deviation of apple weights
mean_b <- 83  # Mean weight in grams
sd_b<- 11    # Standard deviation in grams

#3 What is the probability of an apple weighing less than 90 grams?
# Use the cumulative distribution function (pnorm) to find the probability
prob_less_than_90 <- pnorm(90, mean_b, sd_b)

# Print the result
cat("3",sep=",",prob_less_than_90,"\n")  # This will give the probability of an apple weighing less than 90 grams

# Question 4: How many apples are expected to weigh between 79 and 92 grams?
# Calculate the cumulative probabilities for both 79 grams and 92 grams
prob_between_79_92 <- pnorm(92, mean_b, sd_b) - pnorm(79, mean_b, sd_b)

# Then multiply the probability by the total number of apples to find the expected number of apples
n_apples <- 135  # Total number of apples
expected_apples <- n_apples * prob_between_79_92

# Print the result
cat("4",sep=",",expected_apples,"\n")  # This gives the expected count of apples weighing between 79 and 92 grams

#5 What is the cut-off weight for the top 12% of apples (heaviest)?
# We use the quantile function (qnorm) to find the weight at the 88th percentile (100% - 12%)
heavy_cutoff <- qnorm(0.88, mean_b, sd_b)

# Print the result
cat("5",sep=",",heavy_cutoff,"\n")  # This gives the weight above which the top 12% heaviest apples fall

#6 What is the cut-off weight for the bottom 8% of apples (lightest)?
# We use the quantile function (qnorm) to find the weight at the 8th percentile
light_cutoff <- qnorm(0.08, mean_b, sd_b)

# Print the result
cat("6",sep=",",light_cutoff,"\n") 


#PART-C
# Given: There are 90 people, and the probability of ordering a drink is 50%
n <- 90  # Number of trials (people)
p <- 0.50  # Probability of success (ordering a drink)

#7 What is the mean of the normal distribution?
# We use the formula for the mean of a binomial distribution: mean = n * p
mean <- n * p

# Print the result
cat("7",sep=",",mean,"\n") 

#8 What is the standard deviation of the normal distribution?
# We use the formula for standard deviation: sd = sqrt(n * p * (1 - p))
sd <- sqrt(n * p * (1 - p))

# Print the result
cat("8",sep=",",sd ,"\n") 

#9 What is the probability that exactly 48 people will order a drink?
# For normal approximation, we use P(47.5 < X < 48.5) to get the probability of exactly 48
prob_exactly_48 <- pnorm(48.5, mean, sd) - pnorm(47.5, mean, sd)

# Print the result
cat("9",sep=",",prob_exactly_48 ,"\n")

#10 What is the probability that more than 43 people will order a drink?
# We use the cumulative distribution function to calculate P(X > 43)
prob_more_than_43 <- 1 - pnorm(43, mean, sd)

# Print the result
cat("10",sep=",",prob_more_than_43 ,"\n")

#11 What is the probability that less than 50 people will order a drink?
# We use the cumulative distribution function to calculate P(X < 50)
prob_less_than_50 <- pnorm(50, mean, sd)

# Print the result
cat("11",sep=",",prob_less_than_50  ,"\n")

#PART-D
# Part 1: Confidence Interval for the population mean
n <- 25  # Sample size
mean <- 11.6  # Sample mean
sd <- 3.8  # Sample standard deviation
confidence_level <- 0.95  # Confidence level

# Calculate the degrees of freedom
df <- n - 1  # Degrees of freedom for t-distribution

# Get the critical value from the t-distribution
t_critical <- qt(1 - (1 - confidence_level) / 2, df)

# Question 12: The margin of error
margin_of_error <- t_critical * (sd / sqrt(n))

# Print the result
cat("12",sep=",",margin_of_error,"\n")

# Question 13: The lower cut-off of the confidence interval
lower_cutoff <- mean - margin_of_error

# Print the result
cat("13",sep=",",lower_cutoff  ,"\n")

#14 The upper cut-off of the confidence interval
upper_cutoff <- mean + margin_of_error

# Print the result
cat("14",sep=",",upper_cutoff  ,"\n")

# Part 2: Confidence Interval for the proportion of defective parts
n <- 160  # Sample size
p <- 0.18  # Sample proportion (defective parts)
confidence_level <- 0.93  # Confidence level

# Calculate the standard error for the proportion
#15 What is the standard error of sample proportion?

standard_error <- sqrt((p * (1 - p)) / n)

# Print the result
cat("15",sep=",",standard_error ,"\n")

# Get the z-critical value for 93% confidence level
z_critical <- qnorm(1 - (1 - confidence_level) / 2)

#16 The margin of error for the proportion
margin_of_error <- z_critical * standard_error

# Print the result
cat("16",sep=",",margin_of_error  ,"\n")

#17 The lower cut-off of the confidence interval for proportion
lower_cutoff <- p - margin_of_error

# Print the result
cat("17",sep=",",lower_cutoff  ,"\n")

#18 The upper cut-off of the confidence interval for proportion
upper_cutoff <- p + margin_of_error

# Print the result
cat("18",sep=",",upper_cutoff  ,"\n")

# PART-E

# Generate normal distribution with mean=100, sigma=18, n=90
mean_val <- 100
sigma_val <- 18
n <- 90

ndp <- rnorm(n, mean = mean_val, sd = sigma_val)

# Convert values to integers
ndp <- round(ndp)

# 19 Calculate mean
mean_ndp <- mean(ndp)

# Print the result
cat("19,", mean_ndp, "\n")

# 20 Calculate population standard deviation (use 'sd' for sample std dev, convert to population)
pop_std_dev <- sd(ndp) * sqrt((n-1) / n)

# Print the result
cat("20,", pop_std_dev, "\n")

# 21 Calculate skewness
skewness_ndp <- skewness(ndp)

# Print the result
cat("21,", skewness_ndp, "\n")

sink()

# PARTITION the graph area into 4 parts (2 rows and 2 columns)
par(mfrow=c(2,2), mar=c(3,3,2,1)) # 2 rows and 2 columns

# Density plot of ndp
plot(density(ndp), main="Density Plot of NDP", xlab="NDP Values", ylab="Density")

# Boxplot of ndp
boxplot(ndp, main="Boxplot of NDP", xlab="NDP Values")

# Histogram of ndp
hist(ndp, main="Histogram of NDP", xlab="NDP Values", ylab="Frequency", col="lightblue", border="black")

# QQ plot of ndp (with qqline)
qqnorm(ndp, main="QQ Plot of NDP")
qqline(ndp, col="red")




