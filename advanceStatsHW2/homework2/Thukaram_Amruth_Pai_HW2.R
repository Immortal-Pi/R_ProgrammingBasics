#install.packages('dplyr')
#install.packages('readxl')
#install.packages('moments')
#library(dplyr)
#library(readxl)
#library(moments)

name1='Thukaram_Amruth_Pai'
csvfile <- paste(name1,"_HW2.csv",sep=""); csvfile
sink(csvfile)
cat("NAME",  sep = ","   ,  "Thukaram Amruth Pai", "\n")
cat("NETID" ,  sep = ","   , "AXT230147", "\n")
cat("--------------------------------", "\n")
#sink()



#PART A
#Binomial Distribution
#The Probability of a student starting the HW on the last day is 23%.  
#There are 67 students in my class.  Answer the following questions.  

#1 Probability that exactly 12 students will start on the last day.
na=67
pa=0.23
ka=12
prob_exactly12=dbinom(ka,na,pa)
#prob_exactly12
cat("1",sep = ",",prob_exactly12,"\n")

#2.) Probability that between 15 to 18 will start the HW on the last day.
na=67
pa=0.23
prob_between_15_to_18=pbinom(18,na,pa)-pbinom(14,na,pa)
#prob_between_15_to_18
cat("2",sep = ",",prob_between_15_to_18,"\n")



#PART B
#Normal Distribution
#The average weight of 135 apples that you have is 83 grams and the std dev is 11 grams.  
#Given the weights are normally distributted, answer the following questions.

#3 What is the probability of an apple weighing less than 90 grams?
mean_b=83
sd_b=11
n_apples=135
prob_less_than_90=pnorm(90,mean_b,sd_b)
#prob_less_than_90
cat("3",sep = ",",prob_less_than_90,"\n")

#4 How many apples are expected to be between 79 and 92 grams?
prob_between_79_to_92=pnorm(92,mean_b,sd_b)-pnorm(79,mean_b,sd_b)
ans4=prob_between_79_to_92*n_apples
cat("4",sep = ",",ans4,"\n")


#5 If top 12% of the apples are considered to be heavy, what is the cut off weight?
headvy_apples_cutoff=qnorm(0.88,mean_b,sd_b)
cat("5",sep=",",headvy_apples_cutoff,"\n")


#6 What is the cut-off weight for the bottom 8% of apples (lightest)?
light_apples_cutoff=qnorm(0.08,mean_b,sd_b)
cat("6",sep=",",light_apples_cutoff,"\n")



#PART C
#Normal to Binomial
#There are 90 people in the restaurant.  The probability of someone ordering a 
#drink with the food is 50%.  Use Normal approximation of Binomial Distribution
#to answer the following 6 questions.

#7. What is the mean of the Normal distribution?
n=90
p=0.5
mean=n*p
cat("7",sep=",",mean,"\n")

#8. What is the standard deviation of the normal distribution?
sd=sqrt(n*p*(1-p))
cat("8",sep=",",sd,"\n")

#9. What is the probability that exactly 48 people will order a drink?
prob_48=pnorm(48.5,mean,sd)-pnorm(47.5,mean,sd)
cat("9",sep=",",prob_48,"\n")

#10. What the probability that more than 43 people will order a drink?
prob_more_than_43=1-pnorm(43.5,mean,sd)
cat("10",sep=",",prob_more_than_43,"\n")

#11. What is the probability that less than 50 people will order a drink?
prob_less_than_50=pnorm(49.5,mean,sd)
cat("11",sep=",",prob_less_than_50,"\n")



#PART D
#Confidence Interval
#A BUAN 6359 studernt is curious as to how many hours each week a JSOM 
#student spends in studying for the classes.  She took a random sample of 
#25 students.  The mean number of hours was 11.6 and the standard deviation 
#was 3.8 hours.  Based on this, create a 95% Confidence Interval for the 
#population mean and answer the following questions.   
#Note: Use t-distribution since we do not know σ

#12 the margin error
n=25
sample_mean=11.6
sd=3.8
CI=0.95

df=n-1
t_critical=qt(1-(1-CI)/2,df)
margin_error=t_critical*(sd/sqrt(n))
cat("12",sep=",",margin_error,"\n")

#13.  The lower cut-off
lower_cutoff=sample_mean-margin_error
cat("13",sep=",",lower_cutoff,"\n")

#14. The upper cut-off
upper_cutoff=sample_mean+margin_error
cat("14",sep=",",upper_cutoff,"\n")


#You took a sample of 160 parts from a warehouse and found that 18% were 
#defective.  With a Confidence level of 93%, what can you say about the 
#thousands of parts sitting in the warehouse?  Answer the questions below.

#15. What is standard error of sample proportion?
n=160
p=0.18
CI=0.93
standard_error=sqrt(p*(1-p)/n)
cat("15",sep=",",standard_error ,"\n")

#16.  What is the margin of error?
z_critical=qnorm(1-(1-CI)/2)
margin_error=z_critical*standard_error
cat("16",sep=",",margin_error  ,"\n")

#17.  What will be lower cut-off?
lower_cutoff=p-margin_error
cat("17",sep=",",lower_cutoff,"\n")


#18. What will be the upper cut-off?
upper_cutoff=p+margin_error
cat("18",sep=",",upper_cutoff,"\n")



#PART E
# Generate a normal distribution population with mean = 100, sigma = 18, n  = 90
# Assign it to a vector ndp  
# Make all the numbers of vector ndp  integer
# For ndp, find the following  and print the values and label in your csv file.
mean=100
sigma=18
n=90
ndp=rnorm(n,mean =mean,sd=sigma)
ndp=as.integer(ndp)

#19. Mean
mean_ndp=mean(ndp)
cat("19",sep=",",mean_ndp,"\n")

#20. Population Std Dev (R gives sample std dev.  Convert it into population std dev)
pop_std=sd(ndp)*sqrt((n-1)/n)
cat("20",sep=",",pop_std,"\n")

#21. Skewness
skewness=skewness(ndp)
cat("21,", skewness, "\n")

sink()



# Partition the graph area into 4 parts (2 rows and 2 columns). This will print 
#  all 4 graphs below on one screen. 
par(mfrow=c(2,2),mar = c(3, 3, 2, 2))

# density plot of ndp
plot(density(ndp),main = 'Density plot of NDP', xlab='NDP values',ylab='Density')

# boxplot of ndp
boxplot(ndp,main="Boxplot of NDP", xlab="NDP Values")

# Histogram of ndp
hist(ndp, main="Histogram of NDP", xlab="NDP Values", ylab="Frequency", col="lightgreen", border="black")

# qqplot of ndp (and also qqline)
qqnorm(ndp)
qqline(ndp,col='red')
