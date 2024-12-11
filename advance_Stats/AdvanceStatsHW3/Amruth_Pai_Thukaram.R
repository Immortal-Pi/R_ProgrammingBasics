rm(list=ls())
library(readxl)

#Problem 1 (Set-1)
#Logistic Regression 

hospital.df=read_excel('HW3-F24-6359.xlsx',sheet='Logistics')
head(hospital.df)
attach(hospital.df)
reg1=glm(Hospital~ Age+Health+Vaccine,family = 'binomial')
reg1
hos.coef=as.numeric(coef(reg1))

#question 1
hos.coef[1]

#question 2
hos.coef[4]

#question 3
hos.coef[3]

#question 4
hos.coef[2]


#question 5
odds58=exp(hos.coef[1]+hos.coef[2]*58+hos.coef[3]*9+hos.coef[4]*1)
ans=odds58/(1+odds58)
ans


#question 6
odds55=exp(hos.coef[1]+hos.coef[2]*55+hos.coef[3]*7+hos.coef[4]*0)
odds55
ans=odds55/(1+odds55)
ans

#question 7
odds=exp(hos.coef[2]*(51-50))
odds

#question 8
oods=exp(hos.coef[2]*40+ hos.coef[3]*(3-7)+hos.coef[4]*1)
oods






#Problem 2(set2)
#ANOVA
class.df=read_excel('HW3-F24-6359.xlsx',sheet = 'ANOVA')
head(class.df)
library(tidyr)

#format the data for aov algorithm 
reshaped_data=pivot_longer(class.df,cols = starts_with('Test'),
                           names_to = 'Test',
                           values_to = 'Score')
reshaped_data$Section=as.factor(reshaped_data$Section)
reshaped_data$Test=as.factor(reshaped_data$Test)
attach(reshaped_data)
av1=aov(Score~Section+Test+Section:Test)
anova_summary=summary(av1)

tapply(Score, list(Section,Test),mean)
interaction.plot(Test,Section,Score,lwd=2,col = 1:3,main='Test vs Section')
interaction.plot(Section,Test,Score,lwd=2,col = 1:3,main='Section vs Test')

# question 1
pvalue <- anova_summary[[1]]$`Pr(>F)`[1]
paste('the P-value for Sections is ',pvalue)


#question 2
pvalue <- anova_summary[[1]]$`Pr(>F)`[2]
paste('the P-value for Test is ',pvalue)


#question 4
fstat <- anova_summary[[1]]$`F value`[1]
paste('the F-stat for Test is ',fstat)





#Problem 3(Set-3)
#log Transformation
library(moments)
#library(e1071)
solve.df=read_excel('HW3-F24-6359.xlsx',sheet = 'Log')
head(solve.df)

#a]
#hist
hist(solve.df$Time)
qqnorm(solve.df$Time)
qqline(solve.df$Time)

boxplot(solve.df$Time)
s=skewness(solve.df$Time)

#conclusion right skewed data
paste('a] conclusion : - its a right skewed data with skewness ',s)

#log transform
solve.df.log=log(solve.df$Time)
#solve.df.log
summary(solve.df.log)
skew=skewness(solve.df.log)
par(mfrow=c(3,2))
hist(solve.df$Time, main = 'Original Data',xlab = 'Value')
hist(solve.df.log, main='Log-Transformed Data', xlab = 'Value')

qqnorm(solve.df$Time,main = 'Original Data')
qqline(solve.df$Time)
qqnorm(solve.df.log,main = 'Log-Transformed Data')
qqline(solve.df.log)
boxplot(solve.df$Time,main = 'Original Data')
boxplot(solve.df.log,main = 'Log-Transformed Data')
paste('b] After log transformation the values look normally distributed. The skewness after log transformation is ',skew)

#c]
#t distrubution 
log_mean=mean(solve.df.log)
log_sd=sd(solve.df.log)
n=length(solve.df.log)  #sample size 
paste('question 13: mean of the log-transformed data is ',log_mean)
paste('question 14: skewness of the sample after log-transformation ',skew)
paste('question 15: standard deviation of the log-transformed data is ',log_sd)

alpha=0.05
t_critical=qt(1-alpha/2,df=n-1)
margin_error=t_critical*(log_sd/sqrt(n))
paste('question 16: standard error if the log-transformed data ',log_sd/sqrt(n))

lower_bound=log_mean-margin_error
upper_bound=log_mean+margin_error

cat("95% Confidence Interval: [", lower_bound, ", ", upper_bound, "]\n")

paste('question 17: upper limit of Confidence Interval for a Confidence level before reverse transform is',upper_bound)

#reverse transform
original_lower=exp(lower_bound)
original_upper=exp(upper_bound)
cat("95% Confidence Interval in original scale: [", original_lower, ", ", original_upper, "]\n")
paste('question 18: UCL after reverse transformation is ',original_upper)

#using t-test
t.test(solve.df.log,conf.level = .95)








#Problem 4 (set-4)
#t-test

social_media.df=read_excel('HW3-F24-6359.xlsx',sheet = 't-test')
head(social_media.df)

social_df=pivot_longer(social_media.df, cols = c('Male','Female'),
                       names_to = 'Gender', values_to = 'Time'
                       )
#variance test (F-test)
social_df$Gender=as.factor(social_df$Gender)
attach(social_df)
variance_test=var.test(Time~Gender,var.equal=TRUE)
variance_test
#0.909>0.05 variance are equal i.e there is inssuficient evidence to conclude that the variance of the 2 groups are different 
paste('4] p value 0.094>0.05 variance are equal i.e there is inssuficient evidence to conclude that the variance of the 2 groups are different')

#t-test 
t_test_equal=t.test(Time~Gender,var.equal=TRUE)
t_test_equal
#t_test_unequal=t.test(Time~Gender,var.equal=FALSE)
#t_test_unequal
