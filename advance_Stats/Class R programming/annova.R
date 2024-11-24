rm(list = ls())
library(readxl)

# stock dependency on age 
data.df=read_excel('ANOVA-S.xlsx',sheet='Stocks-R')
names(data.df)
attach(data.df)
head(data.df)
sum(data.df[2])

boxplot(Portfolio ~ AgeGroup)

anov1=aov(Portfolio ~ AgeGroup)
summary(anov1)
anov1$coefficients
anov1$residuals


#diet restriction and longevity 
diet.df=read_excel('ANOVA-S.xlsx',sheet = 'Case0501-R')
diet.df
attach(diet.df)
head(diet.df)
sum(diet.df[1])

boxplot(Lifetime ~ Diet)

anov2=aov(Lifetime ~ Diet)
summary(anov2)


#cholestrol 
health.df=read_excel('ANOVA-S.xlsx',sheet = 'Cholestrol-R')
health.df
boxplot(health.df$Reduction ~ health.df$Drug )
anov3=aov(health.df$Reduction ~ health.df$Drug )
summary(anov3)


#random block analysis 
# segregating based on age group
anno4=aov(health.df$Reduction ~ health.df$Group + health.df$Drug)
summary(anno4)

