rm(list=ls())
library(readxl)
#Problem 1 (Set-1)
#Logistic Regression 

hospital.df=read_excel('HW3-F24-6359.xlsx',sheet='Logistics')
head(hospital.df)
attach(hospital.df)
reg1=glm(Hospital~ Age+Health+Vaccine)
reg1
