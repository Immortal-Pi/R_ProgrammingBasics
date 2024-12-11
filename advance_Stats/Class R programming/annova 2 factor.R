
rm(list = ls())
library(readxl)

#first page 

teachers.df=read_excel('ANOVA-2-Teachers.xlsx',sheet = 'R-1')
teachers.df
attach(teachers.df)
a1=aov(Score ~ Teacher+Method+Teacher:Method )
summary(a1)

# mean table 
tapply(Score, list(Method,Teacher),mean)
interaction.plot(Teacher,Method,Score,lwd=2,col=1:3,main='Method vs Reachers')
interaction.plot(Method,Teacher,Score,lwd=3,col=1:3,main='Reachers vs Method')
detach()

#second page
teachers.df2=read_excel('ANOVA-2-Teachers.xlsx',sheet = 'R-2')
teachers.df2
attach(teachers.df2)
a2=aov(Score~Teacher+Method+Teacher:Method)
summary(a2)
#mean table
tapply(Score,list(Method,Teacher),mean)
interaction.plot(Teacher,Method,Score,lwd=2,col = 1:3,main='Method vs Reachers')
interaction.plot(Method,Teacher,Score,lwd=3,col = 1:3,main='Reachers vs Method')
detach(teachers.df2)

#drugs 
drugs.df=read_excel('ANOVA-2-Teachers.xlsx',sheet='Transformed_Drug_Data')
drugs.df
attach(drugs.df)
a3=aov(Score~Gender+Drug+Gender:Drug)
summary(a3)
tapply(Score,list(Gender,Drug),mean)
interaction.plot(Gender,Drug,Score,lwd=3,col=1:3,main='Gender vs Drug')
interaction.plot(Drug,Gender,Score,lwd=2,col=1:3,main='Drug vs gender')
