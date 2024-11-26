library(moments)
library(readxl)

car.df=read_excel('Regrsssion-Used-cars.xlsx',sheet = 'Sheet1')
head(car.df)
attach(car.df)
plot(Odometer, Price, col='blue', main='Price vs Odometer',
    abline(lm(Price~Odometer)), cex=1.3, pch=16, xlab='Odometer',ylab='Price')

qqnorm(Price)
qqline(Price)
skewness(Price)

reg1=lm(Price~Odometer)
summary(reg1)


#regression analysis
res1=resid(reg1)
skewness(res1)
hist(res1)

plot(Odometer,res1)
abline(0,0)
qqnorm(res1)
qqline(res1)



#regression using categorical varaible 

rm(list=ls())
#install.packages('e1071',dep=TRUE)
library(moments)
library(readxl)
data.df=read_excel('Used-Car-Categorical.xlsx')
head(data.df)
colnames(data.df)=c('Price','Odometer','I-1','I-2','I-3')
attach(data.df)
reg1=lm(Price~Odometer+`I-1`+`I-2`)
#colnames(data.df)
summary(reg1)


#residual analysis
res1=resid(reg1)
skewness(res1)
hist(res1)
plot(Odometer,res1)
abline(0,0)

qqnorm(res1)
qqline(res1)





#log transform for non normal distribution 
rm(list=ls())
library(readxl)
library(e1071)
bacteria.df=read_excel('Log-Bacteria-Regression.xlsx',sheet='Data')
head(bacteria.df)
attach(bacteria.df)

#without log transform
x=Seconds
y= `Bacteria Count`
plot(x,y)
hist(y)
skewness(y)

reg1=lm(y~x)
summary(reg1)

res1=resid(reg1)
skewness(res1)
hist(res1)
qqnorm(res1)
qqline(res1)

plot(x,y)
abline(reg1)


#after trasformation 
y=log(`Bacteria Count`)
hist(y)
skewness(y)

reg2=lm(y~x)
summary(reg2)
res2=resid(reg2)
skewness(res2)
hist(res2)


qqnorm(res2)
qqline(res2)

#reverse transform 
b1=coef(reg2)[2]

#bacteria goes down to 90.64# every second
y1=exp(b1);y1

#bacteria goes down to 11.64% every 10 secs 
y10=exp(b1*10);y10

#bacteria goes down to 3.97% every 15 secs 
y15=exp(b1*15);y15

#bacteria count after 14 secs prediction
ans=as.numeric(coef(reg2)[1])+as.numeric(coef(reg2)[2])*14
exp(ans)

