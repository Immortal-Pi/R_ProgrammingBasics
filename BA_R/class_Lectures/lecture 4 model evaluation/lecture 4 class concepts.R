library(forecast)
set.seed(1)

toyota.corolla.df=read.csv('ToyotaCorolla.csv')
head(toyoto.df)
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

reg=lm(Price~.,data=toyota.corolla.df[-c(1,2,8,11)],subset=training, na.action = na.exclude)
reg       

pred_t=predict(reg,na.action = na.pass)
pred_t
pred_v=predict(reg,newdata = toyota.corolla.df[validation,-c(1,2,8,11)], na.action = na.pass)
pred_v

accuracy(pred_t,toyota.corolla.df[training,]$Price)
accuracy(pred_v,toyota.corolla.df[validation,]$Price)
library(gains)

#lift curve
gain=gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)], pred_v[!is.na(pred_v)])
gain
price=toyota.corolla.df[validation,]$Price[!is.na(pred_v)]
plot(c(0,gain$cume.pct.of.total)*sum(price)~c(0,gain$cume.obs),type='l')
lines(c(0,sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]),col='grey',lty=2)

#Decile-wise lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth)


#aoc curve
r
