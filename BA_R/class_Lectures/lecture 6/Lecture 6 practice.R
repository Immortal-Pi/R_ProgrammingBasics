# plot the proabability again odd and logit 

p=seq(0.01, 0.99, by=0.01)

odds=p/(1-p)
odds
logit=log(odds)

par(mfrow=c(1,2))
plot(p,odds,type = 'l',col='blue',lwd=2, xlab = "Probability of Success (p)", 
     ylab = "Odds", 
     main = "Odds as a function of Probability" )
plot(p,logit,type = "l", col = "red", lwd = 2, 
     xlab = "Probability of Success (p)", ylab = "Logit", 
     main = "Logit as a function of Probability")



#single predictor model
bank.df=read.csv('UniversalBank.csv')
colnames(bank.df)
bank.df=bank.df[,c(4,10)]
View(bank.df)
set.seed(1)

train.index=sample(c(1:dim(bank.df)[1]),dim(bank.df)[1]*0.6)
train.df=bank.df[train.index,]
valid.df=bank.df[-train.index]

#logistic regression
logit.reg=glm(Personal.Loan~.,data=train.df,family='binomial')
options(scipen=999)
summary(logit.reg)


#try for a different value 
intercept=-6.217130
coefficient=0.037568
x=500
y=1/(1+exp(-(intercept+coefficient*x)))

#apply the below function to the income values 
calculate_probabilities<- function(x){
  1/(1+exp(-(intercept+coefficient*x)))
}
income_values=seq(0,250,by=1)
probabilities=sapply(income_values, calculate_probabilities)

plot(income_values,probabilities,type = "l", col = "blue", lwd = 2,
     xlab = "Income (x)", ylab = "Probability P(Personal Loan = Yes)",
     main = "Logistic Regression: Probability vs. Income")







#full model
bank.df=read.csv('UniversalBank.csv')
bank.df=bank.df[,-c(1,5)]
#categorical variables 
bank.df$Education=factor(bank.df$Education,levels = c(1,2,3),
                         labels = c("Undergrad", "Graduate", "Advanced/Professional"))

set.seed(1)
train.index=sample(c(1:dim(bank.df)[1]),dim(bank.df)[1]*0.6)
train.df=bank.df[train.index,]
valid.df=bank.df[-train.index,]

logit.reg=glm(Personal.Loan~.,data=train.df,family = 'binomial')
options(scipen = 999)
summary(logit.reg)


train.prob=predict(logit.reg,type='response')
train.prob
train.preds=ifelse(train.prob>0.5,1,0)
train.preds
library(caret)
confusionMatrix(factor(train.preds),factor(train.df$Personal.Loan))


##validate the model
logit.reg.pred=predict(logit.reg,newdata = valid.df[,-8],type = 'response')
data.frame(actual=valid.df$Personal.Loan, predicted=logit.reg.pred)
valid.pred=ifelse(logit.reg.pred>0.5,1,0)
confusionMatrix(factor(valid.pred),factor(valid.df$Personal.Loan))


par(mfrow=c(1,2))
#lift chart
library(gains)
gain=gains(valid.df$Personal.Loan,logit.reg.pred, groups = 10)
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Personal.Loan))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l"
     )
lines(c(0,sum(valid.df$Personal.Loan))~c(0,dim(valid.df)[1]),lty=2)

#deciles
height=gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints=barplot(height,names.arg=gain$depth, ylim = c(0,9), 
                  xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
text(midpoints, height+0.5, labels=round(height, 1), cex = 0.8)   


#roc curve
roc_curve=roc(valid.df$Personal.Loan, logit.reg.pred)
plot(roc_curve)
auc(roc_curve)
