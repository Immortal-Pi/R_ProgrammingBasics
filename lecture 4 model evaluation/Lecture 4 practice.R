library(forecast)
set.seed(1)

toyoto.df=read.csv('ToyotaCorolla.csv')
head(toyoto.df)

training=sample(toyoto.df$Id,600)
validation=sample(setdiff(toyoto.df$Id,training),400)

#run linear regression model
reg=lm(Price~.,data=toyoto.df[,-c(1,2,8,11)], subset = training,na.action =na.exclude)
pred_t=predict(reg,na.action = na.pass)       
pred_v=predict(reg,newdata = toyoto.df[validation,-c(1,2,8,11)],na.action = na.pass)
pred_v
#View(toyoto.df[validation,-c(1,2,8,11)])

accuracy(pred_t,toyoto.df[training,]$Price)
accuracy(pred_v,toyoto.df[validation,]$Price)


#Histogram and Boxplot of prediction erros 
training_data=toyoto.df[training,]
validation_data=toyoto.df[validation,]
training_data$pred_t=pred_t
validation_data$pred_v=pred_v
training_data$error=training_data$Price-training_data$pred_t
#View(training_data)
validation_data$error=validation_data$Price-validation_data$pred_v
View(validation_data)

par(mfrow=c(1,2))
#histogram for error ranges 
hist(training_data$error)
hist(validation_data$error)

#boxplots
boxplot(training_data$error)
boxplot(validation_data$error)



#methods to remove missing values 
valdata=toyoto.df[validation,]
valdata

toyota.corolla.df=toyoto.df[!is.na(toyoto.df[validation,]$Price),]
View(toyota.corolla.df)

# generate random Training and Validation sets
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(toyota.corolla.df$Id, 400)

reg <- lm(Price~., data = toyota.corolla.df[,-c(1,2,8,11)], subset = training)
pred_v <- predict(reg, newdata = toyota.corolla.df[validation,-c(1,2,8,11)])

#package gains - effectiveness of the model
library(gains)
gain=gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)],pred_v[!is.na(pred_v)])
gain
options(scipen = 999)
price=toyoto.df[validation,]$Price[!is.na(toyoto.df[validation,]$Price)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs),  xlab = "Percentile", ylab = "Mean Response", main='Lift Chart', type='l')
# baseline
lines(c(0,sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]), col="gray", lty=2)



library(caret, pos = 'future.apply_1.11.0.tar.gz')
library(e1071)
library(reshape)
owner.df=read.csv('ownerExample.csv')
owner.df$Class=as.factor(owner.df$Class)
pre=confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')), 
                    owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')), 
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')), 
                pre.melt <- melt(pre$table))
                
        # Convert the melted confusion matrix to a data frame
pre.melt <- as.data.frame(pre.melt)
                
                # Plot the confusion matrix using ggplot2
heatmap(pre$table)
ggplot(pre$table) 
pre$table
df <- read.csv("liftExample.csv")

# create empty accuracy table
accT = c() 

# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(as.factor(1 * (df$prob > cut)), as.factor(df$actual))
  accT = c(accT, cm$overall[1])
}

# plot accuracy
plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright",  c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)



#### Figure 5.5
install.packages(pROC)
library(pROC)
r <- roc(df$actual, df$prob)
plot.roc(r)

# compute auc
auc(r)



#### Figure 5.6

# first option with 'caret' library:
library(caret)
lift.example <- lift(relevel(as.factor(actual), ref="1") ~ prob, data = df)
xyplot(lift.example, plot = "gain")

# Second option with 'gains' library:
library(gains)
df <- read.csv("liftExample.csv")
gain <- gains(df$actual, df$prob, groups=dim(df)[1])
plot(c(0, gain$cume.pct.of.total*sum(df$actual)) ~ c(0, gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l")
lines(c(0,sum(df$actual))~c(0,dim(df)[1]), col="gray", lty=2)




#### Figure 5.7

# use gains() to compute deciles. 
# when using the caret package, deciles must be computed manually. 

gain <- gains(df$actual, df$prob)
barplot(gain$mean.resp / mean(df$actual), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")
heatmap()

