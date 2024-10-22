#### Table 5.1

# package forecast is required to evaluate performance
library(forecast)
set.seed(1)

# load file
toyota.corolla.df <- read.csv("ToyotaCorolla.csv")
View(toyota.corolla.df)
# randomly generate training and validation sets
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

# run linear regression model
reg <- lm(Price~., data=toyota.corolla.df[,-c(1,2,8,11)], subset=training,
          na.action=na.exclude)
pred_t <- predict(reg, na.action=na.pass)
pred_v <- predict(reg, newdata=toyota.corolla.df[validation,-c(1,2,8,11)],
                  na.action=na.pass)


## evaluate performance
# training
accuracy(pred_t, toyota.corolla.df[training,]$Price)
# validation
accuracy(pred_v, toyota.corolla.df[validation,]$Price)

##################
# Histogram and Boxplot of prediction errors
training_data <- toyota.corolla.df[training,]
validation_data <- toyota.corolla.df[validation,]
training_data$pred_t <- pred_t
validation_data$pred_v <- pred_v
training_data$error <- training_data$Price - training_data$pred_t
validation_data$error <- validation_data$Price - validation_data$pred_v
par(mfrow=c(1,2))
hist(training_data$error)
hist(validation_data$error)
boxplot(training_data$error)
boxplot(validation_data$error)

valdata <- toyota.corolla.df[validation,]
##################
valdata

#### Figure 5.2


# remove missing Price data
toyota.corolla.df <-     
  toyota.corolla.df[!is.na(toyota.corolla.df[validation,]$Price),]

# generate random Training and Validation sets
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(toyota.corolla.df$Id, 400)

# regression model based on all numerical predictors
reg <- lm(Price~., data = toyota.corolla.df[,-c(1,2,8,11)], subset = training)

# predictions
pred_v <- predict(reg, newdata = toyota.corolla.df[validation,-c(1,2,8,11)])
prediction=predict(reg, newdata= toyota.corolla.df[validation,-c(1,2,8,11)])
# load package gains, compute gains (we will use package caret for categorical y later)
gain=gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)],prediction[!is.na(pred_v)])
gain
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs), type='l')
lines(c(0,sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]),col='gray',lty=2)
library(gains)
price
gain <- gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)], pred_v[!is.na(pred_v)])

# cumulative lift chart
options(scipen=999) # avoid scientific notation

# First extract non-missing prices

plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")

#plot(gain$mean.resp)
# baseline
lines(c(0,sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]), col="gray", lty=2)

# Decile-wise lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")




#### Table 5.5
#install future.apply --- future.apply_1.11.0.tar.gz 
library(caret, pos = 'future.apply_1.11.0.tar.gz')
library(e1071)

owner.df <- read.csv("ownerExample.csv")
owner.df$Class <- as.factor(owner.df$Class)
owner.df
#ConfusionMatrix(predicted, actual)
pre=confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')), 
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')), 
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')), 
                owner.df$Class)
library(reshape2)
pre.melt=melt(pre$table)
library(ggplot2)

heatmap(pre$table)
ggplot(pre$table) +geom_point(data = pre$table)
pre$table

#### Figure 5.4

# replace data.frame with your own
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

pl# compute auc
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

