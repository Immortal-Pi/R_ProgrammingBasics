car.df=read.csv('ToyotaCorolla(1).csv')
head(car.df)
car.df <- car.df[1:1000, ]
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)  
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)

library(forecast)
car.lm.pre=predict(car.lm,valid.df)
car.lm.pre
options(scipen=999,digits = 2)
some.residuals=valid.df$Price[1:20]-car.lm.pre[1:20]
data.frame("Predicted"=car.lm.pre[1:20], "Actual"=valid.df$Price[1:20],"Residual"=some.residuals)
options(scipen = 999,digits=3)
accuracy(car.lm.pre,valid.df$Price)



car.lm.pred=predict(car.lm,valid.df)
all.residuals=valid.df$Price-car.lm.pred
all.residuals
length(all.residuals[which(all.residuals>-1392 & all.residuals<1392)])/400
hist(all.residuals,breaks = 25,xlab='Residuals',main = '')

plot(predict(car.lm),residuals(car.lm))




library(leaps)
search=regsubsets(Price~.,data=train.df,nbest = 1,nvmax=dim(train.df)[2], method = 'exhaustive')
sum=summary(search)
sum$rsq
sum$adjr2
sum$cp
plot(sum$adjr2, type = "l", xlab = "Number of Predictors", ylab = "Adjusted R-squared", main = "Adjusted R-squared vs Number of Predictors")



#forward 
car.lm.null=lm(Price~.,data = train.df)
car.lm.step=step(car.lm.null,scope=list(lower=car.lm.null,upper=car.lm),direction='forward')
summary(car.lm.step)
car.lm.step.pred=predict(car.lm.step,valid.df)
accuracy(car.lm.step)

#backwards 
car.lm.step=step(car.lm,direction = 'backward')
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)  # Which variables were dropped/added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
