car.df=read.csv('ToyotaCorolla(1).csv')
colnames(car.df)
selected.var=c(3,4,7,8,9,10,12,13,14,17,18)

# View(car.df[8])
set.seed(1)
train.index=sample(c(1:1000),600)

train.df=car.df[train.index,selected.var]
valid.df=car.df[-train.index,selected.var]

car.lm=lm(Price~.,data = train.df)
car.lm

#using options() so that we dont see the scientific notation 
options(scipen = 999)
summary(car.lm)
length(train.df$Price)


train.df$Fuel_Type=factor(train.df$Fuel_Type)
View(train.df)


pred_t=predict(car.lm,train.df)
pred_v=predict(car.lm,valid.df)

some.residuals=valid.df$Price[1:20]-pred_v[1:20]
data.frame('Predicted'=pred_v[1:20], 'actual'=valid.df$Price[1:20], 'residual'=some.residuals)
library(forecasts)
accuracy(pred_v,valid.df$Price)

all_residuals=valid.df$Price - pred_v
length(all_residuals[which(all_residuals>-1392 & all_residuals<1392)])/400
hist(all_residuals,breaks = 25, xlab = 'Residuals', main='')


plot(predict(car.lm),residuals(car.lm))
abline(h=0,lty=2)


#exhaustive search
library(leaps)
search=regsubsets(Price~.,data=train.df,nbest=1,nvmax=dim(train.df)[2],method = 'exhaustive')
search
sum=summary(search)
#which subset was selected 
sum$which
#r squared values of each subset 
sum$rsq
# adjusted R squared 
sum$adjr2
# mallows cp 
sum$cp

plot(sum$adjr2, type = "l", xlab = "Number of Predictors", ylab = "Adjusted R-squared", main = "Adjusted R-squared vs Number of Predictors")


#AIC as major factor
#forward  
car.lm.null=lm(Price~1,data = train.df)
car.lm.step=step(car.lm.null,scope = list(lower=car.lm.null,upper=car.lm), direction = 'forward')
summary(car.lm.step)
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

#backward 
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)  # Which variables were dropped?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

#both
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)  # Which variables were dropped/added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
price=valid.df$Price
gain=gains(valid.df$Price, car.lm.step.pred)
plot(c(0,gain$cume.pct.of.total*sum(price))~ c(0,gain$cume.obs))

#Lasso 
#install.packages('glmnet')
library(glmnet)
colnames(train.df)
x <- model.matrix(Price ~ ., train.df)[,-1]
y <- train.df$Price
lasso_model=glmnet(x,y,alpha=1)
cv_lasso=cv.glmnet(x,y,alpha=1)
cv_lasso
#identify the best lamda
plot(cv_lasso)
best_lambda=cv_lasso$lambda.min
best_lambda
lasso_best=glmnet(x,y,alpha=1,lambda = best_lambda)
lasso_best
coef(lasso_best)


ridge_model=glmnet(x,y,alpha=0)
cv_ridge=cv.glmnet(x,y,alpha=0)
cv_ridge
#identify the best lamda
plot(cv_ridge)
best_lambda=cv_ridge$lambda.min
best_lambda
ridge_best=glmnet(x,y,alpha=1,lambda = best_lambda)
ridge_best
coef(ridge_best)
