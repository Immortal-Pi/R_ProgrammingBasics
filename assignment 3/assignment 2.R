#dataset - Boston Housing 
# predict :- median house pricing [MEDV] 
# crime rate, pollution, number of rooms
library(MASS)
library(ggplot2)
library(caret)
library(Metrics)
#b]

house.df=read.csv('BostonHousing.csv')
head(house.df)
set.seed(1)
train.index=sample(c(1:length(house.df$CRIM)),length(house.df$CRIM)*0.70)
house.df.train=house.df[train.index,]
house.df.valid=house.df[-train.index,]
house.df.lm=lm(MEDV~CRIM+CHAS+RM,data = house.df.train)


summary(house.df.lm)
#house.df.pred=predict(house.df.lm,house.df.valid)
#house.df.pred
house.df.lm

#c]

formula=house.df.lm$coefficients['(Intercept)']+house.df.lm$coefficients['CRIM']*0.1+house.df.lm$coefficients['CHAS']*0+house.df.lm$coefficients['RM']*6
formula

#colnames(house.df)
library(dplyr)
new_observation=data.frame(CRIM=0.1,CHAS=0,RM=6)
result=predict(house.df.lm,newdata = new_observation,se.fit = TRUE)
paste('prediction error:',result$se.fit)
# the error is 424.03$ 
#house.df[,-c("CAT.MEDV")]

#d - i]
options(scipen = 999)
library(gplots)


house.df.mat=cor(house.df[,-c(14)])
# View(house.df[,-c(14)])
cor(house.df[,c(3,5,10)])

unique(house.df$RAD)
#d -ii]
heatmap.2(
  house.df.mat,
  cellnote = round(house.df.mat,2),
  notecol = 'black',
  trace='none',
  density.info = 'none',
  dendrogram = 'none',
  col=bluered(100),
  margins = c(10,10),
  breaks = seq(-1,1,length.out=101)
)

#d-iii

#we will remove NOX, INDUS and TAX to avoid multi collinearity

colnames(house.df)
house.df.updated=house.df[,-c(3,5,10)]
colnames(house.df.updated)
train.index=sample(c(1:length(house.df$CRIM)),length(house.df$CRIM)*0.70)
house.df.train.updated=house.df.updated[train.index,]
house.df.valid.updated=house.df.updated[-train.index,]



full_model=lm(MEDV ~.,data = house.df.train.updated)
# backward model
backward_model=step(full_model,direction = 'backward')
summary(backward_model)
#forward model
forward_model=step(full_model,direction = 'forward',scope = formula(full_model))
summary(forward_model)
#both model
both_model=step(full_model,direction = 'both')
summary(both_model)

## prediction
backwards_pred=predict(backward_model,newdata = house.df.valid.updated)
forward_pred=predict(forward_model,newdata = house.df.valid.updated)
both_pred=predict(both_model,newdata = house.df.valid.updated)

house.df.valid.updated$back_prob=backwards_pred
house.df.valid.updated$forw_prob=forward_pred
house.df.valid.updated$both_prob=both_pred
#house.df.valid.updated$CAT.MEDV=ifelse(house.df.valid.updated$MEDV>median(house.df.valid.updated$MEDV),1,0)
# Calculate performance metrics
calc_metrics <- function(actual, predicted) {
  rmse <- RMSE(actual,predicted)
  mape <- mape(actual,predicted)
  mean_error <- mean(actual-predicted)
  return(c(RMSE = rmse, MAPE = mape, Mean_Error = mean_error))
}

#difference 
backward_metrics=calc_metrics(house.df.valid.updated$MEDV,backwards_pred)
forward_metrics=calc_metrics(house.df.valid.updated$MEDV,forward_pred)
both_metrics=calc_metrics(house.df.valid.updated$MEDV,both_pred)
metrics.df=data.frame(
  Model=c('backward','forward','both'),
  RMSE=c(backward_metrics[1],forward_metrics[1],both_metrics[1]),
  MAPE=c(backward_metrics[2],forward_metrics[2],both_metrics[2]),
  Mean_Error=c(backward_metrics[3],forward_metrics[3],both_metrics[3])
)

library(caret)
print(metrics.df)
#house.df.valid.updated$CAT..MEDV <- levels(as.factor(house.df.valid.updated$CAT.MEDV))
lift.backward=lift(relevel(as.factor(house.df.valid.updated$CAT..MEDV),ref = 1)~house.df.valid.updated$back_prob)
xyplot(lift.backward,plot='gain')

lift.forward=lift(relevel(as.factor(house.df.valid.updated$CAT..MEDV),ref = 1)~house.df.valid.updated$forw_prob)
xyplot(lift.forward,plot='gain')

lift.both=lift(relevel(as.factor(house.df.valid.updated$CAT..MEDV),ref = 1)~house.df.valid.updated$both_prob)
xyplot(lift.both,plot='gain')

