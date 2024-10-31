library(rpart)
library(rpart.plot)
library(caret)

ebay.df=read.csv('eBayAuctions.csv')
#View(ebay.df)

#Data Preprocessing

#convert Duration into categorical variable
ebay.df$Duration=as.factor(ebay.df$Duration)

#split the dataset into train and validation 
set.seed(1)
train.index=sample(c(1:dim(ebay.df)[1]),dim(ebay.df)[1]*0.6)
ebay.df.train=ebay.df[train.index,]
ebay.df.valid=ebay.df[-train.index,]


#a 
ebay.class.tree=rpart(Competitive.~.,data =ebay.df.train,method='class',control = rpart.control(minbucket = 50,maxdepth = 7,cp=0.001))
printcp(ebay.class.tree)

#select optimal cp 
optimal.cp=ebay.class.tree$cptable[which.min(ebay.class.tree$cptable[,'xerror']),'CP']
optimal.cp
pruned_ebay_tree=prune(ebay.class.tree,cp=optimal.cp)

#plot 
rpart.plot(pruned_ebay_tree,type=1,extra=106)

#rules
ebay_ct.rules=path.rpart(pruned_ebay_tree,node=rownames(pruned_ebay_tree$frame))
for (i in seq_along(ebay_ct.rules)) {
  cat(paste0("Rule ", i, ": ", paste(ebay_ct.rules[[i]], collapse = " -> "), "\n"))
}
#choosing high importance
ebay_importance=pruned_ebay_tree$variable.importance
ebay_importance

## we can drop sellerRating and consider others 

#b 
#praediction for validation 
ebay.df.valid_pred=predict(pruned_ebay_tree,ebay.df.valid,type='class')
confusionMatrix(ebay.df.valid_pred,as.factor(ebay.df.valid$Competitive.))
#accuracy is 77.69% which is bad and not practical for new auction 

#c 
#interesting patterns 
#OpenPrice serves as a significant split point, indicating that the initial price has a major impact on predictions.
#The threshold at ClosePrice = 10 is meaningful, showing distinct behaviors on either side.
#High OpenPrice values (>= 10.34) might predict consistent, high ClosePrices.

#unineresting patterns 
#Rules involving near-equal price splits (e.g., OpenPrice = 2.48 vs. ClosePrice = 2.455) do not add much value.
#The repeated split on OpenPrice >= 10.34 may indicate the model is too granular in its predictions.



#d
View(ebay.df)
ebay.df=ebay.df[,-c(1,2,3,4,5)]

train.index=sample(c(1:dim(ebay.df)[1]),dim(ebay.df)[1]*0.6)
ebay.df.train=ebay.df[train.index,]
ebay.df.valid=ebay.df[-train.index,]

ebay.class.tree=rpart(Competitive.~.,data =ebay.df.train,method='class',control = rpart.control(minbucket = 50,maxdepth = 7,cp=0.001))
printcp(ebay.class.tree)

#select optimal cp 
optimal.cp=ebay.class.tree$cptable[which.min(ebay.class.tree$cptable[,'xerror']),'CP']
optimal.cp
pruned_ebay_tree=prune(ebay.class.tree,cp=optimal.cp)

#plot
rpart.plot(pruned_ebay_tree,type=1,extra=106)

#rules
ebay_ct.rules=path.rpart(pruned_ebay_tree,node=rownames(pruned_ebay_tree$frame))
for (i in seq_along(ebay_ct.rules)){ 
  cat(paste0("Rule ", i, ": ", paste(ebay_ct.rules[[i]], collapse = " -> "), "\n"))
}
ebay_importance=pruned_ebay_tree$variable.importance
ebay_importance





#e
ggplot(ebay.df, aes(x = ClosePrice, y = OpenPrice, color = as.factor(Competitive.))) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("red", "blue"), 
                     labels = c("Noncompetitive", "Competitive")) +
  labs(title = "Auction Outcome Classification by OpenPrice and ClosePrice",
       x = "OpenPrice", y = "ClosePrice", color = "Auction Outcome") +
  xlim(0,15) +
  ylim(0,55) +
  # Add split lines based on tree structure
  geom_vline(xintercept = 3.615, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 10.34, linetype = "dashed", color = "black") +
 
  theme_minimal() 
#effective for low prices auctions but not fully account for acuions with higher openprice or close price
#split is reasonable for the main cluster of auctions but nay need refinement for better separation acreoss the entire range 




#f
#confusion matrix
#predict
ebay.df.valid_pred=predict(pruned_ebay_tree,ebay.df.valid,type='class')
conf_mat=confusionMatrix(ebay.df.valid_pred,as.factor(ebay.df.valid$Competitive.))
conf_mat
print(conf_mat$table)
# as per the confusion matrix the accuracy is 80% which is quite low 
#liftchart
library(gains)
library(dplyr)

ebay.df.valid_pred <- as.integer(as.character(ebay.df.valid_pred))
gain=gains(ebay.df.valid$Competitive.,ebay.df.valid_pred,groups = 10)
plot(c(0,gain$cume.pct.of.total*sum(ebay.df.valid$Competitive.))~c(0,gain$cume.obs),
     xlab='# cases', ylab = 'Cumulative', main='',type='l'
     )
lines(c(0,sum(ebay.df.valid$Competitive.))~c(0,dim(ebay.df.valid)[1]),lty=2)
# less area covered under the curve and the line not the best model for prediction 


#g
#seller (duration, opening price, ending day, currency)?
#A lower opening price could attract more bidders, especially if buyers perceive they might get a good deal. Conversely, a higher opening price may deter bidders
#ending day
#if the opening price is above 3.6









# 11.3

#a]
rm(list = ls())
toyoto.df.original=read.csv('ToyotaCorolla.csv')
#View(toyoto.df)
toyoto.df=toyoto.df.original[,c('Age_08_04', 'KM', 'Fuel_Type', 'HP', 'Automatic', 'Doors', 'Quarterly_Tax',
                       'Mfr_Guarantee', 'Guarantee_Period', 'Airco', 'Automatic_airco', 'CD_Player', 'Powered_Windows',
                       'Sport_Model','Tow_Bar','Price')]
#create dummy variables
#install.packages("fastDummies")
library(fastDummies)
toyoto.df$Fuel_Type=as.factor(toyoto.df$Fuel_Type)
toyoto.df=dummy_cols(toyoto.df,select_columns = c('Fuel_Type','Doors'),remove_first_dummy = TRUE)

#remove fuel type and doors 
toyoto.df=toyoto.df[,-c(3,6)]

#split train and valid
train.index=sample(c(1:dim(toyoto.df)[1]),dim(toyoto.df)[1]*0.6)
toyoto.df.train=toyoto.df[train.index,]
toyoto.df.valid=toyoto.df[-train.index,]
View(toyoto.df.train)

#scale to the range 0-1
norm.values=preProcess(toyoto.df.train,method = 'range')
toyoto.df.train.norm = predict(norm.values, toyoto.df.train)
toyoto.df.valid.norm = predict(norm.values, toyoto.df.valid)

 

View(toyoto.df.train.norm)
set.seed(1)
#neural network
#install.packages('neuralnet')
library(neuralnet)
summary(toyoto.df.train.norm)

#neural network with single layer and 2 nodes 
nn=neuralnet(Price ~ 
               Age_08_04 + KM + HP + Automatic + Quarterly_Tax +
               Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + 
               CD_Player + Powered_Windows + Sport_Model + Tow_Bar +
               Fuel_Type_Petrol + Fuel_Type_Diesel + Doors_3 + Doors_4 + Doors_5,data = toyoto.df.train.norm,hidden = 2)

#upscaling the Price value 
min = min(toyoto.df.train$Price)
max = max(toyoto.df.train$Price)

unscale.price <- function(scaled.price){
  unscaled = scaled.price*(max-min) + min
  return(unscaled)
}
#weights
nn$weights

#prediction train data
train.predict=compute(nn,toyoto.df.train.norm)
unscaled=unscale.price(train.predict$net.result[,1])
plot(nn,rep = 'best')
pred_train=data.frame(Actual=toyoto.df.train$Price,unscaled)
#RMSE
library(Metrics)
paste('RMSE train data:',rmse(pred_train$Actual,pred_train$unscaled))

#prediction validation data
valid.predict=compute(nn,toyoto.df.valid.norm)
unscaled=unscale.price(valid.predict$net.result[,1])
pred_valid=data.frame(Actual=toyoto.df.valid$Price,unscaled)
#RMSE
paste('RMSE valid data:',rmse(pred_valid$Actual,pred_valid$unscaled))
all_metrics=data.frame(Neural_Network='RMSE_1_HIDDEN_2_NODES',RMSE_TRAIN=rmse(pred_train$Actual,pred_train$unscaled),RMSE_VALID=rmse(pred_valid$Actual,pred_valid$unscaled))






#Neural network with single hidden layer and 5 nodes 
nn=neuralnet(Price ~ 
               Age_08_04 + KM + HP + Automatic + Quarterly_Tax +
               Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + 
               CD_Player + Powered_Windows + Sport_Model + Tow_Bar +
               Fuel_Type_Petrol + Fuel_Type_Diesel + Doors_3 + Doors_4 + Doors_5,data = toyoto.df.train.norm,hidden = 5)

#prediction train data
train.predict=compute(nn,toyoto.df.train.norm)
unscaled=unscale.price(train.predict$net.result[,1])
plot(nn,rep = 'best')
pred_train=data.frame(Actual=toyoto.df.train$Price,unscaled)
#RMSE
library(Metrics)
paste('RMSE train data:',rmse(pred_train$Actual,pred_train$unscaled))

#prediction validation data
valid.predict=compute(nn,toyoto.df.valid.norm)
unscaled=unscale.price(valid.predict$net.result[,1])
pred_valid=data.frame(Actual=toyoto.df.valid$Price,unscaled)
#RMSE
paste('RMSE valid data:',rmse(pred_valid$Actual,pred_valid$unscaled))

#all_metrics
all_metrics2=data.frame(Neural_Network='RMSE_1_HIDDEN_5_NODES',RMSE_TRAIN=rmse(pred_train$Actual,pred_train$unscaled),RMSE_VALID=rmse(pred_valid$Actual,pred_valid$unscaled))
all_metrics=rbind(all_metrics,all_metrics2)



#Neural network with 2 hidden layers and 5 nodes 
nn=neuralnet(Price ~ 
               Age_08_04 + KM + HP + Automatic + Quarterly_Tax +
               Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + 
               CD_Player + Powered_Windows + Sport_Model + Tow_Bar +
               Fuel_Type_Petrol + Fuel_Type_Diesel + Doors_3 + Doors_4 + Doors_5,data = toyoto.df.train.norm,hidden = c(5,5))

#prediction train data
train.predict=compute(nn,toyoto.df.train.norm)
unscaled=unscale.price(train.predict$net.result[,1])
plot(nn,rep = 'best')
pred_train=data.frame(Actual=toyoto.df.train$Price,unscaled)
#RMSE
library(Metrics)
paste('RMSE train data:',rmse(pred_train$Actual,pred_train$unscaled))

#prediction validation data
valid.predict=compute(nn,toyoto.df.valid.norm)
unscaled=unscale.price(valid.predict$net.result[,1])
pred_valid=data.frame(Actual=toyoto.df.valid$Price,unscaled)
#RMSE
paste('RMSE valid data:',rmse(pred_valid$Actual,pred_valid$unscaled))
all_metrics3=data.frame(Neural_Network='RMSE_2_HIDDEN_5_NODES',RMSE_TRAIN=rmse(pred_train$Actual,pred_train$unscaled),RMSE_VALID=rmse(pred_valid$Actual,pred_valid$unscaled))
all_metrics=rbind(all_metrics,all_metrics3)
all_metrics

#