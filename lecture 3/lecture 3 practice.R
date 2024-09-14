install.packages('GGally','ggmap','mosaic','treemap')

amtrak.df=read.csv('Amtrak data.csv')

head(amtrak.df)

#use time series analysis
library(forecast)

ridership.ts=ts(amtrak.df$Ridership, start=c(1991,1),end = c(2003,3), freq=12)
plot(ridership.ts,xlab='year',ylab='Ridership',ylim=c(1300,2300))


#Boston House data
housing.df=read.csv('BostonHousing.csv')
head(housing.df)

#bar plot 
data.for.plot=aggregate(housing.df$MEDV, by=list(housing.df$CHAS),FUN=mean)
data.for.plot
names(data.for.plot) =c('CHAS','MEANMEDV')
data.for.plot
barplot(data.for.plot$MEANMEDV, names.arg=data.for.plot$CHAS,xlab='CHAS',ylab='Avg. MEDV')

#ggplot
library(ggplot2)
ggplot(data.for.plot) + geom_bar(aes(x=CHAS,y=MEANMEDV), stat='identity')

#scatter plot with axes names 
plot(housing.df$MEDV~ housing.df$LSTAT, xlab='MEDV',ylab='LSTAT')
#ggplot
ggplot(housing.df) +geom_point(aes(x=LSTAT,y=MEDV),color='red',alpha=0.5)


#Barchart of CHAS vs. % CAT.MEDV
data.for.plot = aggregate(housing.df$CAT..MEDV, by=list(housing.df$CHAS),FUN=mean)
names(data.for.plot) = c('CHAS','MeanCATMEDV')
barplot(data.for.plot$MeanCATMEDV*100, names.arg=data.for.plot$CHAS,xlab = 'CHAS',ylab='% of CATMEDV')







#### Figure 3.2
# histogram fo MEDV
hist(housing.df$MEDV,xlab = 'MEDV')
ggplot(housing.df) + geom_histogram(aes(x=MEDV),binwidth = 5)

## boxplot of MEDV for different values of CHAS
boxplot(housing.df$MEDV~housing.df$CHAS,xlabs='CHAS', ylab = 'MEDV')
##using ggplot
ggplot(housing.df) + geom_boxplot(aes(x=as.factor(CHAS), y=MEDV)) + xlab('CHAS')



#### Figure 3.3

## side-by-side boxplots
# use par() to splot the plots into panels
par(mfcol=c(1,4))
boxplot(housing.df$NOX ~ housing.df$CAT..MEDV, xlab='CAT.MEDV',ylab='NOX')
boxplot(housing.df$LSTAT ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "LSTAT")
boxplot(housing.df$PTRATIO ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "PTRATIO")
boxplot(housing.df$INDUS ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "INDUS")





## Figure 3.4
# heatmaps 
heatmap(cor(housing.df),Rowv = NA, Colv = NA )
#heatmap with values 
library(gplots)
heatmap.2(cor(housing.df),Rowv=FALSE, Colv=FALSE, dendrogram='none', cellnote=round(cor(housing.df),2),
          notecol='black', key=FALSE, trace='none', margins=c(10,10)
          )
cor(housing.df)

library(ggplot2)
library(reshape)
cor.mat=round(cor(housing.df),2)
melted.cor.mat=melt(cor.mat)
melted.cor.mat

ggplot(melted.cor.mat,aes(x=X1,y=X2,fill = value)) +geom_tile() +geom_text(aes(x=X1,y=X2,label=value))
