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

data.for.plot=aggregate()