install.packages('GGally','ggmap','mosaic','treemap')

amtrak.df=read.csv('Amtrak data.csv')

head(amtrak.df)
amtrak.df
#use time series analysis
library(forecast)
class(amtrak.df$Month)
ridership.ts=ts(amtrak.df$Ridership, start=c(1991,1),end = c(2003,3), freq=12)
plot(ridership.ts,xlab='year',ylab='Ridership',ylim=c(1300,2300))


#Boston House data
housing.df=read.csv('datasets/BostonHousing.csv')
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

heatmap(1*is.na(housing.df),Rowv = NA, Colv = NA)



## figure 3.6
## color plot
par(mfcol=c(1,1))
plot(housing.df$NOX, housing.df$LSTAT, xlab='NOX',ylab='LSTAT',
     col=ifelse(housing.df$CAT..MEDV==1,'black','red')
   )
##adding legend
legend('topleft', legend=c('CAT.MEDV=1','CAT>MEDV=0'),col=c('black','red')
       ,pch = 1,cex=0.5
       )

##ggplot
library(ggplot2)
ggplot(housing.df,aes(y=NOX,x=LSTAT,colour = CAT..MEDV)) + geom_point(alpha=0.6)



##panel plots
data.for.plot=aggregate(housing.df$MEDV, by=list(housing.df$RAD,housing.df$CHAS),FUN=mean,drop=FALSE)
data.for.plot
names(data.for.plot)=c('RAD','CHAS','meanMEDV')
data.for.plot

par(mfcol=c(2,1))
barplot(height = data.for.plot$meanMEDV[data.for.plot$CHAS==0],
        names.arg = data.for.plot$RAD[data.for.plot$CHAS==0],
        xlab = 'RAD', ylab='Avg. MEDV', main='CHAS =0'
        )
barplot(height = data.for.plot$meanMEDV[data.for.plot$CHAS==1],
        names.arg = data.for.plot$RAD[data.for.plot$CHAS==1],
        xlab='RAD',ylab = 'Avg. MEDV', main='CHAS=1'
        )
#ggplot
ggplot(data.for.plot) +geom_bar(aes(x=as.factor(RAD),y=meanMEDV),stat = 'identity') +
   xlab('RAD')  + facet_grid(CHAS ~ .)






## figure 3.7
## simple plot
plot(housing.df[,c(1,3,12,13)])

library(GGally)
ggpairs(housing.df[,c(1,3,12,13)])




#figure 3.8
options(scipen = 999) # avoid scientific notation 
plot(housing.df$MEDV~housing.df$CRIM,xlab='CRIM',ylab='MEDV')
plot(housing.df$MEDV ~ housing.df$CRIM, 
     xlab = "CRIM", ylab = "MEDV", log = 'xy')


library(ggplot2)
ggplot(housing.df) + geom_point(aes(x=CRIM,y=MEDV))+
scale_x_log10(breaks = 10^(-2:2),
              labels = format(10^(-2:2), scientific = FALSE, drop0trailing = TRUE)) +
  scale_y_log10(breaks = c(5, 10, 20, 40))


#boxplot in log scale
boxplot(housing.df$CRIM~housing.df$CAT..MEDV)
boxplot(housing.df$CRIM~housing.df$CAT..MEDV,log='y')



#figure 3.9
#timeseries 
library(forecast)
Amtrak.df <- read.csv("datasets/Amtrak data.csv")
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

ridership.ts
library(ggplot2)
## fit cggplot2## fit curve 
ridership.ts.lm=tslm(ridership.ts ~trend + I(trend^2))
plot(ridership.ts, xlab = "", ylab = "", ylim = c(1300, 2300))
lines(ridership.ts.lm$fitted, lwd = 2)

library(ggplot2)
ggplot(Amtrak.df, aes(y = Ridership, x = Month, group = 12)) +
  geom_line() + geom_smooth(formula = y ~ poly(x, 2), method= "lm",
                            colour = "navy", se = FALSE, na.rm = TRUE)

#modifying x to handle appropriately
ggplot(Amtrak.df, aes(y=Ridership,x=as.numeric(time(ridership.ts)))) +geom_line() + 
  geom_smooth(formula=y~poly(x,2),method = 'lm' ,colour = "navy", se = FALSE, na.rm = TRUE) +
  xlab("Year") + ylab("Ridership (in 000s)")








##zoom in, monthly and annual plots 
ridership.2yrs=window(ridership.ts,start=c(1991,1),end=c(1992,12))
plot(ridership.2yrs)
monthly.ridership.ts=tapply(ridership.ts,cycle(ridership.ts),mean)
plot(monthly.ridership.ts,ylim=c(1300,2300),type = 'o')





ridership.2yrs <- window(ridership.ts, start = c(1991,1), end = c(1992,12))
plot(ridership.2yrs, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
monthly.ridership.ts <- tapply(ridership.ts, cycle(ridership.ts), mean)
plot(monthly.ridership.ts, xlab = "Month", ylab = "Average Ridership",
     ylim = c(1300, 2300), type = "l", xaxt = 'n')

axis(1,at=c(1:12),labels = c("Jan","Feb","Mar", "Apr","May","Jun",
                             "Jul","Aug","Sep",  "Oct","Nov","Dec"))
annual.ridership.ts=aggregate(ridership.ts,FUN=mean)
plot(annual.ridership.ts, ylim = c(1300, 2300))



# Figure 3.10
utilities.df=read.csv('datasets/Utilities.csv')
head(utilities.df)

plot(utilities.df$Fuel_Cost~utilities.df$Sales)
text(x=utilities.df$Sales,y=utilities.df$Fuel_Cost,labels = utilities.df$Company,pos = 4, cex = 0.8, srt = 20, offset = 0.2)
library(ggplot2)
ggplot(utilities.df, aes(y = Fuel_Cost, x = Sales)) + geom_point() +
  geom_text(aes(label = paste(" ", Company)), size = 4, hjust = 0.0, angle = 15) +
  ylim(0.25, 2.25) + xlim(3000, 18000)


#Figure 3.11
universal.df=read.csv('datasets/UniversalBank.csv')
head(universal.df)
library(scales)
plot(jitter(universal.df$CCAvg, 1) ~ jitter(universal.df$Income, 1),
     col = alpha(ifelse(universal.df$Securities.Account == 0, "gray", "red"), 0.4),
     pch = 20, log = 'xy', ylim = c(0.1, 10),
     xlab = "Income", ylab = "CCAvg")
# alternative with ggplot
library(ggplot2)
ggplot(universal.df) +
  geom_jitter(aes(x = Income, y = CCAvg, colour = Securities.Account)) + 
  scale_x_log10(breaks = c(10, 20, 40, 60, 100, 200)) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.4, 0.6, 1.0, 2.0, 4.0, 6.0))
#replace geom_jitter with geom_point to create without jitter to see the difference. 




#figure 3.12
housing.df=read.csv('datasets/BostonHousing.csv')
library(MASS)
par(mfcol = c(2,1)) # set graphical parameters
parcoord(housing.df[housing.df$CAT..MEDV == 0, -14], main = "CAT.MEDV = 0")
parcoord(housing.df[housing.df$CAT..MEDV == 1, -14], main = "CAT.MEDV = 1")


#### Figure 3.14

library(igraph)
ebay.df <- read.csv("datasets/eBayNetwork.csv")

# transform node ids to factors
ebay.df[,1] <- as.factor(ebay.df[,1])
ebay.df[,2] <- as.factor(ebay.df[,2])

graph.edges <- as.matrix(ebay.df[,1:2])
g <- graph.edgelist(graph.edges, directed = FALSE)
isBuyer <- V(g)$name %in% graph.edges[,2]

plot(g, vertex.label = NA, vertex.color = ifelse(isBuyer, "gray", "black"), 
     vertex.size = ifelse(isBuyer, 7, 10))



#### Figure 3.15

library(treemap)
tree.df <- read.csv("datasets/EbayTreemap.csv")

# add column for negative feedback
tree.df$negative.feedback <- 1* (tree.df$Seller.Feedback < 0)

# draw treemap
treemap(tree.df, index = c("Category","Sub.Category", "Brand"), 
        vSize = "High.Bid", vColor = "negative.feedback", fun.aggregate = "mean",
        align.labels = list(c("left", "top"), c("right", "bottom"), c("center", "center")),
        palette = rev(gray.colors(3)), type = "manual", title = "")




#### Figure 3.16

# need google api key
library(ggmap)
SCstudents <- read.csv("datasets/SC-US-students-GPS-data-2016.csv")
Map <- get_map("Denver, CO", zoom = 3)
ggmap(Map) + geom_point(aes(x = longitude, y = latitude), data = SCstudents,
                        alpha = 0.4, colour = "red", size = 0.5)



#### Figure 3.17

library(mosaic)

gdp.df <- read.csv("datasets/gdp.csv", skip = 4, stringsAsFactors = FALSE) 
names(gdp.df)[5] <- "GDP2015"
happiness.df <- read.csv("datasets/Veerhoven.csv")

# Convert character column to UTF-8 encoding
gdp.df$Coun
try.Name <- iconv(gdp.df$Country.Name, to = "UTF-8")

# gdp map
mWorldMap(gdp.df, key = "Country.Name", fill = "GDP2015") + coord_map()

# Convert character column to UTF-8 encoding
happiness.df$Nation <- iconv(happiness.df$Nation, to = "UTF-8")

# well-being map
mWorldMap(happiness.df, key = "Nation", fill = "Score") + coord_map() + 
  scale_fill_continuous(name = "Happiness")

