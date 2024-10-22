library(ggplot2)


#3.a
shipments.df<-read.csv('ApplianceShipments.csv')
head(shipments.df)
shipments.df
summary(shipments.df)
shipments.df.ts<- ts(shipments.df$Shipments,start=c(1985,1),end=c(1989,4),frequency = 4)
## axis(1,c(1:4),labels = c('Q1','Q2','Q3','Q4'))
plot(shipments.df.ts,ylab='shipments',xlab='years',type='o')



# 3.b
plot(shipments.df.ts,ylab='shipments',xlab='years',ylim=c(3500,5000), type='o')

#3.c
shipments.df<-read.csv('ApplianceShipments.csv')
head(shipments.df)
shipments.df$Quarter_Label=sub('^(Q\\d).*-\\d{4}$','\\1',shipments.df$Quarter)
shipments.df$Quarter_Label=factor(shipments.df$Quarter_Label,levels = c('Q1','Q2','Q3','Q4'))

shipments.df$Year=sub('.*-(\\d{4})','\\1',shipments.df$Quarter)
shipments.df
ggplot(shipments.df,aes(x=Year,y=Shipments,colour =Quarter_Label, group = Quarter_Label))+
  geom_line(size=1) +
  labs(title = 'Quarterly Shipments of Household Appliances',
       x='Year',
       y='Shipment',
       )+
  ylim(3500,5000)
  
# method 2 with multiple dataframes
shipments.df.q1=shipments.df[shipments.df$Quarter_Label=='Q1',]
shipments.df.q2=shipments.df[shipments.df$Quarter_Label=='Q2',]
shipments.df.q3=shipments.df[shipments.df$Quarter_Label=='Q3',]
shipments.df.q4=shipments.df[shipments.df$Quarter_Label=='Q4',]

plot(shipments.df.q1$Year,shipments.df.q1$Shipments,col='red',type='o',ylim = c(3500,5000),xlab='shipments')
lines(shipments.df.q2$Year,shipments.df.q2$Shipments,col='blue',type='o',ylim = c(3500,5000),xlab='shipments')
lines(shipments.df.q3$Year,shipments.df.q3$Shipments,col='green',type='o',ylim = c(3500,5000),xlab='shipments')
lines(shipments.df.q4$Year,shipments.df.q4$Shipments,col='black',type='o',ylim = c(3500,5000),xlab='shipments')
legend("topright", legend = c("Q1", "Q2", "Q3", "Q4"), col = c("red", "blue", "green", "purple"), lty = 1)




#3.d
shipments.df.yearly=aggregate(Shipments~Year,data=shipments.df,FUN = sum)
shipments.df.yearly
ggplot(shipments.df.yearly)+
  geom_line(aes(x=Year,y=Shipments,group = 1))+
  labs(title='Total Yearly Shipments of Household Appliances',
       x='Year',
       y='Shipment',)



#4.a
ridingMovers.df<-read.csv('RidingMowers.csv')
head(ridingMovers.df)

ggplot(ridingMovers.df)+
  geom_point(aes(x=Lot_Size,y=Income,colour = Ownership),size=5,alpha=10)



#5.a
laptop.df<-read.csv('LaptopSalesJanuary2008.csv')
head(laptop.df)
laptop.df$Store.Postcode=factor(laptop.df$Store.Postcode, levels=unique(laptop.df$Store.Postcode))
laptop.df$OS.X.Store =factor(laptop.df$OS.X.Store , levels=unique(laptop.df$OS.X.Store))
laptop.df.avg=aggregate(laptop.df$Retail.Price,FUN=mean,by=list(laptop.df$OS.X.Store))
names(laptop.df.avg)=c('Store','Avg_Retail_Price')
laptop.df.avg
ggplot(laptop.df.avg,aes(x=Store,y=Avg_Retail_Price))+
         geom_bar(stat = "identity", fill = "skyblue")+
  labs(title = 'Average Price of east Store',
       x='Store Post code',
       y='Avg. Retail Price')
laptop.df.avg[laptop.df.avg$Avg_Retail_Price==min(laptop.df.avg$Avg_Retail_Price),]
laptop.df.avg[laptop.df.avg$Avg_Retail_Price==max(laptop.df.avg$Avg_Retail_Price),]


#5.b
ggplot(laptop.df)+
  geom_boxplot(aes(x=as.factor(Store.Postcode),Retail.Price))+
  labs(
    title='Box plot of Retail Prices on Store',
    x='Store Post Code',
    y='Retail Price'
  )
laptop.df
