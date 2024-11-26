rm(list=ls())

library(readxl)
data.df=read_excel('Chi-Sq-S.xlsx',sheet = 'MBA-R')
head(data.df)
data.df=as.data.frame(data.df)

rownames(data.df)=(data.df[1:nrow(data.df), 1])
data.df=data.df[,-c(1)]
chisq.test(as.matrix(data.df))
