library(readxl)

data.df=read_excel('case2001.xlsx',sheet = 'Donner')
summary(data.df)
attach(data.df)
donner=glm(Status ~ Age+Sex,family = 'binomial')
coef(donner)
regout=c(coef(donner))
regout

#survival odds for women 50 years old
odd50=exp(regout[1]+regout[2]*50+regout[3])
odd50
