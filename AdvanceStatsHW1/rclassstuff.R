# advance stats R programming

# clear the environment
rm(list=ls())

#creating vectors
a1=c(23,45,46,2,342,34,57,54,4,23534,2)
a2=c(1234,235,345,2,412,31,2432,3532,52)
a3=c(45.3,23,2345,45,23,42,34,235,6,4,5634)

v1=c(a1,a2,a3)
v1

# rounding the values 
v1=round(v1,0)
v1

length(v1)

v1[1:10]

#get last 5 numbers 
v1[27:5]

v1[c(v1>200 & v1<500)]
