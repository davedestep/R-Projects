install.packages("readxl")
library("readxl")
my_data <- read_excel("\\Ages_n11426.xlsx")
x <- as.data.frame(my_data)

install.packages("gamlss")
library("gamlss")

plot(avg_sys~age_days, data=my_data, col="blue", xlab="age", ylab="Systolic BP")
install.packages("dplyr")
library(dplyr)
data2<-filter(my_data, age_days<6570)
plot(avg_sys~age_days, data=data2, col="blue", xlab="age", ylab="Systolic BP")

#GAMLSS
abd0 <- gamlss(avg_sys~poly(age_days,3), data=data2, family=NO)
abd0 <- gamlss(avg_sys~poly(age_days,3), data=data2)
summary(abd0)


abd1<-gamlss(avg_sys~cs(age_days,df=3), data=data2, family=NO)

#Residual Plot
plot(resid(abd0))
plot(abd0)

wp(abd0)


#Loess
abd3 <- gamlss(avg_sys~lo(age_days,span=.4),sigma.formula=~lo(age_days,span=.4),data=data2, family=NO)
abd3 <- gamlss(avg_sys~cs(age_days,3),sigma.formula=~cs(age_days,3), data=data2, family=TF)
plot(abd3)


SYS <- gamlss(avg_sys~cs(age_days,3),sigma.formula=~cs(age_days,3), data=data2, family=BCT)
plot(sys)
wp(sys)

#Centile Plots
centiles(sys,data2$age_days,cent=c(10, 25,50,75, 90), ylab="Systolic Blood Pressure, Box Cox T Distribution", xlab="age")
#Split the centile plot
centiles.split(SYS,data2$age_days,cent=c(10, 25,50,75, 90), ylab="Systolic Blood Pressure, Box Cox T Distribution", xlab="age")



#Average MAP
MAP <- gamlss(avg_MAP~cs(age_days,3),sigma.formula=~cs(age_days,3), data=data2, family=BCT)
centiles(MAP,data2$age_days,cent=c(10, 25,50,75, 90), ylab="MAP, Box Cox T Distribution", xlab="age")
#Split the centile plot
centiles.split(abd3,data2$age_days,cent=c(5, 10, 25,50,75, 90, 95), ylab="MAP, Box Cox T Distribution", xlab="age")

