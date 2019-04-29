library(readxl)
library(tidyverse)
library(rcompanion)
data<-read.csv("Copy_of_SpiralDBS_Analysis2.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
data$difference1<-data$D.DOS.Mean.pre-data$D.DOS.Mean.post



plotNormalHistogram(difference, xlab="Difference (Post - Pre)")
plot(data$D.DOS.Mean.pre, jitter(data$D.DOS.Mean.post), abline(0, 1, col="blue", lwd=2))

one<-t.test(data$D.DOS.Mean.pre, data$D.DOS.Mean.post, paired=TRUE)
two<-t.test(data$ND.DOS.Mean.pre, data$ND.DOS.Mean.post, paired=TRUE)
three<-t.test(data$D.2nd.sm.Mean.pre, data$D.2nd.sm.Mean.post, paired=TRUE)
four<-t.test(data$ND.2nd.sm.Mean.pre, data$ND.2nd.sm.Mean.post, paired=TRUE)
five<-t.test(data$D.Tight.Mean.pre, data$D.Tight.Mean.post, paired=TRUE)
six<-t.test(data$ND.Tight.Mean.pre, data$ND.Tight.Mean.post, paired=TRUE)
seven<-t.test(data$D.Mean.Speed.Mean.pre, data$D.Mean.Speed.Mean.post, paired=TRUE)
eight<-t.test(data$ND.Mean.Speed.Mean.pre, data$ND.Mean.Speed.Mean.post, paired=TRUE)
nine<-t.test(data$D.CoV.Mean.pre, data$D.CoV.Mean.post, paired=TRUE)
ten<-t.test(data$ND.CoV.Mean.pre, data$ND.CoV.Mean.post, paired=TRUE)

combined<-list(c(one),c(two),c(three),c(four),c(five),c(six),c(seven),c(eight),c(nine),c(ten))


##Stratified by diagnosis code,

f <- function(x,y){
  test <- t.test(x,y, paired=TRUE)
  out <- data.frame(stat = test$statistic,
                    df   = test$parameter,
                    pval = test$p.value,
                    conl = test$conf.int[1],
                    conh = test$conf.int[2]
  )
  return(out)
}


####Trying apply functions
list1<-c('Patient.ID', 'D.DOS.Mean.pre', 'ND.DOS.Mean.pre', 'D.2nd.sm.Mean.pre',
         'ND.2nd.sm.Mean.pre', 'D.Tight.Mean.pre', 'ND.Tight.Mean.pre',
         'D.Mean.Speed.Mean.pre', 'ND.Mean.Speed.Mean.pre',
         'D.CoV.Mean.pre', 'ND.CoV.Mean.pre', 'Diagnosis')
list2<-c('Patient.ID', 'D.DOS.Mean.post', 'ND.DOS.Mean.post', 'D.2nd.sm.Mean.post',
         'ND.2nd.sm.Mean.post', 'D.Tight.Mean.post', 'ND.Tight.Mean.post',
         'D.Mean.Speed.Mean.post', 'ND.Mean.Speed.Mean.post',
         'D.CoV.Mean.post', 'ND.CoV.Mean.post', 'Diagnosis')

datadf<-data.frame(data)
datadf2<-datadf[,c(list1, list2)]
datadf2<-datadf2[,-13]
datadf2<-datadf2[,-23]

##########333
#https://stackoverflow.com/questions/49174804/paired-t-test-on-several-columns-with-multiple-groups-in-one-column
prepost<-rbind(list1,list2)
prepost<-prepost[,-12]
prepost<-prepost[,-1]

yuck<-lapply(split(datadf2, datadf2$Diagnosis), function(x)
  apply(prepost, 2, function(y) t.test(x[,y[1]], x[,y[2]], paired=T)))
##########
#
str(yuck, max.level = 1)
sapply(yuck, function(x) sapply(x, function(y) y$p.value))
apply(prepost, 2, paste, collapse = "_")
#
#########################################################
#########################################################




#Regression
data$difference1<-data$D.DOS.Mean.post-data$D.DOS.Mean.pre
D.Dos.Reg <- lm(difference1~UPDRS.pre+UPDRS.post+Dz.Duration..yrs.+Age+Sex+Hoehn...Yahr.pre+Hoehn...Yahr.post,data=data)
summary(D.Dos.Reg)               

data$difference2<-data$ND.DOS.Mean.post-data$ND.DOS.Mean.pre
ND.Dos.Reg <- lm(difference2~UPDRS.pre+UPDRS.post+Dz.Duration..yrs.+Age+Sex+Hoehn...Yahr.pre+Hoehn...Yahr.post,data=data)
summary(ND.Dos.Reg)










#######################

Things that didnt work

#######################

#stratfiy by diagnosis
df1<-data.frame(data$Patient.ID, data$D.DOS.Mean.pre, data$ND.DOS.Mean.pre, data$D.2nd.sm.Mean.pre,
           data$ND.2nd.sm.Mean.pre, data$D.Tight.Mean.pre, data$ND.Tight.Mean.pre,
           data$D.Mean.Speed.Mean.pre, data$ND.Mean.Speed.Mean.pre,
           data$D.CoV.Mean.pre, data$ND.CoV.Mean.pre, data$Diagnosis)
df2<-data.frame(data$Patient.ID, data$D.DOS.Mean.post, data$ND.DOS.Mean.post, data$D.2nd.sm.Mean.post,
              data$ND.2nd.sm.Mean.post, data$D.Tight.Mean.post, data$ND.Tight.Mean.post,
              data$D.Mean.Speed.Mean.post, data$ND.Mean.Speed.Mean.post,
              data$D.CoV.Mean.post, data$ND.CoV.Mean.post, data$Diagnosis)
dfmerged<-cbind(df1, df2)

#ffffff
f <- function(x,y){
  test <- t.test(x,y, paired=TRUE)
  out <- data.frame(MeanDiff = unname(test$estimate),
                    stat = test$statistic,
                    df   = test$parameter,
                    pval = test$p.value,
                    conl = test$conf.int[1],
                    conh = test$conf.int[2]
  )
  return(out)
}

f(dfmerged$data.D.DOS.Mean.pre, dfmerged$data.D.DOS.Mean.post)
f(dfmerged[,2], dfmerged[,14])


for(i in 2:11)
{
  for(j in 14:23)
  {
    f(dfmerged[,i], dfmerged[,j])
  }
}




dfmerged$pattern <- apply(data[, 3:12], 1, paste, collapse = "")
sapply(data$pattern, f)






#single variable p values
p.values<-by(dfmerged,
             dfmerged$Diagnosis,
             FUN=function(x) t.test(dfmerged[,2], dfmerged[,14], paired=T)$p.value)
p.D.DOS <- data.frame(p.values)
p.D.DOS

p.values<-by(dfmerged,
     dfmerged$Diagnosis,
     FUN=function(x) t.test(dfmerged[,2:11], dfmerged[,14:23], paired=T)$p.value)
p.D.DOS <- data.frame(p.values)
p.D.DOS







p.values=c(
  by(data,
     data$Diagnosis,
     FUN=function(x) t.test(x$D.DOS.Mean.pre, x$D.DOS.Mean.post, paired=T)$p.value))
p.D.DOS <- data.frame(p.values)













mapply(function(x) f(x[,2:11], x[,14:23])), 
datadf2[,2:23], 


mapply(function(i, j) 
  t.test(df1[,i], df2[,j], paired = T)$p.value, 
  2:ncol(df1), 2:ncol(df2))



counts$p_value <- unlist(lapply(t.result, function(x) x$p.value))
#You will also need some sort of multiple testing correction.

counts$fdr <- p.adjust(counts$p_value, method = "fdr")






lapply(datadf[list1], mean, na.rm=TRUE)
lapply(datadf2[,2:7], function(i) f(datadf2[,2], datadf2[,14]))
lapply(datadf2[,2], function(i) f(datadf2[,2:11], datadf2[,14:23]))

#Good model#########################################33#
lapply(datadf2[2:6,2:3], function(x) f(datadf2[,2], datadf2[,14]))


