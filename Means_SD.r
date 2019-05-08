install.packages(stargazer)
library(stargazer)
library(dplyr)
install.packages("psych")
library(psych)

cols<-c(D.Dos.mean.pre, 
ND.Dos.mean.pre,
D.2nd.sm.mean.pre,
ND.2nd.sm.mean.pre,
D.Tight.mean.pre,
ND.Tight.mean.pre,
D.Mean.Speed.mean.pre,
ND.Mean.Speed.mean.pre,
D.CoV.mean.pre,
ND.CoV.mean.pre)



d.summary <- data_d1 %>%
  select(D.DOS.Mean.pre, 
          ND.DOS.Mean.pre,
          D.2nd.sm.Mean.pre,
          ND.2nd.sm.Mean.pre,
          D.Tight.Mean.pre,
          ND.Tight.Mean.pre,
          D.Mean.Speed.Mean.pre,
          ND.Mean.Speed.Mean.pre,
          D.CoV.Mean.pre,
          ND.CoV.Mean.pre)
test<-describe(d.summary)
write.csv(test, file="diag1.csv")

d.summary <- data_d1 %>%
  select(D.DOS.Mean.post, 
         ND.DOS.Mean.post,
         D.2nd.sm.Mean.post,
         ND.2nd.sm.Mean.post,
         D.Tight.Mean.post,
         ND.Tight.Mean.post,
         D.Mean.Speed.Mean.post,
         ND.Mean.Speed.Mean.post,
         D.CoV.Mean.post,
         ND.CoV.Mean.post)
test<-describe(d.summary)
write.csv(test, file="diag1.csv")



#Diagnosis 2
d.summary <- data_d2 %>%
  select(D.DOS.Mean.pre, 
         ND.DOS.Mean.pre,
         D.2nd.sm.Mean.pre,
         ND.2nd.sm.Mean.pre,
         D.Tight.Mean.pre,
         ND.Tight.Mean.pre,
         D.Mean.Speed.Mean.pre,
         ND.Mean.Speed.Mean.pre,
         D.CoV.Mean.pre,
         ND.CoV.Mean.pre)
test<-describe(d.summary)
write.csv(test, file="diag2.csv")

d.summary <- data_d2 %>%
  select(D.DOS.Mean.post, 
         ND.DOS.Mean.post,
         D.2nd.sm.Mean.post,
         ND.2nd.sm.Mean.post,
         D.Tight.Mean.post,
         ND.Tight.Mean.post,
         D.Mean.Speed.Mean.post,
         ND.Mean.Speed.Mean.post,
         D.CoV.Mean.post,
         ND.CoV.Mean.post)
test<-describe(d.summary)
write.csv(test, file="diag2.csv")

#Diagnosis 3
d.summary <- data_d3 %>%
  select(D.DOS.Mean.pre, 
         ND.DOS.Mean.pre,
         D.2nd.sm.Mean.pre,
         ND.2nd.sm.Mean.pre,
         D.Tight.Mean.pre,
         ND.Tight.Mean.pre,
         D.Mean.Speed.Mean.pre,
         ND.Mean.Speed.Mean.pre,
         D.CoV.Mean.pre,
         ND.CoV.Mean.pre)
test<-describe(d.summary)
write.csv(test, file="diag3.csv")

d.summary <- data_d3 %>%
  select(D.DOS.Mean.post, 
         ND.DOS.Mean.post,
         D.2nd.sm.Mean.post,
         ND.2nd.sm.Mean.post,
         D.Tight.Mean.post,
         ND.Tight.Mean.post,
         D.Mean.Speed.Mean.post,
         ND.Mean.Speed.Mean.post,
         D.CoV.Mean.post,
         ND.CoV.Mean.post)
test<-describe(d.summary)
write.csv(test, file="diag3.csv")
