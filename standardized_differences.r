#Use this macros is SAS to get Standardized differences

#Plot:

library(tidyverse)

setwd("C:\\Users\\Dave\\Desktop\\ALL\\adhd stddif")
stddif<-haven::read_sas("zzz_bigdataset.sas7bdat")

##########
#Female NY
##########
a<-stddif %>%
  filter(#surg=="pylo",
    state=="NY",
    sex=="female") %>% 
  mutate(Stddiff=abs(as.numeric(Stddiff))) %>% 
  ggplot( 
    mapping = aes(x = VarName, y = Stddiff,group = period, color = period)) +
  geom_point() + geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() + theme_bw() + theme(legend.key = element_blank(),
                                    axis.text.y = element_text(color = "grey20", size = 5))

a + facet_grid(rows = vars(surg))


#############################################################################
##########
#Female TX
##########
b<-stddif %>%
  filter(#surg=="pylo",
    state=="TX",
    sex=="female") %>% 
  mutate(Stddiff=abs(as.numeric(Stddiff))) %>% 
  ggplot( 
    mapping = aes(x = VarName, y = Stddiff,group = period, color = period)) +
  geom_point() + geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() + theme_bw() + theme(legend.key = element_blank(),
                                    axis.text.y = element_text(color = "grey20", size = 5))

b + facet_grid(rows = vars(surg))


#############################################################################
##########
#Male NY
##########
c<-stddif %>%
  filter(#surg=="pylo",
    state=="NY",
    sex=="male") %>% 
  mutate(Stddiff=abs(as.numeric(Stddiff))) %>% 
  ggplot( 
    mapping = aes(x = VarName, y = Stddiff,group = period, color = period)) +
  geom_point() + geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() + theme_bw() + theme(legend.key = element_blank(),
                                    axis.text.y = element_text(color = "grey20", size = 5))+
  title("Male NY")

c + facet_grid(rows = vars(surg))




#############################################################################
##########
#Male TX
##########
d<-stddif %>%
  filter(#surg=="pylo",
         state=="TX",
         sex=="male") %>% 
  mutate(Stddiff=abs(as.numeric(Stddiff))) %>% 
  ggplot( 
    mapping = aes(x = VarName, y = Stddiff,group = period, color = period)) +
  geom_point() + geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() + theme_bw() + theme(legend.key = element_blank(),
                                    axis.text.y = element_text(color = "grey20", size = 5))

d + facet_grid(rows = vars(surg))






####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

#Only diffs for male and female, not stratified by surgery, state, age

library(tidyverse)
setwd("C:\\Users\\Dave\\Desktop\\ALL\\adhd stddif")
stddif<-haven::read_sas("Sex_cdiff2.sas7bdat")



stddif$Variables <- factor(stddif$varname2, levels = rev(unique(stddif$varname2)), ordered=TRUE)
stddif <- stddif %>% rename(Standardized_Differences = Stddiff) 




stddif %>%
  filter(sex=='male') %>% 
  mutate(Standardized_Differences=abs(as.numeric(Standardized_Differences))) %>% 
  ggplot( 
    mapping = aes(x = Variables, y = Standardized_Differences)) +
  geom_point() + geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() + theme_bw() + theme(legend.key = element_blank(),
                                    axis.text.y = element_text(color = "grey20", size = 5))



stddif %>%
  filter(sex=='fema') %>% 
  mutate(Standardized_Differences=abs(as.numeric(Standardized_Differences))) %>% 
  ggplot( 
    mapping = aes(x = Variables, y = Standardized_Differences)) +
  geom_point() + geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() + theme_bw() + theme(legend.key = element_blank(),
                                    axis.text.y = element_text(color = "grey20", size = 5))

