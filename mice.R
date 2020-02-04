#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## OPEN 
dat<-read.csv("C:/Users/dd/Desktop/Depression and GA/01 ANNUAL FILES/D11_STUDY.csv",sep=",", dec=".", header = T,stringsAsFactors =F)
dim(dat)
head(dat)
summary(dat)
summary(is.na(dat$DSHOSPID))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## definition of categorical OR numeric variables   with missing values

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## definition of predictors
NOTexclude<-c("ANXIETY_OUT",
"DEPRESSION_OUT",
"PTSD_OUT",
"SUIC_OUT",
"ANXIETY_HIST",
"DEPRESSION_HIST",
"PTSD_HIST",
"SUIC_HIST",
"ANESTH2",
"YEAR",
"DSHOSPID",
"AGE",
"MEDICAID_CARE",
"BATEMAN2",
"CHARLSON2",
"AWEEKEND",
"FHR_ABNO",
"FETAL_DISTR",
"ABRUPTION",
"UT_RUPT",
"CORD_PROLAP",
"POSTP_HEMO",
"BLOODICD9",
"PRAEV",
"ACCRET_NO_HEMO",
"COAG_DEF",
"THROMBOPENIA",
"SEPSIS",
"CHORIO_AMNIO",
"H_DEL",
"H_CDRATE",
"H_WEEKEND",
"H_MEDICAID_CARE",
"H_CHARL",
"H_BATE",
"H_CODING")


NOTexclude
length(NOTexclude)
NUM<-which(is.element(colnames(dat),NOTexclude)) # columns number of predictors 
NUM
length(NUM)
dat2<-dat[,-NUM] # dataframe excluding predictors 
dim(dat2)
head(dat2)
exclude<-names(dat2) # names of variables excluded from predictors 
exclude

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# excluded from imputation

excluimpu<-c("KEY")
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# MULTIPLE IMPUTATION
library(mice)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# creation of the MICE OBJECT



ini<-mice(dat,maxit=0)
#ini


pred<-ini$predictorMatrix  # predictor matrix used for imputations

exclumatrix<-exclude ## columns to be excluded from the predictor matrix       

pred[,exclumatrix]<-0 #limitation of the predictor matrix    with exclusions of the columns defined in vector excluded


ini<-mice(dat,maxit=0,pred=pred, pri=F)     # redefiniton of the predictor matrix

#ini

excluimpu<-c("KEY")# definition of columns not to be imputed 

meth<-ini$meth

meth[excluimpu]<-""

ini<-mice(dat,maxit=0,m=5,meth=meth,pred=pred, pri=F)

ini




imp<-mice(dat,maxit=5,m=5,meth=meth,pred=pred, pri=F, seed=1)

plot(imp,graphics.record=F)
plot(imp,print=F)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# to save the file
write.csv(complete(imp,1), file = "C:/Users/dd/Desktop/Depression and GA/01 ANNUAL FILES/D12_STUDY_SAMPLE_CD_HOSP_LIMITED_2005_2013_run1.csv", 
row.names=FALSE) # to save the file
write.csv(complete(imp,2), file = "C:/Users/dd/Desktop/Depression and GA/01 ANNUAL FILES/D12_STUDY_SAMPLE_CD_HOSP_LIMITED_2005_2013_run2.csv", 
row.names=FALSE) # to save the file
write.csv(complete(imp,3), file = "C:/Users/dd/Desktop/Depression and GA/01 ANNUAL FILES/D12_STUDY_SAMPLE_CD_HOSP_LIMITED_2005_2013_run3.csv", 
row.names=FALSE) # to save the file
write.csv(complete(imp,4), file = "C:/Users/dd/Depression and GA/01 ANNUAL FILES/D12_STUDY_SAMPLE_CD_HOSP_LIMITED_2005_2013_run4.csv", 
row.names=FALSE) # to save the file
write.csv(complete(imp,5), file = "C:/Users/dd/Desktop/Depression and GA/01 ANNUAL FILES/D12_STUDY_SAMPLE_CD_HOSP_LIMITED_2005_2013_run5.csv", 
row.names=FALSE) # to save the file



