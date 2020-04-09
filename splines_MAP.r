library(tidyverse)
library(icd)
library(naniar)
library(pccc)
library(sqldf)
library(summarytools)
library(knitr)
library(kableExtra)
library(rms)
library(Hmisc)
library(survival)

#. If the relation of a predictor to outcome is linear, 
# then the higher-order spline terms will have non-significant coefficients.
# If the relation is non-linear, you can adjust for any reasonable degree of nonlinearity 
# by adding more knots. 

setwd("C:\\Users\\Dave\\Desktop\\TRAC\\Projects\\MRN Patient List")

results<-haven::read_sas("_20200204_results.sas7bdat")


cols<-c("race2",
        "ethnicity2",
        "gender",
        "multilevel2_label",
        "technique_final",
        "bin_sevo",
        "bin_iso",
        "bin_des",
        "bin_nitrous",
        "bin_prop",
        "neuromusc",
        "cvd",
        "respiratory",
        "renal",
        "gi",
        "hemato_immu",
        "metabolic",
        "scol",
        "neonatal",
        "agecat")

results[cols] <- lapply(results[cols], factor)
results$multilevel2_label = relevel(results$multilevel2_label, ref=5)


results$surv<-Surv(results$censor, results$outcome)

############################################################
#Splines using rms and hmisc packages
############################################################

d <- datadist(results)
d$limits$ranky[2] <- 50
options(datadist='d')

f <- cph(surv ~ rcs(ranky, 3), data=results)
f <- cph(surv ~ rcs(ranky, 4), data=results)
f <- cph(surv ~ rcs(ranky, 5), data=results)




#other standard found in RCS paper
f <- cph(surv ~ rcs(ranky, c(10,25, 50, 75,90)), data=results)


#Same as SAS heuristic
f <- cph(surv ~ rcs(ranky, c(5, 27.5, 50, 72.5, 90)), data=results)

cox.zph(f, "rank") # tests of PH
anova(f)
ggplot(Predict(f, ranky, fun=exp)) 
#ggplot(Predict(f, ranky, gender)) 
#non linear test is not rejected, so statistically we cannot conclude that this is a nonlinear relationship..

#To get the "waist"/reference point set to the 50th percentile
pdata <- Predict(f, ranky, ref.zero = TRUE, fun = exp)
ggplot(data = pdata) +
  geom_hline(aes(yintercept = 1), linetype = 3) +
  labs(x = "Average MAP percentile",
       y = "Hazard Ratio (95% CI) vs. 50th Percentile",
       title = "Diagnosis hazard ratio as a function of MAP percentile",
       subtitle = "Natural spline: knots at 10, 25, 50, 75, and 90 percentile")


############################################################
#p <- cph(surv ~ rcs(ranky, 4) +  duration_co2 +multilevel2_label , data=results)
#p <- cph(surv ~ rcs(ranky, c(5, 25, 50, 75, 95)) +  duration_co2 +multilevel2_label , data=results)
p <- cph(surv ~ rcs(ranky, c(10, 25, 50, 75, 90)) +  duration_co2 +multilevel2_label , data=results)



#Interaction
#p <- cph(surv ~ multilevel2_label*rcs(ranky, 4) +  duration_co2  , data=results)


cox.zph(p, "rank") # tests of PH
anova(p)
ggplot(Predict(p, ranky, fun=exp)) 


pdata <- Predict(p, ranky, ref.zero = TRUE, fun = exp)

ggplot(data = pdata) +
  geom_hline(aes(yintercept = 1), linetype = 3) +
  labs(x = "Average MAP percentile",
       y = "Hazard Ratio (95% CI) vs. 50th Percentile",
       title = "Diagnosis hazard ratio as a function of MAP percentile, \ncontrolling for procedure length and type",
       subtitle = "Natural spline: knots at 10, 25, 50, 75, and 90 percentile")



#############################################################
#Try with all variables included from the fully adjusted model:
#############################################################

a <- cph(surv ~ rcs(ranky, c(10, 25, 50, 75, 90)) +  duration_co2 +race2 +ethnicity2+ gender +multilevel2_label +technique_final +avg_sevo +avg_iso 
         +avg_des +avg_nitrous + total_prop_w_bolus +bin_sevo +bin_iso +bin_des +bin_nitrous +bin_prop +
           cvd +respiratory +renal +gi +hemato_immu +metabolic +scol +neonatal +agecat, data=results)

#cox.zph(a, "rank") # tests of PH
anova(a)

plot(Predict(a, name="ranky"), lty=2, lwd=2)
ggplot(Predict(a, ranky, fun=exp)) 




pdata <- Predict(a, ranky, ref.zero = TRUE, fun = exp)

ggplot(data = pdata) +
  geom_hline(aes(yintercept = 1), linetype = 3) +
  labs(x = "Average MAP percentile",
       y = "Hazard Ratio (95% CI) vs. 50th Percentile",
       title = "Diagnosis hazard ratio as a function of MAP percentile, \ncontrolling for all covariates",
       subtitle = "Natural spline: knots at 10, 25, 50, 75, and 90 percentile")



#######
#######
#######
#######Add linear line from regression overlaid#####
#######
#######
#######
#######







########################
#Trying different knots
#######################
p <- cph(surv ~ rcs(ranky, quantile(ranky, c(0,.05,.275,.5,.775,.95,1), include.lowest=TRUE)), data=results)

cox.zph(p, "rank") # tests of PH
anova(p)
ggplot(Predict(p, ranky, fun=exp)) 


p <- cph(surv ~ rcs(ranky, quantile(ranky, c(0,.05,.275,.5,.775,.95,1), include.lowest=TRUE)) +  duration_co2 +multilevel2_label , data=results)

cox.zph(p, "rank") # tests of PH
anova(p)
ggplot(Predict(p, ranky, fun=exp)) 



p <- cph(surv ~ rcs(ranky, quantile(ranky, c(0,.05,.275,.5,.775,.95,1), include.lowest=TRUE))+  duration_co2 +race2 +ethnicity2+ gender +multilevel2_label +technique_final +avg_sevo +avg_iso 
         +avg_des +avg_nitrous + total_prop_w_bolus +bin_sevo +bin_iso +bin_des +bin_nitrous +bin_prop +
           cvd +respiratory +renal +gi +hemato_immu +metabolic +scol +neonatal +agecat
         , data=results)

cox.zph(p, "rank") # tests of PH
anova(p)
ggplot(Predict(p, ranky, fun=exp)) 
########################
#End trying different knots
#######################








#Using hmisc package
rcspline.plot(results$ranky,results$censor,model=c("cox"), event=results$outcome, nk=4,
              show=c("xbeta","prob"), adj=cbind(results$gender, results$duration_co2, results$multilevel2_label),
              plotcl=TRUE)



rcspline.plot(results$ranky,results$censor,model=c("cox"), event=results$outcome, nk=4,
              show=c("xbeta","prob"),
              plotcl=TRUE)









#https://www.hindawi.com/journals/cmmm/2013/745742/
#https://www.hindawi.com/journals/cmmm/2013/745742/
#https://www.hindawi.com/journals/cmmm/2013/745742/
#https://www.hindawi.com/journals/cmmm/2013/745742/
#https://www.hindawi.com/journals/cmmm/2013/745742/
library(smoothHR)

#try smooth hr
#Can use natural splines or psplines
results<-data.frame(results)

df1<-smoothHR::dfmacox (time= "censor", status
              = "outcome", nl.predictors = c ("ranky"),
              smoother = "ns", method = "AIC",
              data = results)


hr1<-smoothHR (time = "censor", status
               = "outcome", 
               formula = ~ns (ranky, df =df1$df), data = results)


plot (hr1, predictor = "ranky",
      prob = .5, conf.level = 0.95, ref.label =
        "Ref.", xaxt = "n", main = "", xlab
      = "Fasting glucose(mg/dL)")




#Try more predictors
df2<-smoothHR::dfmacox (time= "censor", status
                        = "outcome", nl.predictors = c ("ranky"),
                        other.predictors
                        = c ("duration_co2",
                             "gender",
                             "race2",
                             "ethnicity2"
                             ),
                        smoother = "ns", method = "AIC",
                        data = results)


# fit.mvcox.1<-coxph (Surv (censor, outcome)
#                     ~ns (ranky, df = df2$df[1])
#                     + duration_co2 + gender,
#                     data = results, x = TRUE)

fit.mvcox.1<-coxph (Surv (censor, outcome)
                    ~ns (ranky, df = 4)
                    + duration_co2 + gender,
                    data = results, x = TRUE)



hr2<-smoothHR (data = results,
                  coxfit = fit.mvcox.1)



#Getting the same spline as the one from SAS
plot (hr2, predictor = "ranky",
      prob = .5, conf.level = 0.95, ref.label =
        "Ref.", xaxt = "n", main = "", xlab
      = "Fasting glucose(mg/dL)")






# +  duration_co2 +race2 +ethnicity2+ gender +multilevel2_label +technique_final +avg_sevo +avg_iso 
# +avg_des +avg_nitrous + total_prop_w_bolus +bin_sevo +bin_iso +bin_des +bin_nitrous +bin_prop +
#   cvd +respiratory +renal +gi +hemato_immu +metabolic +scol +neonatal +agecat
# 

#When you add the remaining variables, first the function breaks,
#but the plot does not change much at all, why does the rms plot change so drastically when the other variables are added?
fit.mvcox.2<-coxph (Surv (censor, outcome)
                    ~ns (ranky, df = 4)
                    +  duration_co2 +race2 +ethnicity2+ gender +multilevel2_label +technique_final +avg_sevo +avg_iso 
                    +avg_des +avg_nitrous + total_prop_w_bolus + agecat,
# +bin_sevo +bin_iso +bin_des +bin_nitrous +bin_prop +cvd +respiratory +renal +gi +hemato_immu +metabolic +scol +neonatal +agecat,
                    data = results, x = TRUE)




fit.mvcox.2<-coxph (Surv (censor, outcome)
                    ~ns (ranky, df = 4)
                    +  duration_co2 +race2 +ethnicity2+ gender +multilevel2_label +agecat,
                    data = results, x = TRUE)



hr3<-smoothHR (data = results,
               coxfit = fit.mvcox.2)



#Getting the same spline as the one from SAS
plot (hr3, predictor = "ranky",
      prob = .5, conf.level = 0.95, ref.label =
        "Ref.", xaxt = "n", main = "", xlab
      = "Fasting glucose(mg/dL)")
















###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
#https://cran.r-project.org/web/packages/survival/vignettes/splines.pdf



require(survival)

mfit1 <- coxph(Surv(censor, outcome) ~ gender + rcs(ranky, df=4), data=results)
mfit1
termplot(mfit1, term=2, se=TRUE, col.term=1, col.se=1)


mfit2 <- coxph(Surv(censor, outcome) ~ duration_co2 + rcs(ranky, df=4)+race2 +ethnicity2+ gender +multilevel2_label +technique_final +avg_sevo +avg_iso 
              +avg_des +avg_nitrous + total_prop_w_bolus +bin_sevo +bin_iso +bin_des +bin_nitrous +bin_prop +
                cvd +respiratory +renal +gi +hemato_immu +metabolic +scol +neonatal +agecat
              , data=results)
mfit2
termplot(mfit2, term=2, se=TRUE, col.term=1, col.se=1, ylim=c(-.6, 0.6))







#################################################################
#Try looking at all different combos of df this is with psplines
fit <- coxph(Surv(censor, outcome) ~ gender + pspline(ranky, 4), results)


termplot(fit, se=TRUE, col.term=1, col.se=1, term=2,
           xlab="MAP", ylim=c(-.4, 1.3))

df <- c(3, 2.5, 2)

for (i in 1:3) {
  tfit <- coxph(Surv(censor, outcome) ~ gender +
                  pspline(ranky, df[i], nterm=8), results)
  temp <- termplot(tfit, se=FALSE, plot=FALSE, term=2)
  lines(temp$ranky$x, temp$ranky$y, col=i+1, lwd=2)
}
legend(14, 1, paste("df=", c(4, df)), lty=1, col=1:4, lwd=2)















########################################################
########################################################
########################################################
########################################################
#Test for linearity
library(survminer)
ggcoxfunctional(Surv(censor, outcome) ~ ranky+ sqrt(ranky), data = results)


fit1 <- coxph(surv ~ ranky, data=results)
fit2 <- coxph(surv ~ ranky + multilevel2_label , data=results)
summary(fit1)

summary(fit2)


anova(fit1, fit2, test = "Chisq")



ggcoxdiagnostics(fit1, type = "martingale",
                 linear.predictions = FALSE, ggtheme = theme_bw())






# One "iron clad" way to test is to fit a model that has the variable of interest
# "x" as a linear term, then a second model with splines, and do a likelihood
# ratio test with 2*(difference in log-likelihood) on (difference in df) degrees
# of freedom. With a penalized model this test is conservative: the chi-square is
# not quite the right distribution, the true dist has the same mean but smaller
# variance.
# 
# The pspline function uses an evenly spaced set of symmetric basis functions. A
# neat consequence of this is that the Wald test for linear vs 'more general' is a
# test that the coefficients of the spline terms fall in a linear series. That
# is, a linear trend test on the coefficients. This is what coxph does. As with
# the LR test, the chi-square dist is conservative. I have not worked at putting
# in the more correct distribution. See Eilers and Marx, Statistical Science
# 1986.
# And what is the null for the non-linear test?
#   The linear test is "is a linear better than nothing", the non-linear one is a
# sequential test "is the non-linear better than the linear". The second test of
# course depends on the total number of df you allowed for the pspline fit. As a
# silly example adding "+ pspline(x, df 0)" would likely show that the nonlinear
# term was not a significant addition, i.e., not worth 199 more degrees of
# freedom.

#Lin null is that x is linear
#non lin null is that x is not linear
#Saying x is linear

coxph(Surv(censor, outcome) ~ gender + pspline(ranky), results)



coxph(Surv(censor, outcome) ~ duration_co2 +race2 +ethnicity2+ gender +multilevel2_label +technique_final +avg_sevo +avg_iso 
      +avg_des +avg_nitrous + total_prop_w_bolus +bin_sevo +bin_iso +bin_des +bin_nitrous +bin_prop +
        cvd +respiratory +renal +gi +hemato_immu +metabolic +scol +neonatal +agecat
      +pspline(ranky), results)








