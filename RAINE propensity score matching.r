library(twang)
library(Matching)
#library(cobalt)
library(broom)
library(tidyverse)


# ***********************************************************************************************
# Run TWANG #####################################################################################
# ***********************************************************************************************


raines<-haven::read_sas("C:\\Users\\Dave\\Desktop\\ALL\\Raine\\2018 Data\\Joined_single_r.sas7bdat")
str(raines)

raines<-raines %>% janitor::clean_names()

#OG model
#prenatal_GA sex m_race1 M_schlvl1 income1 N_weight1 CS_cat epid prem rsurg123 rub2


raines2<-raines %>% 
  select(y10_cf15,
         y10_cf16,
         y10_cf18,
         y10_cpm4,
         swamse,
         y10intt,
         y10extt,
         y10tott,
         y10_pb4,
         y10_sd2,
         y10_sd1,
         y10_ma30,ga_updated, sex, m_race_new, m_schlvl_new, income_new, 
         u_smkave_new, u_alcoh_new, mh_dx, mh_epi, mh_hd, mh_psyc, mh_thyr, cs_cat, epid, prem, pet, n_iugr, n_weight_new
        ) 


raines2[sapply(raines2, is.character)] <- lapply(raines2[sapply(raines2, is.character)], as.factor)


raines2<-raines2 %>% mutate( cs_cat=as.factor(cs_cat))


raines2=as.data.frame(raines2)


##################################################################################################################
#Taking out race because there is only one non caucasian in the exposed group. Which made this particular person's propensity score MUCH larger than the rest
ps_formula<-as.formula(ga_updated ~ sex+ m_race_new + m_schlvl_new+ income_new+ u_smkave_new+ u_alcoh_new+ mh_dx+ mh_epi+ mh_hd+ mh_psyc+ mh_thyr ) 
#mothers age


raines2.ps <- ps(as.formula(ps_formula),
                data = raines2,
                n.trees = 10000,
                perm.test.iters = 0, 
                verbose = TRUE,
                estimand = 'ATT', #ATT or ATE?
                #stop.method = c("es.mean","ks.max")
                stop.method = c("ks.mean", "ks.max", "es.mean", "es.max")
                #stop.method = 'ks.mean'
)




################################################
df.mv %>% 
  mutate(ga_updated=as.factor(ga_updated)) %>% 
  ggplot(aes(x=ps))+
  geom_histogram()+
  facet_grid(~ga_updated)+theme_bw()
  


df.mv %>% 
  mutate(ga_updated=as.factor(ga_updated)) %>% 
  group_by(ga_updated) %>% 
  summarise(mean=mean(ps))

plot(raines2.ps)

summary(raines2.ps$gbm.obj,
        n.trees=raines2.ps$desc$ks.mean.ATT$n.trees)





######################################
#Should check the overlap at this step
######################################
require(gridExtra)

plot1 <- df.mv %>% 
  mutate(ga_updated=as.factor(ga_updated)) %>% 
  ggplot(aes(x=ps, group=ga_updated, fill=ga_updated))+
  geom_histogram()+
  viridis::scale_fill_viridis(discrete = TRUE)

plot2 <- df.mv %>% 
  mutate(ga_updated=as.factor(ga_updated)) %>% 
  ggplot(aes(x=ps1, group=ga_updated, fill=ga_updated))+
  geom_histogram()+
  viridis::scale_fill_viridis(discrete = TRUE)


plot3 <- df.mv %>% 
  mutate(ga_updated=as.factor(ga_updated)) %>% 
  ggplot(aes(x=ps2, group=ga_updated, fill=ga_updated))+
  geom_histogram()+
  viridis::scale_fill_viridis(discrete = TRUE)


plot4 <- df.mv %>% 
  mutate(ga_updated=as.factor(ga_updated)) %>% 
  ggplot(aes(x=ps3, group=ga_updated, fill=ga_updated))+
  geom_histogram()+
  viridis::scale_fill_viridis(discrete = TRUE)


grid.arrange(plot1, plot2, plot3, plot4, ncol=2)




##############################################################################
####                       Matching                                       ####
##############################################################################
ps.scores <- raines2.ps$ps$ks.mean.ATE
ps.scores2 <- raines2.ps$ps$ks.max.ATE
ps.scores3 <- raines2.ps$ps$es.mean.ATE
ps.scores4 <- raines2.ps$ps$es.max.ATE


df.mv <- raines2.ps$data %>%
  bind_cols(ps = ps.scores) %>% bind_cols(ps = ps.scores2)%>% bind_cols(ps = ps.scores3)%>% bind_cols(ps = ps.scores4)
  filter(!is.na(y10intt))

matched.mv <- Match(Y = df.mv$y10intt,
                    Tr = df.mv$ga_updated,
                    X = df.mv$ps,
                    estimand = 'ATT',
                    caliper = 0.2,
                    M = 3,
                    replace = FALSE,
                    version = 'standard') # version = 'fast'


# Regenerate matched data frames using the indices returned by the Match function
index.treated <- matched.mv$index.treated
index.control <- matched.mv$index.control
df.matched.mv <- rbind(df.mv[row.names(df.mv) %in% index.treated, ], 
                       df.mv[row.names(df.mv) %in% index.control, ])


