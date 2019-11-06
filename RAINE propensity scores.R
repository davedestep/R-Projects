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







##########
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





##############################################################################
####                       Matching                                       ####
##############################################################################
ps.scores <- raines2.ps$ps$ks.mean.ATE
ps.scores2 <- raines2.ps$ps$ks.max.ATE
ps.scores3 <- raines2.ps$ps$es.mean.ATE
ps.scores4 <- raines2.ps$ps$es.max.ATE


df.mv <- raines2.ps$data %>%
  bind_cols(ps = ps.scores) %>% bind_cols(ps = ps.scores2)%>% bind_cols(ps = ps.scores3)%>% bind_cols(ps = ps.scores4) %>% 
  filter(!is.na(y10tott))

matched.mv <- Match(Y = df.mv$y10tott,
                    Tr = df.mv$ga_updated,
                    X = df.mv$ps,
                    estimand = 'ATE',
                    caliper = 0.2,
                    M = 3,
                    replace = FALSE,
                    version = 'standard') # version = 'fast'


# Regenerate matched data frames using the indices returned by the Match function
index.treated <- matched.mv$index.treated
index.control <- matched.mv$index.control
df.matched.mv <- rbind(df.mv[row.names(df.mv) %in% index.treated, ], 
                       df.mv[row.names(df.mv) %in% index.control, ])



#Linear model
ATE.TOTTT.Reg <- glm(y10tott~ga_updated + sex+ m_race_new + m_schlvl_new + income_new+ u_smkave_new+ u_alcoh_new, data=df.matched.mv)
summary(ATE.TOTTT.Reg)               









###########################################################
###########################################################
###########################################################
###########################################################
#Matching using matchit
###########################################################
###########################################################
###########################################################
library(MatchIt)
library(kableExtra)

pacman::p_load(tableone)
table1 <- CreateTableOne(vars = c('sex', 'm_schlvl_new', 'income_new', 'u_smkave_new', 'u_alcoh_new', 'y10tott'), 
                         data = raines2, 
                         factorVars = c('sex', 'm_schlvl_new', 'income_new', 'u_smkave_new', 'u_alcoh_new'), 
                         strata = 'ga_updated')
table1 <- print(table1, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table1[,1:3],  
      align = 'c', 
      caption = 'Table 1: Comparison of unmatched samples') %>% kable_styling

raines3<-raines2 %>% 
  dplyr::select(ga_updated, y10tott, sex, m_race_new, m_schlvl_new, income_new, u_smkave_new, u_alcoh_new) %>% 
  filter(!is.na(y10tott))

match.it <- matchit(ga_updated ~  sex+ m_race_new + m_schlvl_new + income_new+ u_smkave_new+ u_alcoh_new, data = raines3, method="nearest", ratio=5)
a <- summary(match.it)

kable(a$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Sample sizes') %>% 
  kable_styling

kable(a$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data') %>% 
  kable_styling

plot(match.it, type = 'jitter', interactive = FALSE)


df.match <- match.data(match.it)[1:ncol(raines3)]


pacman::p_load(tableone)
table4 <- CreateTableOne(vars = c('sex', 'm_schlvl_new', 'income_new', 'u_smkave_new', 'u_alcoh_new', 'y10tott'), 
                         data = df.match, 
                         factorVars = c('sex', 'm_schlvl_new', 'income_new', 'u_smkave_new', 'u_alcoh_new'), 
                         strata = 'ga_updated')
table4 <- print(table4, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table4[,1:3],  
      align = 'c', 
      caption = 'Table 4: Comparison of matched samples') %>% 
  kable_styling


ATE.TOTTT.Reg <- glm(y10tott~ga_updated, data=df.match)
summary(ATE.TOTTT.Reg)               







##############################################################################
#Can't pull out columns from a data with class "PS", is there a fix?
matches <- function(x) {
  
  df.mv <- raines2.ps[[data]])
  # %>%
  #   bind_cols(ps = ps.scores)
}
  df.mv <-df.mv[!is.na(df.mv[["x"]]),]
  
}
  
  matched.mv <- Match(Y = df.mv[[x]],
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
  x.df.matched.mv <- rbind(df.mv[row.names(df.mv) %in% index.treated, ], 
                         df.mv[row.names(df.mv) %in% index.control, ])
  
}

matches()

matches("y10intt")


