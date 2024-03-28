setwd("../rawdata")
# read data
df <- read.csv("june1data.csv")

# import libraries
library(tidyverse)
library(DHARMa)
library(lme4)
library(ggstats)
library(performance)
library(glmmTMB)


# reformat data
df = mutate(df,
            Intervention = recode(Intervention,
                                  "follow" = "preint"), # set all control periods to preint
            Total2 = ZeroCal + Sugary)%>%               # add sum of zerocal and sugary for future use
  filter(!(Intervention %in% c("wash", "wash2")))       # remove wash periods

# set preint as baseline
df$Intervention = relevel(as.factor(df$Intervention),ref = "preint") 

# get site-specific and intervention-specific data
df_chop = subset(df, Site == "chop")
df_HF = subset(df, Site == "HF")
df_NS = subset(df, Site == "NS")

df_dis = subset(df, Intervention == df$Intervention[45])
df_dismes = subset(df, Intervention == "dismes")
df_cal = subset(df, Intervention == "cal")
df_excer = subset(df, Intervention == "excer")
df_both = subset(df, Intervention == "both")



## Preliminary GLM run to check if site-specific random effect is needed ####
# zeroCal
glm0=glm(ZeroCal~Intervention+offset(log(Total)), data = df_chop, family=poisson)
glm1=glm(ZeroCal~Intervention+offset(log(Total)), data = df_HF, family=poisson)
glm2=glm(ZeroCal~Intervention+offset(log(Total)), data = df_NS, family=poisson)

models <- list("chop" = glm0,
               "HF" = glm1,
               "NS" = glm2)

ggcoef_compare(models, intercept = T)
# sugary
glm0=glm(Sugary~Intervention+offset(log(Total)), data = df_chop, family=poisson)
glm1=glm(Sugary~Intervention+offset(log(Total)), data = df_HF, family=poisson)
glm2=glm(Sugary~Intervention+offset(log(Total)), data = df_NS, family=poisson)

models <- list("chop" = glm0,
               "HF" = glm1,
               "NS" = glm2)

ggcoef_compare(models, intercept = T)
# ==> site-specific random effect is present but should not be included due to small level size (3 sites)


# preliminary GLM run to check if intervention-specific random effect is needed
# zeroCal
glm0=glm(ZeroCal~Site+offset(log(Total)), data = df_dis, family=poisson)
glm1=glm(ZeroCal~Site+offset(log(Total)), data = df_dismes, family=poisson)
glm2=glm(ZeroCal~Site+offset(log(Total)), data = df_cal, family=poisson)
glm3=glm(ZeroCal~Site+offset(log(Total)), data = df_excer, family=poisson)
glm4=glm(ZeroCal~Site+offset(log(Total)), data = df_both, family=poisson)

models <- list("dis" = glm0,
               "dismes" = glm1,
               "cal" = glm2,
               "excer" = glm3,
               "both" = glm4)

ggcoef_compare(models, intercept = T)
# sugary
glm0=glm(Sugary~Site+offset(log(Total)), data = df_dis, family=poisson)
glm1=glm(Sugary~Site+offset(log(Total)), data = df_dismes, family=poisson)
glm2=glm(Sugary~Site+offset(log(Total)), data = df_cal, family=poisson)
glm3=glm(Sugary~Site+offset(log(Total)), data = df_excer, family=poisson)
glm4=glm(Sugary~Site+offset(log(Total)), data = df_both, family=poisson)

models <- list("dis" = glm0,
               "dismes" = glm1,
               "cal" = glm2,
               "excer" = glm3,
               "both" = glm4)

ggcoef_compare(models, intercept = T)
# ==> intervention-specific random effect is needed for slope only




## Test effectiveness of interventions ####
df$Intervention = relevel(as.factor(df$Intervention),ref = "preint")
# zeroCal
glmm_zerocal=glmmTMB(ZeroCal~Intervention + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_zerocal)

check_overdispersion(glmm_zerocal)
plot(simulateResiduals(glmm_zerocal))

# sugary
glmm_sugary=glmmTMB(Sugary~Intervention + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_sugary)

check_overdispersion(glmm_sugary)
plot(simulateResiduals(glmm_sugary))

# ==> Conclusion: dismes increases zeroCal

## Test if effectiveness of interventions varies based on site ####
# chop baseline
df$Site = relevel(as.factor(df$Site),ref = "chop")
# zeroCal
# poisson
glmm_zerocal_chop=glmmTMB(ZeroCal~Intervention*Site + (0+Site|Intervention) + offset(log(Total)), data = df, family=poisson)
summary(glmm_zerocal_chop)

check_overdispersion(glmm_zerocal_chop)
plot(simulateResiduals(glmm_zerocal_chop))
# => overdispersion detected + terrible KS test
# negative binomial
glmm_zerocal_chop=glmmTMB(ZeroCal~Intervention*Site + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_zerocal_chop)

check_overdispersion(glmm_zerocal_chop)
plot(simulateResiduals(glmm_zerocal_chop))
# => overdispersion not detected + lest terrible KS test

# sugary
glmm_sugary_chop=glmmTMB(Sugary~Intervention*Site + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_sugary_chop)

check_overdispersion(glmm_sugary_chop)
plot(simulateResiduals(glmm_sugary_chop))
# => overdispersion not detected + less terrible KS test

# HF baseline
df$Site = relevel(as.factor(df$Site),ref = "HF")
# zeroCal
glmm_zerocal_HF=glmmTMB(ZeroCal~Intervention*Site + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_zerocal_HF)

check_overdispersion(glmm_zerocal_HF)
plot(simulateResiduals(glmm_zerocal_HF))
# => overdispersion not detected + terrible KS test

# sugary
glmm_sugary_HF=glmmTMB(Sugary~Intervention*Site + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_sugary_HF)

check_overdispersion(glmm_sugary_HF)
plot(simulateResiduals(glmm_sugary_HF))
# => overdispersion not detected + terrible KS test

# ==> Conclusions:
# * poisson cause overdispersion - use nbinom2 instead
# * For zeroCal:
#   dis, dismes and both became significant after changing baseline to HF (!?)
#   chop > NS > HF
#   dis_chop < dis_HF
#   dismes_NS > dismes_chop,HF
#   both_HF < both_chop,NS
# * For sugary:
#   chop > NS,HF
#   dismes_NS > dismes_chop,HF

## Effectiveness of both compared to cal and excer ####
df$Intervention = relevel(as.factor(df$Intervention),ref = "both")
# zeroCal
glmm_zerocal_both=glmmTMB(ZeroCal~Intervention + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_zerocal_both)

check_overdispersion(glmm_zerocal_both)
plot(simulateResiduals(glmm_zerocal_both))

# sugary
glmm_sugary_both=glmmTMB(Sugary~Intervention + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_sugary_both)

check_overdispersion(glmm_sugary_both)
plot(simulateResiduals(glmm_sugary_both))

# ==> Conclusion: no significant results found


## Effectiveness of dis compared to dismes ####
df$Intervention = relevel(as.factor(df$Intervention),ref = "dismes")
# zeroCal
glmm_zerocal_dis=glmmTMB(ZeroCal~Intervention + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_zerocal_dis)

check_overdispersion(glmm_zerocal_dis)
plot(simulateResiduals(glmm_zerocal_dis))

# sugary
glmm_sugary_dis=glmmTMB(Sugary~Intervention + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_sugary_dis)

check_overdispersion(glmm_sugary_dis)
plot(simulateResiduals(glmm_sugary_dis))

# ==> Conclusion: dismes is better than dis for zeroCal


## Effectiveness of cal compared to excer ####
df$Intervention = relevel(as.factor(df$Intervention),ref = "cal")
# zeroCal
glmm_zerocal_cal=glmmTMB(ZeroCal~Intervention + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_zerocal_cal)

check_overdispersion(glmm_zerocal_cal)
plot(simulateResiduals(glmm_zerocal_cal))

# sugary
glmm_sugary_cal=glmmTMB(Sugary~Intervention + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)
summary(glmm_sugary_cal)

check_overdispersion(glmm_sugary_cal)
plot(simulateResiduals(glmm_sugary_cal))

# ==> Conclusion: no significant results found
