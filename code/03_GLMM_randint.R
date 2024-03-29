# Import libraries
library(tidyverse)
library(DHARMa)
library(lme4)
library(ggstats)
library(performance)
library(glmmTMB)
library(here)
library(MASS)
library(broom.mixed)
library(patchwork)



# Read data
df <- read.csv(here("rawdata", "june1data.csv")) %>% 
  filter(!(Intervention %in% c("wash", "wash2"))) %>%       # remove wash periods
  mutate(Intervention = recode(Intervention,
                                  "wash" = "Washout",
                                  "wash2" = "Washout",
                                  "preint" = "Pre/pos-intervention",
                                  "follow" = "Pre/pos-intervention",
                                  "dis " = "Discount",
                                  "dismes" = "Discount +\nmessaging",
                                  "cal" = "Caloric content \nmessaging",
                                  "excer" = "Exercise equivalents \nmessaging",
                                  "both" = "Both \nmessages"), # set all control periods to preint
         Site = recode(Site,
                       "chop" = "Site A",
                       "HF" = "Site B",
                       "NS" = "Site C"),
            Total2 = ZeroCal + Sugary)        # add sum of zerocal and sugary for future use
  


# set preint as baseline
df$Intervention = relevel(as.factor(df$Intervention),ref = "Pre/pos-intervention") 

## Plotting CI's to see if random slopes and intercepts are needed

df_chop = subset(df, Site == "Site A")
df_HF = subset(df, Site == "Site B")
df_NS = subset(df, Site == "Site b")

df_dis = subset(df, Intervention == "Discount")
df_dismes = subset(df, Intervention == "Discount +\nmessaging")
df_cal = subset(df, Intervention == "Caloric content \nmessaging")
df_excer = subset(df, Intervention == "Exercise equivalents \nmessaging")
df_both = subset(df, Intervention == "Both \nmessages")

# preliminary GLM run to check if intervention-specific random effect is needed
# zeroCal
glm0=glm(ZeroCal~Site+offset(log(Total)), data = df_dis, family=poisson)
glm1=glm(ZeroCal~Site+offset(log(Total)), data = df_dismes, family=poisson)
glm2=glm(ZeroCal~Site+offset(log(Total)), data = df_cal, family=poisson)
glm3=glm(ZeroCal~Site+offset(log(Total)), data = df_excer, family=poisson)
glm4=glm(ZeroCal~Site+offset(log(Total)), data = df_both, family=poisson)

models <- list("Discount" = glm0,
               "Discount +\nmessaging" = glm1,
               "Caloric content \nmessaging" = glm2,
               "Exercise equivalents \nmessaging" = glm3,
               "Both \nmessages" = glm4)

model <- ggcoef_compare(models, intercept = T)


glmnb0=glm.nb(ZeroCal~Site+offset(log(Total)), data = df_dis)
glmnb1=glm.nb(ZeroCal~Site+offset(log(Total)), data = df_dismes)

glmnb2=glm.nb(ZeroCal~Site+offset(log(Total)), data = df_cal)
glmnb3=glm.nb(ZeroCal~Site+offset(log(Total)), data = df_excer)
glmnb4=glm.nb(ZeroCal~Site+offset(log(Total)), data = df_both)

modelsnb <- list("Discount" = glmnb0,
               "Discount +\nmessaging" = glmnb1,
               "Caloric content \nmessaging" = glmnb2,
               "Exercise equivalents \nmessaging" = glmnb3,
               "Both \nmessages" = glmnb4)

# modelnb <- ggcoef_compare(modelsnb, intercept = T,
#                           conf.level = 1-0.05/46)



# First question analysis -----

## Test effectiveness of interventions ####
df$Intervention = relevel(as.factor(df$Intervention),ref = "Pre/pos-intervention")
# zeroCal
# poisson
glmm_zerocalposs=glmmTMB(ZeroCal~Intervention + (0+Site|Intervention) + offset(log(Total)), data = df, family=poisson)

# poss_zerocal <- plot(simulateResiduals(glmm_zerocal))
# negative binomial
glmm_zerocalnb=glmmTMB(ZeroCal~Intervention + (0+Site|Intervention) + offset(log(Total)), 
                       data = df, family=nbinom2)

# nb_zerocal <- plot(simulateResiduals(glmm_zerocal))

glmm_sugary=glmmTMB(Sugary~Intervention + (0+Site|Intervention) + offset(log(Total)), 
                    data = df, family=nbinom2)

#Plot intervention ----
# ggcoef_model(glmm_zerocalnb, intercept = T,
#              include = c("Intervention")) +
#   labs(subtitle = "Zero-Calorie Beverage") +
# ggcoef_model(glmm_sugary, intercept = T,
#              include = c("Intervention")) +
#   labs(subtitle = "Sugary Beverage ") 



# Second question plot ------
# chop baseline
df$Site = relevel(as.factor(df$Site),ref = "Site A")
# zeroCal
glmm_zerocal_chop=glmmTMB(ZeroCal~Intervention*Site + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)

# broom.mixed::tidy(glmm_zerocal_chop) 
# => overdispersion not detected + lest terrible KS test

# sugary
glmm_sugary_chop=glmmTMB(Sugary~Intervention*Site + (0+Site|Intervention) + offset(log(Total)), data = df, family=nbinom2)





