##### LME analysis #####
library(ggplot2)
library(lme4)
library(nlme)
library(dplyr)
library(knitr)
library(tidyverse)
library(ggstats)

### import data
raw <- read.csv("rawdata/june1data.csv")



### restructure the data for GLME
# delete NAs
raw <- raw[!is.na(raw$ZeroCal),]
# remove washup periods
raw <- raw[!(raw$Intervention %in% c('wash','wash2')),]
# rename follow to preint
raw$Intervention[raw$Intervention == 'follow'] <- 'preint'
# make preint the reference
raw$Intervention <- relevel(as.factor(raw$Intervention),ref = "preint")
# reset time for each period
for (inte in unique(raw$Intervention)){
  for (s in unique(raw$Site)){
    idx = (raw$Intervention==inte)&(raw$Site==s)
    raw$Count[idx] = raw$Count[idx] - min(raw$Count[idx])
  }
}
# adding weekend indicator
raw$Weekend = raw$DofW > 5





glm_zeroCal <- glm(ZeroCal ~ Intervention + Site + Weekend + offset(log(Total)), data = raw, family = poisson)
summary(glm_zeroCal)

glme_zeroCal <- glmer(ZeroCal ~ Intervention + (Intervention|Site) + offset(log(Total)), data = raw, family = poisson)
summary(glme_zeroCal)


glm_zeroCal <- list()
Site_names <- unique(raw$Site)
for (i in 1:3){
  glm_zeroCal[[i]] <- glm(ZeroCal ~ Intervention + offset(log(Total)), data = raw[raw$Site==Site_names[i],], family = poisson)
}

summary(glm_zeroCal[[1]])
summary(glm_zeroCal[[2]])
summary(glm_zeroCal[[3]])


modelplot(glm_zeroCal[[1]])



glm_zeroCal <- glm(ZeroCal ~ Intervention + Site + Weekend + offset(log(Total)), data = raw, family = poisson)
summary(glm_zeroCal)

## Sugary drinks
sug <- data[data$beverage=='Sugary',]
sug <- sug[!is.na(sug$trend),]
sug <- sug[sug$count>21,]
sug <- sug[!(sug$intervention %in% c('wash','wash2','follow')),]
sug$intervention <- relevel(as.factor(sug$intervention),ref = "preint")
for (inte in unique(sug$intervention)){
  for (s in unique(sug$site)){
    idx = (sug$intervention==inte)&(sug$site==s)
    sug$count[idx] = sug$count[idx] - min(sug$count[idx])
  }
}






lm_zeroCal <- lm(trend ~ count + intervention + site,data=zeroCal)
summary(lm_zeroCal)

lm_sug <- lm(trend ~ count + intervention + site,data=sug)
summary(lm_sug)


lme_zeroCal <- lmer(trend ~ count*intervention+ site + (1|site), data=zeroCal)
summary(lme_zeroCal)
