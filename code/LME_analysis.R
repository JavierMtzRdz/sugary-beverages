##### LME analysis #####
library(ggplot2)
library(lme4)
library(nlme)
library(dplyr)
library(knitr)
library(tidyverse)

### import data
raw <- read.csv("rawdata/june1data.csv")
sug_bev_trend <- read.csv("gendata/sug_bev_decompos.csv")
data <- sug_bev_trend[,c('beverage','site','intervention','count','trend')]

### restructure the data for LME
## ZeroCal drinks
zeroCal <- data[data$beverage=='Zero-calorie',]
# delete NAs
zeroCal <- zeroCal[!is.na(zeroCal$trend),]
# use only the preint in the middle
zeroCal <- zeroCal[zeroCal$count>21,]
# remove washup and follow periods
zeroCal <- zeroCal[!(zeroCal$intervention %in% c('wash','wash2','follow')),]
# make preint the reference
zeroCal$intervention <- relevel(as.factor(zeroCal$intervention),ref = "preint")
# reset time for each period
for (inte in unique(zeroCal$intervention)){
  for (s in unique(zeroCal$site)){
    idx = (zeroCal$intervention==inte)&(zeroCal$site==s)
    zeroCal$count[idx] = zeroCal$count[idx] - min(zeroCal$count[idx])
  }
}


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
