setwd("../rawdata")
# read data
df <- read.csv("june1data.csv")

# import libraries
library(tidyverse)
library(lme4)
library(ggstats)
library(performance)

# reformat data
df = mutate(df,
            Intervention = recode(Intervention,
                                  "follow" = "preint"), # set all control periods to preint
            Total2 = ZeroCal + Sugary)%>%               # add sum of zerocal and sugary for future use
  filter(!(Intervention %in% c("wash", "wash2")))       # remove wash periods


# set preint as baseline
df$Intervention = relevel(as.factor(df$Intervention),ref = "preint") 

## Plotting CI's to see if random slopes and intercepts are needed
library(lme4)

df_chop = subset(df, Site == "chop")
df_HF = subset(df, Site == "HF")
df_NS = subset(df, Site == "NS")

df_dis = subset(df, Intervention == df$Intervention[45])
df_dismes = subset(df, Intervention == "dismes")
df_cal = subset(df, Intervention == "cal")
df_excer = subset(df, Intervention == "excer")
df_both = subset(df, Intervention == "both")

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

model <- ggcoef_compare(models, intercept = T)
model+ggtitle("CIs for consumption of Zero Calorie Beverages")
