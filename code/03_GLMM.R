setwd("../rawdata")
df <- read.csv("june1data.csv")

library(tidyverse)

df = mutate(df,
            Intervention = recode(Intervention,
                                  "follow" = "preint"),
            Total2 = ZeroCal + Sugary)%>%
  filter(!(Intervention %in% c("wash", "wash2")))

df$Intervention = relevel(as.factor(df$Intervention),ref = "preint")

library(lme4)
df_chop = subset(df, Site == "chop")
df_HF = subset(df, Site == "HF")
df_NS = subset(df, Site == "NS")
glm0=glm(ZeroCal~Intervention+offset(log(Total2)), data = df_chop, family=poisson)
glm1=glm(ZeroCal~Intervention+offset(log(Total2)), data = df_HF, family=poisson)
glm2=glm(ZeroCal~Intervention+offset(log(Total2)), data = df_NS, family=poisson)

models <- list("chop" = glm0,
               "HF" = glm1,
               "NS" = glm2)

library(ggstats)
ggcoef_compare(models, intercept = T)

glmm_zerocal1=glmer(ZeroCal~Intervention+(Intervention+1|Site)+offset(log(Total2)), data = df, family=poisson)
summary(glmm_zerocal)

glmm_sugary1=glmer(Sugary~Intervention+(Intervention+1|Site)+offset(log(Total2)), data = df, family=poisson)
summary(glmm_sugary)

glmm_zerocal2=glmer(ZeroCal~Site*Intervention+(Intervention+1|Site)+offset(log(Total)), data = df, family=poisson)
summary(glmm_zerocal2)

df$Intervention = relevel(as.factor(df$Intervention),ref = "both")
glmm_zerocal_comb3 = glmer(ZeroCal~Intervention+(Intervention+1|Site)+offset(log(Total)), data = df, family=poisson)
summary(glmm_zerocal_comb3)

df$Intervention = relevel(as.factor(df$Intervention),ref = "cal")
glmm_zerocal4 = glmer(ZeroCal~Intervention+(Intervention+1|Site)+offset(log(Total)), data = df, family=poisson)
summary(glmm_zerocal4)

