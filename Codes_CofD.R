################################################################################
## Dengue - Data analysis ##
################################################################################
rm(list=ls()) #Remove all previous R objects#
## Packages ##
library(maptools)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(ggrepel)
library(ggplot2)
library(tidyverse)
setwd("E:\\ResearchProject\\Sorowar Sir\\Causes of Deaths")

cofd <- read.csv("DeathRegistryJamalpu.csv", header = T)

cofd$Age_cat

################################################################################
## Descriptive statistics ##
################################################################################

tab <- table(cofd$Decade)
tab
prop.table(tab)*100

tab <- table(cofd$Seasons)
tab
prop.table(tab)*100

tab <- table(cofd$Sex)
tab
prop.table(tab)*100

tab <- table(cofd$CofD)
tab
prop.table(tab)*100



tab <- table( cofd$Decade, cofd$Sex)
tab
prop.table(tab,1)*100
chisq.test(tab)

tab <- table( cofd$Seasons, cofd$Sex)
tab
prop.table(tab,1)*100
chisq.test(tab)


tab <- table( cofd$Age_cat, cofd$Sex)
tab
prop.table(tab,1)*100
chisq.test(tab)

tab <- table( cofd$CofD, cofd$Sex)
tab
prop.table(tab,1)*100
chisq.test(tab)






tab <- table(cofd$Age_cat)
tab
prop.table(tab)*100

tab <- table( cofd$Decade, cofd$Age_cat)
tab
prop.table(tab,1)*100
chisq.test(tab)

tab <- table( cofd$Seasons, cofd$Age_cat)
tab
prop.table(tab,1)*100
chisq.test(tab)


tab <- table( cofd$Sex, cofd$Age_cat)
tab
prop.table(tab,1)*100
chisq.test(tab)

tab <- table(cofd$Sex, cofd$Age_cat)
tab
prop.table(tab,1)*100
chisq.test(tab)




model <- glm(relevel(factor(cofd$Sex), ref = "Female")~ relevel(factor(cofd$Decade), ref = "2017-2021") + relevel(factor(cofd$Seasons), ref = "Rainy") 
              + relevel(factor(cofd$Age_cat), ref = "18+") + relevel(factor(cofd$CofD), ref = "Other"),
             family=binomial(link='logit'),data=cofd)
summary(model)
exp(cbind(coef(model), confint(model)))


model <- glm(relevel(factor(cofd$Age_cat), ref = "18+")~ relevel(factor(cofd$Decade), ref = "2017-2021") + relevel(factor(cofd$Seasons), ref = "Rainy") 
             + relevel(factor(cofd$Sex), ref = "Female") + relevel(factor(cofd$CofD), ref = "Other"),
             family=binomial(link='logit'),data=cofd)
summary(model)
exp(cbind(coef(model), confint(model)))



