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

tab <- table(cofd$CofD)
tab
prop.table(tab)*100

tab <- table(cofd$Sex)
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

tab <- table( cofd$CofD, cofd$Age_cat)
tab
prop.table(tab,1)*100
chisq.test(tab)
