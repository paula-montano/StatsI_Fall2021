title: "PS04.R"
author: "Paula Montano"
date: "26/11/2021"

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())

# detach all libraries

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

library(stringr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridisLite)
library(broom)
library(stargazer)
install.packages("car",dependencies=TRUE) 
library("car")

data(Prestige)
help(Prestige)
options(scipen = 999)


# Load necessary packages

lapply(c("stringr"),  pkgTest)
lapply(c("dplyr"),  pkgTest)
lapply(c("tidyverse"),  pkgTest)
lapply(c("ggplot2"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/StatsI_Fall2021/problemSets")


####################
# Problem 1
#####################

summary(Prestige) 

## (a)
Prestige$professional <- ifelse(Prestige$type == "prof", 1,0)
str(Prestige$professional)

## (b)

interact_reg <- lm(prestige ~ income + professional, income:professional, data = Prestige) 
summary(interact_reg)
stargazer(interact_reg, type = "html", title = "Regression Results")

## (c) prediction equation 

## Yi (prestige) = B0 (Intercept) + B1 (income)*Xi + B2 (professional)*Di + Ei
## Yi = prestige outcome variable, the predicted value
## B0 = (intercept: 30.6183338) 
## B1 = income (coefficient:  0.0013706) *Xi
## B2 = professional (coefficient: 22.7569999) *Di

stargazer(interact_reg <- lm(prestige ~ income + professional, income:professional, data = Prestige),
  type = "html"
)

## (d) 
##The income coefficient (0.0013706) shows a positive relationship with the outcome variable prestige. 
##The variable prestige is predicted to increase (0.0013706) when the variable income goes up one.    

## (e)
##The professional coefficient (22.7569999) shows a positive effect on the outcome variable prestige.
##The variable prestige is predicted to increase (22.7569999) when the variable professional increase by one. 

## (f)
y_incomeVar1 <- 30.6183338 + 0.0013706*1000 + 22.7569999*1 + (1000*0.0002)
y_incomeVar1 = 54.9459337

## (g)
y_incomeVar2 <- 30.6183338 + 0.0013706*6000 + 22.7569999*1 + (6000*0.0002)
y_incomeVar2 = 62.7989337

####################
# Problem 2
####################

## (a)
## Set null and alternative hypothesis:
## Ho: Yard signs in precincts do not affect vote share. B2 = 0
## Ha: Yard signs in precincts affect vote share. B2 different from 0.  

## Test Statistic 
## t = (0.042-0) / (0.016) 
## t =  2.625

## Calculate p value 
## df = N - k = 131 - 3 = 128
p <- 2*pt(2.625, 128, lower.tail = F)
## p = 0.0097200197

## (b)
## After conducting hypothesis testing, our p value is smaller than alpha (0.05). 
## Therefore, there is enough evidence to reject the null hypothesis (Ho). We can consider that
## our alternative hypothesis could be true and yard signs in precincts could affect vote share.

## (c)
## The intercept or constant value on vote share (outcome) is expected to be 0.302 when law signs (predictor) is zero. 
## The intercept value is important in this regression analysis because it helps us to understand if there is an 
## effect on the outcome variable when the predictor variable is zero. 

## (d)
## The R square coefficient (0.094) tells us that the distance of the data from the mean all squared is closer to one.
## Therefore, the R square in the regression model suggests a good fit for the data and the points fall closer to the
## real slope. 
