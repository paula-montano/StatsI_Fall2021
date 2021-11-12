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

incumbents_subset <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/incumbents_subset.csv")
summary(incumbents_subset) 

## Run a regression
## 1
voteshare_diff <- lm(voteshare ~ difflog, data = incumbents_subset)
summary(lm(voteshare ~ difflog, data = incumbents_subset))

## 2
ggplot(incumbents_subset, aes(difflog, voteshare)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

## 3
voteshare_diff_resid <- resid(voteshare_diff)

## Plot residuals 
ggplot(aes(incumbents_subset$difflog, voteshare_diff_resid), data = NULL) +
  geom_point(alpha = 0.4) +
  geom_smooth()

## 4. Prediction equation 
## y= β0 (intercept: 0.579031) + βˆ1 (difflog: 0.041666) * x
## Y = voteshare (outcome variable) 
## x = the specific value on difflog (explanatory variable) 
## β1 = difflog coefficient 

class(voteshare_diff)

####################
# Problem 2
#####################

## 1. 
presvote_diff <- lm(presvote ~ difflog, data = incumbents_subset)
summary(lm(presvote ~ difflog, data = incumbents_subset))

## 2
ggplot(incumbents_subset, aes(difflog, presvote)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

## 3
presvote_diff_resid <- resid(presvote_diff)

## Plot residuals 
ggplot(aes(incumbents_subset$difflog, presvote_diff_resid), data = NULL) +
  geom_point(alpha = 0.4) +
  geom_smooth()

## Prediction equation
## y (presvote) = β0 (intercept: 0.507583) + β1 (difflog coefficient: 0.023837) * x 
## β0 = (intercept: 0.507583) 
## Y = presvote outcome variable, value that we want to predict 
## β1 = difflog  coefficient (0.023837) * x

class(presvote_diff)

####################
# Problem 3
#####################

## 1. 
voteshare_presv <- lm(voteshare ~ presvote, data = incumbents_subset)
summary(lm(voteshare ~ presvote, data = incumbents_subset))

## 2
ggplot(incumbents_subset, aes(presvote, voteshare)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

## 3. Prediction equation
## y (voteshare) = β0 (intercept: 0.441330) + β1 (presvote coefficient: 0.388018) * x
## Y  = voteshare is outcome variable, value that we want to predict 
## β0 = 0.441330intercept 
## x  = the specific value on presvote (explanatory variable) 
## βˆ1  = presvote coefficient (0.388018)

class(voteshare_presv)

####################
# Problem 4
#####################

## 1. 
voteshareresid_presvresid <- lm(voteshare_diff_resid ~ presvote_diff_resid, data = incumbents_subset)
summary(lm(voteshare_diff_resid ~ presvote_diff_resid, data = incumbents_subset))

## 2
ggplot(incumbents_subset, aes(presvote_diff_resid, voteshare_diff_resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

## 3. Prediction equation
## y (voteshare_diff_resid) = β0 (intercept:-0.00000000000000000486) + β1 (presvote_diff_resid coefficient: 0.25687701270009788423) * x
## Y = voteshare_diff_resid (outcome variable), value that we want to predict 
## x = is the specific value on the explanatory variable 
## β1 = presvote_diff_resid coefficient

class(voteshareresid_presvresid)

####################
# Problem 5
#####################

## 1. 
voteshare_difflog_presvote <- lm(voteshare ~ difflog + presvote, data = incumbents_subset)
summary(lm(voteshare ~ difflog + presvote, data = incumbents_subset)) 

## 2. Prediction equation
## μy (voteshare) = β0 (intercept: 0.4486442) + β1 (difflog coefficient:0.035543) * x1 + β2 (presvote coeffient:0.2568770) * x2
## μy = voteshare (outcome variable), value that we want to predict 
## β1 = difflog coefficient:0.035543 * x1 
## β2 = presvote coeffient:0.256877 * x2 

incumbents_subset_p1 <- augment(voteshare_difflog_presvote)

## 3. 
## The output that is identical in  question 4 is the residual standard error coefficient (0.07339 ).  
## In question 4 the variation in voteshare is not explained by the difference in spending between incumbent and challenger, 
## while in question 5 the voteshare variation is explained by the difference between the incumbent and challenge