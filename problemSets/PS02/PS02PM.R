---
  title: "PS02PM"
author: "Paula"
date: "10/5/2021"
output: pdf_document
---
  
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

# Load necessary packages

lapply(c("stringr"),  pkgTest)
lapply(c("dplyr"),  pkgTest)
lapply(c("tidyverse"),  pkgTest)
lapply(c("ggplot2"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/StatsI_Fall2021/StatsI_PM/problemSets/Completed problemSets /PS02_PM")


####################
# Problem 1
#####################

##H0: The variables are statistically independent
##Ha: The variables are statistically dependent

##a) Calculate a test-statistic (χ2 statistic)

##f observed = fo = observed frequency = the raw count 
##f expected = fe = what we would expect for independent samples
##= Row total / Grand total*Column total
##If H0 is true, then we would expect f observed = f expected

Answer: X2 =  3.79

##b) Calculate p-value from test-statistic (χ2 statistic)  
##df = (rows-1) (columns-1)
##df = (3-1) (2-1)
##p-value 
pchisq(3.79, df=2, lower.tail=FALSE)

##p-value = 0.1503183
## If p ≤ α we conclude that the evidence supports the alternative hypothesis Ha. 
## If p > α we cannot reject the null hypothesis H0.

##What we conclude from α = .1
## Our p-value is greater than α, therefore we cannot reject our null hypothesis. The variables are not statistically dependent.    

##Complete question 1 was done by hand. 

#####################
# Problem 2
#####################

westBengal <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

#######################
# Analysing the dataset
#######################

str(westBengal)
head(westBengal)
summary(westBengal)

##State a null and alternative (two-tailed) hypothesis

##*(a)
##Ho: When GP was reserved for women leaders the number of new or repaired drinking-water facilities decreased in the village.
##Ha: When GP was reserved for women leaders the number of new or repaired drinking-water facilities increased in the village.

##(b)

#####################
# Problem 3
#####################

fruitfly <-read.csv("http://stat2.org/datasets/FruitFlies.csv")

#######################
# Analysing the dataset
#######################

##Import the data set and obtain summary statistics and examine the distribution of the overall lifespan of the fritflies. 

str(fruitfly)
head(fruitfly)
summary(fruitfly)

lifespan_histogram <- ggplot(fruitfly, aes(x = Longevity)) + 
  geom_histogram(bins = 10, color = "blue", fill = "lightblue") +
  labs(x = "Longevity", y = "Frequency",
       title = "Histogram of lifespan of the fruitflies") +
  theme_minimal()

##(2)
##Plot lifespan vs thorax.

Plot_lifespan_vs_thorax <- plot(fruitfly$Longevity, fruitfly$Thorax,
                                main = "Scatter Plot of lifespan vs thorax",
                                xlab = "Predictor Longevity on X axis",
                                ylab = "Target Thorax on y axis")


lifespan_thorax <- lm(Longevity ~ Thorax, data = fruitfly)
summary(lifespan_thorax)
class(lifespan_thorax)

ggplot(aes(Thorax, Longevity), data = fruitfly) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)
str(lifespan_thorax)

##The variables lifespan and thorax have a positive relation. The plot depicts a positive linear relationship when a value in X increases it also increses in Y. The correlation coefficient shows a strong association between liespan and thorax variable.    

##(3)
Plot_lifespan_thorax <- ggplot(aes(Longevity, Thorax), data = fruitfly) +
  geom_point(alpha = 0.4) +
  labs(x = "Lifespan", y = "Thorax",
       title = "Scatter Plot of lifespan vs thorax")

plot(fruitfly$Longevity, fruitfly$Thorax,
     main = "Scatter Plot of lifespan on thorax",
     xlab = "Predictor Longevity on X axis",
     ylab = "Target Thorax on y axis")
lm(fruitfly$Thorax ~ fruitfly$Longevity) 
abline(lm(fruitfly$Thorax ~ fruitfly$Longevity), col = "red")

##The red slope shows the positive relation between lifespan and thorax in the fruitflies. The slope is steep and shows the strong association between the input and output variables.

##(4)

lm(Longevity~Thorax, data = fruitfly)

##(5)
z90 <-  qnorm((1-.90) / 2, lower.tail = FALSE)
n <- length(na.omit(fruitfly$lifespan_thorax))
fruitfly_mean <- mean(fruitfly$lifespan_thorax, na.rm = TRUE) 
fruitfly_sd <- sd(fruitfly$lifespan_thorax, na.rm = TRUE)
lower_90 <- fruitfly_mean - (z90 * (fruitfly_sd  / sqrt(n))) 
upper_90 <- fruitfly_mean + (z90 * (fruitfly_sd  / sqrt(n))) 
confint90 <- c(lower_90, upper_90)

##Function confint()
confint(lifespan_thorax, parm = 0.90, level = 0.90)



