#######################
# set working directory
# and load libraries
#######################

# remove all objects
rm(list=ls())
# and detach all libraries
# in case you have something lurking in 
# your global environment
detachAllPackages <- function() {
  # list of basic packages you want
  basic.packages <- c("package:stats",
                      "package:graphics",
                      "package:grDevices",
                      "package:utils",
                      "package:datasets",
                      "package:methods",
                      "package:base")
  # check if there are any "non-basic" packages
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, T, F)]
  package.list <- setdiff(package.list, basic.packages)
  # if there are any packages hidding, remove them
  if (length(package.list)>0){
    for (package in package.list){
      detach(package, character.only=T)
    }
  } 
}
detachAllPackages()

# use function "pkgTest"
# to bulk load and install other libraries 
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
  sink()
}

lapply(c(), pkgTest)

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 2
#####################

library(MASS)
means <- c(350, 300)
covariance <- matrix(c(20,15,15,25), ncol=2)
simulation <- data.frame(mvrnorm(2500, means, covariance))
names(simulation) <- c("math", "music")
