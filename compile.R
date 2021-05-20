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
  basic.packages <- c("package:stats",
                      "package:graphics",
                      "package:grDevices",
                      "package:utils",
                      "package:datasets",
                      "package:methods",
                      "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, T, F)]
  package.list <- setdiff(package.list,basic.packages)
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

lapply(c("cregg", "texreg", "ggplot2", "ggpubr"), pkgTest)

# set working directory that contains replication
# recommend just putting in Downloads folder
# JZ: 
setwd("~/Dropbox/Trinity/teaching/ASDS/website")
