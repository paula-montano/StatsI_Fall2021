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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# create matrix to conduct chi-square test
trafficViolations <- matrix(c(14, 6, 7, 7, 7, 1), byrow=T, nrow=2)
rownames(trafficViolations) <- c("Upper class", "Lower class")
colnames(trafficViolations) <- c("Not stopped", "Bribe", "Stopped/warned")
# by hand approach
# create function from chi-square test github.io
byHandChiSquare <- function(table){
  # turn into table
  observedValues <- as.table(table)
  # create sums (row, column, and total)
  grandSum <- sum(observedValues)
  sumRow <- rowSums(observedValues)
  sumCol <- colSums(observedValues)
  # calculate expected values for each observation
  # check "?outer" to see that this takes the outer product
  # of the row and col sum divided by the total sum
  expectedValues <- outer(sumRow, sumCol, "*") / grandSum
  v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
  V <- outer(sumRow, sumCol, v, grandSum)
  
  dimnames(expectedValues) <- dimnames(observedValues)
  # create function that calculates each cell residual variance
  # essentially formula on p. 225 in Agresti and Finlay(2009)
  test_statistic <- sum((abs(table - expectedValues))^2 / expectedValues)
  df <- (nrow(observedValues) - 1L) * (ncol(observedValues) - 1L)
  p_value <- pchisq(test_statistic, df, lower.tail = FALSE)
  adjusted_residuals <- (observedValues - expectedValues)/sqrt(expectedValues * (1-sumRow/grandSum) * (1-sumCol/grandSum))
  standardized_residuals <- (observedValues - expectedValues)/sqrt(V)
  # return values
  return(list(statistic = test_statistic,
              df = df,
              p.value = p_value,
              observed = observedValues,
              expected = expectedValues, 
              adj_res = adjusted_residuals,
              std_res = standardized_residuals))  
}
byHandChiSquare(table=trafficViolations)

# run chi square test with built in function
chisq.test(trafficViolations)

# use function to extract standardized residuals
chisq.test(trafficViolations)$stdres

#####################
# Problem 2
#####################

# read in women data from online .csv
women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
# run regression model with water regressed on whether there are reserved seats for women
regression_model_problem2 <- lm(water ~ reserved, data=women)
# get summary of model with coefficient estimates 
summary(regression_model_problem2)

#####################
# Problem 3
#####################

# (a) import the data set
fruitfly <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/fruitfly.csv")
# summarize the statistics in the data set
summary(fruitfly)
# show the distribution of the overall lifespan of the fruitflies
pdf("plot3_a.pdf")
hist(fruitfly$lifespan, main="", xlab="Lifespan")
dev.off()

# (b) produce scatter plot between lifespan and thorax
pdf("plot3_b.pdf")
plot(fruitfly$thorax, fruitfly$lifespan, 
     xlab = "Length of Thorax (mm)", ylab = "Lifespan (days)")
dev.off()

# calculate correlation coefficient between lifespan and thorax
cor(fruitfly$thorax,fruitfly$lifespan)

# (c) # Run the regression of lifespan on thorax
regression_model_problem3 <- lm(lifespan ~ thorax, data=fruitfly)
# get summary statistics for linear regression model
summary(regression_model_problem3)

# (e) calculate the confidence interval by formula
pointEst <- 144.33
se <- 15.77

# get the t-score
t <- qt(0.95,25*5-2)     

# create the upper and lower bounds
lower_CI <- pointEst - t*se
upper_CI <- pointEst + t*se

# now try confint
confint(regression_model_problem3, "thorax", level = 0.9)

# (f) prediction
# store the two variables in x and y
x <- fruitfly$thorax
y <- fruitfly$lifespan

# predict() function to predict an individual fruitfly lifespan
predict(lm(y~x), newdata = data.frame(x=0.8), interval = "prediction", level = 0.90)

# predict() function to predict the average lifespan of fruitflies
predict(lm(y~x), newdata = data.frame(x=0.8), interval = "confidence", level = 0.90)

# (g) create plot of confidence and prediction intervals
# find fitted values and prediction and confidence intervals
# going from .64 to .94 since these are the max and min values of thorax
prep.a <- predict(lm(y~x), newdata = data.frame(x=seq(min(x), max(x), 0.003)),  interval = "confidence")
prep.i <- predict(lm(y~x), newdata = data.frame(x=seq(min(x), max(x), 0.003)),  interval = "prediction")

# open a plot to show the fitted values of lifespan, the prediction and confidence intervals
pdf("plot3_g.pdf")
matplot(seq(min(x), max(x), 0.003), cbind(prep.a, prep.i[,-1]), lty = c(1,2,2,3,3), type = "l", 
        col = c("black", "blue", "green", "red", "purple"), 
        xlab = "Length of Thorax (mm)", ylab = "Fitted Value of Lifespan (days)")

# Add a legend to the plot to show which line represents which value
legend("topleft", legend = c("Fitted Value", "Lower Bound for Confidence Interval", 
       "Upper Bound for Confidence Interval", "Lower Bound for Prediction Interval", 
       "Upper Bound for Prediction Interval"), lty = c(1,2,2,3,3), 
       col = c("black", "blue", "green", "red", "purple"), cex = 0.85)
dev.off()
