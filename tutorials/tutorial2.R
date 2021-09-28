##########################################################
## Tutorial 1: Sampling Distributions, t-distribution, CIs
##########################################################

#### Goals
#### 1. Working with probability distribution functions in R (pnorm and qnorm)
#### 2. Introduce the t-distribution functions
#### 3. Review confidence intervals and calculate them in R

###############
# Distributions
###############

## Go to the help file
help(Normal)


## 1. To generate random numbers from a normal distribution,
##    use rnorm(n=, mean=, sd=)

vec1 <- rnorm(100000, mean=0, sd=1)  # 100000 random numbers with mean=0 and sd=1
vec1

plot(density(vec1),
     main="Distribution of vec1",
     xlab="")

vec2 <- rnorm(5000, mean=50, sd=6)   # 5000 random numbers with mean=50 and sd=6
vec3 <- rnorm(5000, mean=35, sd=10)  # 5000 random numbers with mean=35 and sd=10

plot(density(vec2), 
     main="Distribution of vec2 and vec3",
     xlab="",
     col="red",
     xlim=c(0,100))
lines(density(vec3), lty=2, col="blue")


## 2: dnorm(x=, mean=, sd=) returns the value of the probability density function, 
##    or the height of a density curve, given x.

dnorm(0, mean=0, sd=1)
dnorm(-1, mean=0, sd=1)
dnorm(1, mean=0, sd=1)

# How to plot a normal curve using dnorm()
x.range <- seq(-4, 4, by=0.0001)                       # specify the range of the x-axis
plot(x.range, dnorm(x=x.range, mean=0, sd=1), 
     type="l",                                         # choose type=line
     main="Normal Distribution with mean=0 and sd=1",
     ylab="density",
     lwd=2,
     xaxt="n")
axis(1, at=-3:3, labels=-3:3)



## 3. pnorm(q=, mean=, sd=) returns a probability of drawing q or smaller from 
##    a normal distribution.

pnorm(0, mean=0, sd=1)
pnorm(-1, mean=0, sd=1)
pnorm(1, mean=0, sd=1)
pnorm(-1.96, mean=0, sd=1)

# What's the probability of drawing a value between -1.96 and 1.96?
pnorm(1.96, mean=0, sd=1) - pnorm(-1.96, mean=0, sd=1)



## 4. qnorm(p=, mean=, sd=) returns a value of the *p*th quantile.
##    This is an inverse of pnorm().

qnorm(0.5, mean=0, sd=1)
qnorm(0.15, mean=0, sd=1)
qnorm(0.85, mean=0, sd=1)
qnorm(0.025, mean=0, sd=1)

qnorm(c(0.025, 0.975), mean=0, sd=1)

## Review the help files
?TDist
?Normal


## Recall: t-Distribution is similar to normal distribution, but with fatter tails
## In R, the t-distribution functions are normalized to mean = 0 and sd = 1
## You have to specify degrees of freedom

## Visualize the two different distributions by taking 1 million random draws
## Note: this is different than what you need to do in the homework!
random_normal <- rnorm(1000000)
random_t <- rt(1000000, df = 5)
plot(density(random_normal), col = "blue")
lines(density(random_t), col = "red")
legend("topright", legend = c("Normal", "T"), col = c("blue", "red"), lty = 1)


#### "Distribution" function (cumulative probability)
## Same output because distributions have the same center
pnorm(0, mean = 0, sd = 1)
pt(0, df = 10)

## but t-distribution has more volume in the tails
pnorm(-2, mean = 0, sd = 1)
pt(-2, df = 10)


#### "Quantile" functions, or inverse cumulative probability distribution
## Where is cumulative probability = .025?
## Farther out in the tails of t-distribution 
qnorm(.025, mean = 0, sd = 1)
qt(.025, df = 10)


## "Density" functions
## Very similar density, but t-distribution is smaller in the middle...
dnorm(0, mean = 0, sd = 1)
dt(0, df = 10)

## ... and larger in the tails...
dnorm(-3, mean = 0, sd = 1)
dt(-3, df = 10)

## ... and even larger if we have smaller number of df
dt(-3, df = 5)



#### Using R to calculate CIs

## Load the data: a subset of 2004 American National Election Study
load("anes.Rdata")
View(anes)

## Let's say our confidence coefficient = .95
## Calculate the appropriate confidence interval for the
## mean level of support for how George W. Bush was
## handling the war in Iraq (bushIraq)
z95 <- qnorm((1 - .95)/2, lower.tail = FALSE)## (1-confidence coefficient)/2
n <- length(na.omit(anes$bushiraq))
sample_mean <- mean(anes$bushiraq, na.rm = TRUE)
sample_sd <- sd(anes$bushiraq, na.rm = TRUE)
lower_95 <- sample_mean - (z95 * (sample_sd/sqrt(n)))
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n)))
confint95 <- c(lower_95, upper_95) ## What does this mean?


## Now let's use a confidence coefficient = .99
z99 <- qnorm((1 - .99)/2, lower.tail = FALSE)
lower_99 <- sample_mean - (z99 * (sample_sd/sqrt(n)))
upper_99 <- sample_mean + (z99 * (sample_sd/sqrt(n)))
confint99 <- c(lower_99, upper_99)


## Do our results make sense?
confint95
confint99

### Exercises

################
# Pres. Approval
################

## 1. Read in the Trump Job Approval poll data.
##    Variables are as follows:
##    - Approve = Proportion of the respondents who approve Trump
##    - survey_house = Survey company
##    - end_date = Date the survey ended
##    - sample_subpopulation = Sample type
##    - observations = Number of observations
##    - mode = Survey method



## 2. Plot a histogram of the Trump job approval rates. 



## 3. Suppose you only have the "Gallup" poll from "2/19/2017". If we know that
##    the population variance is 0.25, what is your estimate of the sampling 
##    distribution? 
##    Hint: Find this poll using two conditions (survey_house, end_date)



## 4. According to your answer in Q4, what are the 20th and 75th quantiles of 
##    the distribution?



## 5. Suppose a new poll suggests that the Trump approval rate is 47%. 
##    According to your answer in Q4, what is the probability of a poll showing 
##    support for Trump higher than this?


################
# Military coups
################

## 1. Load data using the following code
## Read the help file
install.packages("faraway")
library(faraway)
data(africa)
?africa


## 2. Create two subsets of the data
## One with only countries were no military coups have occurred
## One where any military coups have occurred






## 3. Find a 95% confidence interval for the mean percent of voting in the last
## elections for each subset of countries







## 4. What do you learn about voter turnout in African countries from
## these confidence intervals?






## 5. Find the 2.5th and 97.5th percentiles of the t distribution
## with 5 degrees of freedom






## 6. Find the probability that x is at least two standard deviations
## above the mean of a t-distribution with 10 degrees of freedom






## 7. Find the density of the t-distribution with 10 degrees of 
## freedom at x = -1.96 and x = 1.96.  Explain the output given what
## we know about the t-distribution and the standard normal distribution

