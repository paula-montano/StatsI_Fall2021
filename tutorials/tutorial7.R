#############################################
# Tutorial 7: Multiple Linear Regression in R
#############################################

#### Goals:
#### 1. Learn the different methods for MLR in R
#### 2. Learn how to organise regression models
#### 3. Create workflows through to visualisation

options(scipen = 999) # to change the default output of numbers
library(tidyverse)
#install.packages("broom")
library(broom)
?broom

#################
# Loading in data
#################

# read in the following url as "salary": 
# https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/salary.csv

#####
# EDA
#####

# Run a quick exploratory data analysis of the salary dataset. How does salary vary
# according to gender? How would we quicky visualise this? What about a test of 
# significance?

# Summary stats


# Base boxplot


# Find means using [] subsetting
mean(salary$Salary_9_mo[])
mean(salary$Salary_9_mo[])

# Find means using pipe and dplyr
salary %>%
  function1(Gender) %>%
  function2(mean = mean(Salary_9_mo))

# How about a quick check for statistical significance?


# How do we interpret our test?


######################
# Running a regression
######################

# Is winning grants associated with salary? How would we find out? Can we visualise
# this relationship?

lm()

ggplot(data, aes(x, y)) +
  geom_1(alpha = 0.5) + #add a scatterplot
  geom_2(method = "?") #add a linear regression line

# Let's add gender to this picture. Does the relationship we discovered in the first
# part of class apply to grant winning too? We can visualise this relationship using
# ggplot()

ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_1(alpha = 0.5, aes(colour = Gender)) +
  geom_smooth(method = "lm", aes(colour = Gender))

# How do we interpret this graph?

############################
# Broom and different models
############################

# Thinking about our regression formula, there are two ways of specifying gender in 
# our model. We could think of it in terms of a change to our intercept (beta zero),
# or as a change to both our intercept and our slope (beta one). By default, ggplot
# gives us the second of these, which is why our lines have different slopes. What
# if we wanted to model gender just as a change to our intercept? (This is called 
# parallel slopes). We need a bit of help from the tidyverse.

## Broom

# The broom package helps us create tidy regression models by *augmenting* our
# datasets with predictions and statistics from our regression model.

mod1 <- lm(Salary_9_mo ~ Avg_Cont_Grants + Gender, data = salary)

# Augment() works like predict(), but creates a data.frame (or tibble) rather than
# a vector

salary_pl <- augment(mod1)

str(salary_pl)

# We can now visualise the difference between the *interaction* model and the 
# parallel slopes model.

ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_point(alpha = 0.5, aes(colour = Gender)) +
  geom_line(data = salary_pl, aes(y = .fitted, colour = Gender)) # we change our data to the fitted values of the salary_pl model

# We can also use augment on to create a data.frame (or tibble) of the *interaction*
# model which ggplot gave us by default

mod2 <- lm(Salary_9_mo ~ Avg_Cont_Grants + Gender + Avg_Cont_Grants:Gender, 
           data = salary)

# First, notice the difference in notation: we have to add the interaction between
# grants and gender as a separate term in the equation, separated with a colon. 
# Another way of writing this:

lm(Salary_9_mo ~ Avg_Cont_Grants * Gender, data = salary) # gives the same output

# We can now augment this second model

salary_int <- augment(mod2)

# And visualise the same way as previously

ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_point(alpha = 0.5, aes(colour = Gender)) +
  geom_line(data = salary_int, aes(y = .fitted, colour = Gender))

# Note: we don't have the error ribbon now, but we could add this using the optional
# interval = "confidence" argument in augment, and the geom_ribbon() function with 
# the .lower and .upper columns supplied to the ymin and ymax arguments-

salary_pl <- augment(mod1, interval = "confidence")

ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_point(alpha = 0.5, aes(colour = Gender)) +
  geom_line(data = salary_pl, aes(y = .fitted, colour = Gender)) +
  geom_ribbon(data = salary_pl, 
              aes(ymin=.lower, ymax=.upper), alpha=0.2)

salary_int <- augment(mod2, interval = "confidence")

ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_point(alpha = 0.5, aes(colour = Gender)) +
  geom_line(data = salary_int, aes(y = .fitted, colour = Gender)) +
  geom_ribbon(data = salary_int, 
              aes(ymin=.lower, ymax=.upper), alpha=0.2)

# Note: our error bars for gender = female are actually a bit wider this way than 
# using ggplot. This is likely due to a difference in weighting for extreme data.

ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_point(alpha = 0.5, aes(colour = Gender)) +
  geom_smooth(method = "lm", aes(colour = Gender))

########################
# Which model is better?
########################

summary(mod1)
summary(mod2)

# What do you think? Compare the outputs above, and pay close attention to the 
# significance of the different coefficients in each model.

##########
# Exercise
##########

# Try the same approach using Rank_Code as a term in your regression analysis. What
# do you find? How does the regression model change when we substitute grant and 
# gender into this model?