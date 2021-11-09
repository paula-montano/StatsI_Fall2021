#############################################
# Tutorial 7: Multiple Linear Regression in R
#############################################

#### Goals:
#### 1. Learn the different methods for MLR in R
#### 2. Learn how to organise regression models
#### 3. Create workflows through to visualisation

options(scipen = 999)
library(tidyverse)
#install.packages("broom")
library(broom)
?broom

#################
# Loading in data
#################

# read in the following url as "salary": 
# https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/salary.csv

salary <- read_csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/salary.csv")


#####
# EDA
#####

# Run a quick exploratory data analysis of the salary dataset. How does salary vary
# according to gender? How would we quicky visualise this? What about a test of 
# significance?

# Summary stats
summary(salary)

# Base boxplot
boxplot(Salary_9_mo ~ Gender, data = salary)

# Find means using [] subsetting
mean(salary$Salary_9_mo[salary$Gender == "M"])
mean(salary$Salary_9_mo[salary$Gender == "F"])

# Find means using pipe and dplyr
salary %>%
  group_by(Gender) %>%
  summarise(mean = mean(Salary_9_mo))

# How about a quick check for statistical significance?
t.test(salary$Salary_9_mo ~ salary$Gender, mu = 0)

# How do we interpret our test?


######################
# Running a regression
######################

# Is winning grants associated with salary? How would we find out? Can we visualise
# this relationship?

lm(Salary_9_mo ~ Avg_Cont_Grants, data = salary)

ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

# Let's add gender to this picture. Does the relationship we discovered in the first
# part of class apply to grant winning too? We can visualise this relationship using
# ggplot()

ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_point(alpha = 0.5, aes(colour = Gender)) +
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

salary %>%
  group_by(Rank_Code) %>%
  summarise(mean = mean(Salary_9_mo))

# It seems that Rank_Code is a variable referring to seniority. Let's see how this
# relates to gender

salary %>%
  group_by(Rank_Code, Gender) %>%
  summarise(mean = mean(Salary_9_mo)) %>%
  pivot_wider(names_from = Gender, values_from = mean) %>% # reshape to make a table
  ungroup() %>% # ungroup to be able to add a column
  mutate(diff = M - F) # add a new column with the difference in means

# From a quick manipulation of the data, it looks like the gender effect is present 
# across all grades, but is greatest in the middle grade.

ggplot(salary, aes(factor(-Rank_Code), Salary_9_mo)) +
  geom_boxplot(aes(colour = Gender))

# The boxplot shows this quite nicely: the medians are closely grouped for 3 and 1, 
# but there is a bigger gender gap in rank 2.

# Rather than a linear regression, when our output variable is continuous and our 
# predictors are categorical, we often use ANOVA. The difference in terms of outputs
# however is small, as ANOVA and regression are both part of the general linear model.

# Perhaps we're thinking about this wrong. Maybe men get paid more because they bring
# in more grant money... And maybe people are ranked according to how much money they
# bring in. We can quickly check this by substituting salary for grants

ggplot(salary, aes(factor(-Rank_Code), Avg_Cont_Grants)) +
  geom_boxplot(aes(colour = Gender))

# Oh, fancy that. It seems that the median grant contribution for women in rank 1 is
# actually higher than their male counterparts. On the other hand, there are a lot
# of outliers. Plus, there is a chance that we have a correlation between grant 
# contribution and salary, which interacts with gender and rank. Let's see if we can
# visualise all those things together to make sense of the relationships.

ggplot(salary, aes(-Rank_Code, # we use "-" here because rank is in reverse order
                   Salary_9_mo, 
                   colour = Gender,
                   size = Avg_Cont_Grants)) +
  geom_jitter(alpha = 0.5) #a good use case for jittering!

# After a bit of experimentation, I think this is the most instructive visualisation.
# Firstly, we see how many more men there are in this dataset. Secondly, although the
# data are a bit noisy, we can see that there is an underlying trend within each rank
# of those with bigger grants getting paid more. Women seem to conform to this trend,
# with the possible exception of rank 1, where there seems to be an upper segment of
# male academics whose salaries are way above any woman's, and indeed their male peers.
# Let's run a regression and see the precise relationship between all these variables.

mod3 <- lm(data = salary, Salary_9_mo ~ Rank_Code + Gender + Avg_Cont_Grants)
summary(mod3)

# Well, we can see from our first attempt at modelling these variables that rank is
# a big association, which is also statistically significant. With this variable 
# included, gender loses some statistical significance (though it remains a big 
# association, and we see from the standard error that it is way above zero). The
# role of grants seems now to be less important, though we obviously need to scale
# the coefficient appropriately, i.e. an extra $50,000 grant is associated with an
# extra $200 in salary...

# Let's try a couple more models: one where we interact gender with rank, and one 
# where we drop the grants terms.

mod4 <- lm(data = salary, Salary_9_mo ~ Rank_Code * Gender + Avg_Cont_Grants)
summary(mod4)

# This is an interesting development: we see that the statistical significance 
# doesn't change very much, neither do we find any statistical significance for our
# interaction term. Look at the change in the estimates though: being male is now
# associated with an extra $15677 salary, versus $5634 last time. This is significant
# at roughly the same level as our previous model. How to interpret this? Let's do
# some visualisations of the two models.

# Model 3: No interaction
ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo)) +
  geom_point(aes(colour = factor(Rank_Code))) +
  geom_line(data = augment(mod3), aes(y = .fitted, 
                                      linetype = Gender, 
                                      colour = factor(Rank_Code)))

# Model 4: Interaction
ggplot(salary, aes(Avg_Cont_Grants, Salary_9_mo)) +
  geom_point(aes(colour = factor(Rank_Code))) +
  geom_line(data = augment(mod4), aes(y = .fitted, 
                                      linetype = Gender, 
                                      colour = factor(Rank_Code)))

# What changed? By visualising the two models, we can see that in the first model
# (no interaction) the distance between gender is held constant at each level of
# rank. In the second model, the distance is allowed to vary within the different 
# levels. Think about an average of the three "rank" lines for each gender in each
# plot: in the first plot, the female average would be higher up on the y axis, hence 
# less of a difference between male and female in the regression coefficients).In the 
# second plot, the female average is lower down, and further away from the male,
# hence the bigger association with gender. Which is "better"? Because there's such
# a lot of noise in the plot, all we can really go off is the t score for each model:
# because this is slightly improved for gender on the second model, we might favour 
# that one, whilst noting that the interaction term itself is not significant (which
# is to say, the association of gender *within* rank is very noisy, as we see from
# the visualisation.) Note: both of these models are parallel slopes, because we were
# interacting two categorical variables with each other, not with a continuous 
# variable (as we did in the class exercise). We could try interacting all of these 
# with the grants variable, but we're then creating a lot of interaction terms, and
# this can become hard to interpret. 

# Model 5: dropping grants

mod5 <- lm(mod4 <- lm(data = salary, Salary_9_mo ~ Rank_Code * Gender))
summary(mod5)

# As we see from our summary, dropping grants reduces the statistical significance of
# our other predictors. We might therefore be encouraged to leave it in, even though
# it is only significant at the 0.05 level.