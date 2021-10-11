###########################################
# Tutorial 4: Correlation & Regression in R
###########################################

#### Goals:
#### 1. Learn how to make scatterplots in R, using base graphics and ggplot
#### 2. Learn how to use lm() and predict() to run a regression analysis

#install.packages("tidyverse")
library(ggplot2)

#################
# Loading in data
#################

#For today's class we will be using two built-in datasets from R and ggplot,
#"anscombe" and "midwest"

#loading built-in datasets
?data()

data("midwest")
data("anscombe")

##################
# Plots of Scatter
##################

#Scatter plots are your most useful tool for visualising the relationship 
#between two continuous variables

plot(anscombe$x1, anscombe$y1,
     main = "Scatter Plot of Two variables",
     xlab = "Predictor Variable on X axis",
     ylab = "Target Variable on y axis")

#We can run a simple linear regression on these two variables using the lm()
#function

lm(anscombe$y1 ~ anscombe$x1) #we use the tilde (~) operator to signify that
#the first variable *depends on* the second, with respect to the regression.

#lm() gives us the intercept and the slope for the regression. An easy way
#to plot this line is using abline() and calling the lm() function within it:

abline(lm(anscombe$y1 ~ anscombe$x1), col = "red") #this adds the line to our
#existing plot

################
# Quick Exercise
################

#Use the plot() function to make a scatter plot of the relationship between x2
#and y2 in the anscombe dataset. Add a regression line to the plot. What do you
#notice?



#############
# Correlation
#############

#When we measure the correlation between two variables, we are trying to put a
#number (between -1 and 1) on their association, i.e. do high values of x 
#correspond with high values of y, do low values correspond with low values?
#The metric we use to measure this is Pearson's correlation coefficient (r), 
#also called the product-moment correlation. We encountered this last week in 
#the cor() function:

cor(anscombe$x1, anscombe$y1)

#with the anscombe data.frame we have a typical coding puzzle: four x columns,
#and four y columns. How do we run a correlation on each paired column without
#endless copy-pasting? Write a "for" loop!

cors <- NULL #create a null object for our correlation results

for(i in 1:4) { #write a for loop to iterate through the x and y columns
  cors[i] <- cor(anscombe[paste0("x", i)], anscombe[paste0("y", i)])
}

cors

#It turns out our data have identical correlation coefficients. Let's plot them
#to see what they all look like (don't worry about the code here)

fmla <- y ~ x
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
  fmla[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(fmla, data = anscombe, xlim = c(3, 19), ylim = c(3, 13))
  abline(lm(fmla, data = anscombe), col = "blue")
}
par(op)

#the Anscombe dataset is actually a famous teaching set in statistics. It shows
#the dangers of relying on statistical output and not visualising your data.

?anscombe

# You may also want to check out this url in your own time: 
# https://www.autodesk.com/research/publications/same-stats-different-graphs 

########
# ggplot
########

#the ggplot package is probably now the most popular way of plotting in R. "Gg"
#stands for "grammar of graphics", and the package uses a specific syntax for
#plotting based upon the work of Leland Wilkinson. A good introduction to ggplot
#is provided in the Wickham/Grolemund "R for Data Science" book, which is 
#available online: https://r4ds.had.co.nz/data-visualisation.html

# A simple ggplot

ggplot(aes(x = x1, y = y1), data = anscombe) +
  geom_point()

#to use ggplot, rather than the plot() function, we call the ggplot() function.
#We supply to it our x and y arguments *inside* the aes() function, and our data
#using the argument data=. We then *add* the type of plot we want as a *geom*
#using the + operator. Here, we want a scatter plot, which is geom_point().

################
# Quick Exercise
################

#Inspect the "midwest" dataset. Using ggplot(), plot a scatterplot of the
#percent college educated as the predictor (x), and percent of people below 
#the poverty line as the target (y)


###############################
# Lots of Data Makes Me Jittery
###############################

#there are a couple of ways that ggplot makes life easier for us. Firstly, the
#syntax is a bit less punishing than base plotting. Secondly, there are a lot
#of advanced graphing tools we can use with minimal fuss. Some of these help
#when we have lots of data in our scatterplot. Try the following code:

ggplot(aes(percbelowpoverty, percollege), data = midwest) +
  geom_point(alpha = 0.2)

#adding the *alpha* argument to a scatter plot creates transparency, so we can
#see the "busy" areas of our plot more easily

ggplot(aes(percbelowpoverty, percollege), data = midwest) +
  geom_point(position = "jitter")

#supplying "jitter" to the position = "argument" adds a small amount of random
#noise to the scatter plot. Here it doesn't make a lot of difference, but it
#can help when your scatter plot has over-plotting issues (many points on the
#same spot)

####################
# Facet-nating Plots
####################

#another big help with ggplot is the ability to facet your plots. Take a look
#at these two plots

ggplot(aes(percbelowpoverty, percollege, colour = state), data = midwest) +
  geom_point(alpha = 0.3)

ggplot(aes(percbelowpoverty, percollege), data = midwest) +
  geom_point(alpha = 0.2) +
  facet_wrap(~state)

#in the first plot, we supply the categorical variable "state" to the colour =
#argument. It gives us a nice plot, but it's quite busy. In the second, we use
#the facet_wrap() geom function to make a separate plot for each category of
#the "state" variable.

################
# Quick Exercise
################

#Make a quick ggplot of "percbelowpoverty" and "percollege", including faceting 
#on the "inmetro" variable. In the call to aes() in the ggplot() function, 
#supply the variable "popadults" to the size = argument


############
# Regression
############

#We were already introduced to the lm() function at the beginning of class. 
#Let's use it now to analyse the relationship between college education and 
#poverty. 

coll_pov <- lm(percbelowpoverty ~ percollege, data = midwest)
summary(coll_pov)

#We know how to plot this in base R:

plot(percbelowpoverty ~ percollege, data = midwest)
abline(lm(percbelowpoverty ~ percollege, data = midwest), col = "blue")

#But how do we plot this using ggplot?

ggplot(aes(percollege, percbelowpoverty), data = midwest) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

#Remember, when you assign the output of lm() to an object, you get an object
#of class...

class(coll_pov)

#An object of class "lm" contains a lot of useful data

str(coll_pov)

#we can plot our residuals!
coll_pov_resid <- resid(coll_pov) #a function for extracting the residuals from
#a model object
plot(midwest$percollege, coll_pov_resid)
abline(h = 0, col = "red")

#with ggplot:
ggplot(aes(midwest$percollege, coll_pov_resid), data = NULL) +
  geom_point(alpha = 0.4) +
  geom_smooth()

############
# Predicting
############

#the lm() function gives us a model as an output, i.e. an intercept and a 
#slope coefficient. We can use these to *predict* the results for other data,
#by suppling our "lm" class object to the predict() function alongside a 
#data.frame of data we want to predict (you need the same variable name in your
#new data as you used in your model). Let's try that:

class(coll_pov)
new_df <- data.frame(percollege = c(runif(30, 0, 50))) #making a random vector 
#with similar characteristics to percollege
predict(coll_pov, newdata = new_df)

#what does predict do? It just fits our new data to the model. As you see
#when we plot it, we just get our regression line.
plot(new_df$percollege, predict(coll_pov, newdata = new_df)) 

?predict()
