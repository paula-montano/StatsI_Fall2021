######################################################
# Tutorial 3: Chi-squared tests and contingency tables 
######################################################

#### Goals:
#### 1. Learn how to make contingency tables in R
#### 2. Chi-squared tests in R

#################
# Loading in data
#################

# The "movies" dataset is a comma-separated values dataset available at the following url:
# https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/movies.csv
# Check the url, and then import the dataset using the appropriate R function. Don't forget
# to assign it to an object.


#######################
# Analysing the dataset
#######################

# What structure does our dataset have, and what variables? Using functions you already know:
# Find the structure of the dataset. Print off the first six rows of each variable. Print out
# a summary of the dataset.


#######################
# Wrangling the dataset
#######################

# A lot of our variables are of class "character". For some this is appropriate. For others
# not. We're interested in variables relating to the *genre* of movie, the *studio*, and the
# *critics rating*. Find the unique values for these variables using the appropriate function



# Two of these variables would be a good candidate for casting to *factor* class. How would
# we do this? Search for the relevant function using help, and then perform the operation.
# After, check that the operation worked by calling class() on the variables, and levels().



#####################
# Subsetting our data
#####################

# Now that we've wrangled our dataset, lets see what the distribution of the two variables
# we're interested in looks like. We can use the table() function to quickly see this. Try
# it, supplying the two variables of interest as arguments.



# There are quite a lot of different movie genres, and many are relatively sparse categories.
# Let's focus on just five categories: "Action & Adventure", "Comedy", "Documentary", "Drama", 
# and "Mystery & Suspense". Use the subset() function to subset the dataset appropriately.



# Casting variables from one kind to another can have unintended consequences. What happened
# here? Try calling levels() on the genre variable again.


######################
# Exploratory Analysis
######################

# How might we visualise the relationship between two categorical variables? 

movie_matrix <- as.matrix(table(movies$genre, movies$critics_rating), nrows = 5)

barplot(height = movie_matrix, 
        beside = TRUE, 
        legend.text = TRUE,
        args.legend = list(x = "topleft", cex = 0.4, box.col = "white"))

# Is there a better way? Look at the help file for prop.table()

##########################
# Testing for Significance
##########################

# Look at the help file for the chisq.test() function. How does it work? Call 
# it on the contingency table we used for the bar plot above.