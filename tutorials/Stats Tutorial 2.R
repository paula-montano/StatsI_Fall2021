###########################################
# Tutorial 2: Hypothesis tests and R basics
###########################################

#### Goals:
#### 1. Revise/learn some R data skills: read in, manipulate, visualise
#### 2. Perform a simple hypothesis test

#################
# Loading in data
#################

#Let's use last week's Trump dataset again. We want to read it in to R, so we'll use 
#the read.csv() function. We'll supply a url as the argument, and create an object.

Trump <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/TrumpApproval.csv")

#We now have an object in our environment window which we can explore. We can do this
#using several different functions. 

#### Exercise
#Try calling the following on Trump.

class() #What class is Trump? Supply the object name as an argument
typeof() #What is Trump a *type of*? Supply the object name as an argument

#Data.frames and lists are containers. To see what other objects are contained in a
#list or data.frame, we can use the following.

objects () 
ls() #These two functions are the same. How many objects are contained inside Trump?

str() #A more elaborate summary of the *structure* of an object can be obtained with
#this function. Supply the object name as an argument
ls.str() #This function combines the ls() and str() functions. Try it. How does it
#differ from str()?

Trump_ls <- ls(Trump) #R is an object oriented language. You can make objects from 
#the output of functions. This can sometimes be useful in solving problems.
class(Trump_ls)
typeof(Trump_ls)
is.vector(Trump_ls) 

###################
# Manipulating Data
###################

#Now that we've loaded in our dataset, if we want to perform any analysis, we will
#need to be able to manipulate it.

Trump_Gallup <- Trump[Trump$survey_house == "Gallup",]
Trump_Gallup <- subset(Trump, survey_house == "Gallup")

#These two commands do exactly the same thing. The first subsets our data using 
#square brackets, the second uses the subset() function, which takes the arguments
#of an object to be subsetted, and the logical expression on which to keep rows.

#### Exercise
#Using square brackets or the subset() function, create subsets of Trump for FOX and
#Pew. You may find the unique() function useful...

Trump_FOX <- subset(Trump, survey_house == "FOX")
Trump_Pew <- subset(Trump, survey_house == "Pew")

###########
# Analysing
###########

#Now that we have our three subsets, it would be good to know something about them.
#Let's try finding and comparing the mean approval. We could do it like this:

mean(Trump_FOX$Approve)
mean(Trump_Pew$Approve)
mean(Trump_Gallup$Approve)

#This is OK, but all we get is a printout of the means. Also, the process was quite
#cumbersome.

aggregate(Trump$Approve, by = list(Trump$survey_house), FUN = mean)

#Here, we use the aggregate() function. It takes as an argument the vector you want
#to apply another function to, here the Trump$Approve vector. We supply the function
#we want to apply to Trump$Approve using the FUN = argument, and we use the by = 
#argument to tell R what we want to group our operation by - here, the survey house. 
#We need to supply a list to the by = argument, so we coerce the survey_house variable
#using the list() function.

Trump_means <- aggregate(Trump$Approve, by = list(Trump$survey_house), FUN = mean)
class(Trump_means) #aggregate returns a data.frame

Trump_means[Trump_means$Group.1 %in% c("Gallup", "Pew", "FOX"),]
subset(Trump_means, Group.1 %in% c("Gallup", "Pew", "FOX"))

#Once we have an object with all the approval means according to survey company, we
#can easily subset that by passing a vector of the companies we want. We use the 
#logical operator %in% to check a list of characters - handy for checking names of
#things.

#### Exercise
#Let's try applying what we've learned using aggregate() to another column in the 
#Trump data.frame. Polling companies use different methods to survey voters. Let's
#see if these different methods result in different means.

#Let's find the right variable
ls.str()

#We can call the unique() function on our variable to see the different methods
unique()

#Now use aggregate() to get the mean of the approval rate as grouped by this
#variable. Remember to assign it to an object.
object1 <- aggregate(data.frame$var, by = list(data.frame$other), FUN = fun)

######################
# Visualising our Data
######################

#We can use R's base plotting functions to see the results of our analysis. Let's 
#try this for the polling companies.

barplot(Trump_means$x)
#What kind of a distribution do we see here?

barplot(Trump_means$x, 
        main = "Trump Approval Ratings by Survey Company", #Our title
        names.arg = Trump_means$Group.1, #The vector with our axis names. Could use Trump_means[,2]
        horiz = TRUE, #Flipping our axes
        las = 1) #Rotating our axis labels 
#We can use additional arguments to barplot() to edit our axis labels, etc. There's
#no really good way of fitting all our labels on here, but it gives you an idea.

#### Exercise
#Try the same with your survey method means object.
barplot(data.frame$x, #change the data.frame name
        main = "Your Title Here", #change the title
        names.arg = data.frame$Group.1, #change the data.frame name
        horiz = TRUE,
        las = 1) 

####################
# Hypothesis Testing
####################

#We'll finish with a hypothesis test. Let's say we think that different survey
#methods produce different approval ratings, in a way which isn't simply due to
#chance (or random) variation in our samples. How would we check this out? Let's
#take the whole process step by step:

#Clear our environment
rm(list = ls())

#1. Read in our dataset
Trump <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/TrumpApproval.csv")

#2. Find our variables
str(Trump)
unique(Trump$mode)
summary(Trump$Approve)

#3. Visualise - Exploratory Data Analysis
#a. Aggregate our data.frame by the relevant variable
Trump_mode <- aggregate(Trump$Approve, by = list(Trump$mode), FUN = mean)
#b. Plot our grouped means
bp <- barplot(Trump_mode[,2], #use square bracket subsetting to select the second col
        names.arg = Trump_mode[,1],
        horiz = TRUE, #Flip our axes
        las = 1, #rotate our text to fit it in,
        cex.names = 0.7, #make our axis text a bit smaller to fit
        main = "Trump Mean Approval Rating by Survey Mode")
text(bp, 0.1, round(Trump_mode[,2], 2)) #adding some text to our barplot with the mean
#Looks like IVR/Online polls have a higher approval....

#4. Formulate our null hypothesis
#NULL HYPOTHESIS: There is no difference in means between IVR/Online polls and 
#Other kinds of polling.

#5. Test our hypothesis
length(Trump$mode[Trump$mode == "IVR/Online"]) #how many samples do we have?

Trump_IVR <- subset(Trump, Trump$mode == "IVR/Online") #make a subset of IVR
Trump_other <- subset(Trump, Trump$mode != "IVR/Online") #make a subset of everything else

Trump_null <- t.test(Trump_IVR$Approve, Trump_other$Approve, mu = 0)
Trump_null
#What does it all mean?!