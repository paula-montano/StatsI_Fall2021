####################################
# Tutorial 6: Data Manipulation in R
####################################

#### Goals:
#### 1. Learn the basics of data manipulation in R with dplyr
#### 2. Learn how to link together operations using the pipe
#### 3. Create simple workflows through to visualisation

#install.packages("tidyverse")
library(tidyverse)

?tidyverse
browseVignettes(package = "tidyverse")

#################
# Loading in data
#################

# The tidyverse has its own package for reading in data: readr
?readr

# readr uses a very similar format to base r functions. For example, we can
# read in a csv file using the read_csv() function, which is similar to
# base R's read.csv() function.

movies <- read_csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/movies.csv")

################
# Quick Exercise
################

# Change the above code to use readr's read_csv() function. Assign
# the output to a different object. What do you notice is different about the
# two functions?




vignette("tibble")

#######
# dplyr
#######

vignette("dplyr")

# we use the dplyr package to manipulate our data. Manipulation operations can
# generally be broken down into three basic steps:

## (Filtering on) rows
filter(movies, title_type == "Feature Film")

## (Selecting or mutating on) columns
select(movies, thtr_rel_month)
mutate(movies, rel_mon = month.abb[thtr_rel_month])

## Group by and summarise into a single row
by_month <- group_by(movies, thtr_rel_month)
summarise(by_month, n = n())

##########
# The pipe
##########

# Filtering, selecting and summarising aren't very useful on their own. Their
# true power comes from combining them together into a single operation. As
# you know, combining operations in R can be difficult. Until now we've either
# had to create an intermediate object using the assignment operator <- , or 
# nest functions inside other functions. It would be useful if we could write
# operations in order though, one after the other. This is where the pipe comes
# in.

movies %>%
  filter(title_type == "Feature Film") %>% # filter on the rows
  select(thtr_rel_month) %>% # select one column
  mutate(month = month.abb[thtr_rel_month]) %>% # change to month abbreviation
  group_by(month) %>% # group data by month
  summarise(n = n()) %>% # perform a summary operation (count the n per month)
  arrange(desc(n)) # sort in descending order

# Here, we perform 6 operations, one after the other, to manipulate our dataset.

##########
# Exercise
##########

# Using the dplyr functions above with the pipe operator %>% filter the movies
# dataset for "Horror" films in the genre column. Which is the most popular month for
# Horror films to be released?

movies %>%
  filter(genre == "Horror") %>%
  mutate(month = month.abb[thtr_rel_month]) %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

#######################
# Complex operations...
#######################

# As we improve our dplyr skills, we can perform more complex operations. Let's say
# we have a hunch about horror films, i.e. that, compared with other films, they're 
# more likely to be released in October for Halloween. How might we check this hunch?
# First, we'd need to check the underlying pattern of releases for all films across
# the year. Let's recycle our code from above, but convert the n into proportions.

movies %>%
  mutate(month = month.abb[thtr_rel_month]) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>%
  mutate(prop_month = round(n / sum(n), 2)) %>% # mutate after our summarise to find the proportion
  arrange(desc(prop_month))

##########
# Exercise
##########

# Using the code above as a template, perform the same operation on a subset of horror
# films

movies %>%
  filter(genre == "Horror") %>%
  mutate(month = month.abb[thtr_rel_month]) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>%
  mutate(prop_month = round(n / sum(n), 2)) %>% # mutate after our summarise to find the proportion
  arrange(desc(prop_month))

#############
# Visualising
#############

# Now that we know the relative frequency of releases for films, it would be good to
# be able to visualise these. We could perform some of these grouping and summarise
# operations directly within ggplot. However, we'll use dplyr here to show how we 
# can manipulate data and then output the results into visualisations.

movies %>%
  select(genre, thtr_rel_month) %>%
  mutate(horror = genre == "Horror") %>%
  group_by(thtr_rel_month, horror) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = horror, values_from = n) %>%
  ungroup() %>%
  mutate(All = round(`FALSE` / sum(`FALSE`), 2),
         Horror = `TRUE` / sum(`TRUE`, na.rm = TRUE)) %>%
  select(thtr_rel_month, All, Horror) %>%
  pivot_longer(cols = c("All", "Horror"), names_to = "film_type") %>%
  mutate(month = factor(month.abb[thtr_rel_month], levels = month.abb)) %>%
  ggplot(aes(month, value)) +
    geom_col(aes(fill = film_type), position = "dodge") +
    labs(title = "Proportion of Theatrical Releases by Month", y = "proportion") 
    
# The code above is quite complex: not only are we filtering and grouping, we are
# also spreading and gathering our data using the pivot longer and pivot wider 
# functions. These functions change the shape of our dataset, and are necessary here
# because of the way dplyr performs different calculations. For now though, we don't
# need to worry too much about them. What's important to note is the way we have used
# the pipe %>% to chain different operations together, to create a visualisation of
# our initial question. How would you interpret the bar plot?

##########
# Exercise
##########

# Are feature films getting longer? Use the dplyr functions you've learned about 
# today to find out whether the average running time of feature films has increased 
# in recent years.

movies %>%
  filter(title_type == "Feature Film") %>%
  select(runtime, thtr_rel_year) %>%
  group_by(thtr_rel_year) %>%
  summarise(av_runtime = mean(runtime)) %>%
  ggplot(aes(thtr_rel_year, av_runtime)) +
    geom_line() +
    geom_smooth(col = "red", se = FALSE) +
    labs(title = "Average Theatrical Running Time", 
         subtitle = "Feature Films", 
         x = "Release Year", 
         y = "Average Running Time (minutes)")
  