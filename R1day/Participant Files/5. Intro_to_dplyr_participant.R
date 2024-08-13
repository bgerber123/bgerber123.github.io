### Introduction to R Workshop ###
### August 15th, 2024 ###
### Gerber, Horton, Titcomb ###

## dplyr overview

library(tidyverse) #but really we're using just the `dplyr` package

sightings = read.csv("./sample_camera_data.csv")
#sightings = read_csv("./sample_camera_data.csv")
sightings = as_tibble(sightings)

#####################################################################
# dplyr is built around 5 verbs. These verbs make up the majority of the
# data manipulation you tend to do. You might need to:
 
# `select()` certain columns of data.
# 
# `filter() your data to select specific rows.
# 
# `arrange()` the rows of your data into an order.
# 
# `mutate()` your data frame to contain new columns.
# 
# `summarise()` chunks of you data in some way.

#####################################################################
#Selecting columns

#select() lets you subset by columns. This is similar to subset() 
#in base R, but it also allows for some fancy use of helper functions 
#such as contains(), starts_with() and, ends_with(). 

glimpse(sightings) #this reveals the structure of the dataframe or tibble

sightings$season
sightings[, c(2,8)]

select(sightings,  count, species) #this selects just the columns you list

select(sightings,  -count, -species) #this drops a column
select(sightings, contains("b")) #this selects column names that contain a "b"
select(sightings, starts_with("s")) #this selects column names that starts with an "s"

#####################################################################
#Filtering rows

#filter() lets you subset by rows. You can use any valid logical statements:

filter(sightings, count<50)
filter(sightings, count=5)
filter(sightings, species=="Buffalo")

#####################################################################
#Arranging rows

#arrange() lets you order the rows by one or more columns in
#ascending or descending order. 

arrange(sightings, -count) #orders the dataframe from high to low based on count
arrange(sightings, count) #orders the dataframe from low to high based on count

#####################################################################
#Mutating columns
#mutate() lets you add new columns. Notice that the new columns 
#you create can build on each other. 

mutate(sightings, high_count = ifelse())

#####################################################################
#Summarising columns

#Finally, summarise() lets you calculate summary statistics. 
#On its own summarise() isn’t that useful, but when combined with group_by(), filter(), 
#you can summarise by chunks of data. 

summarise(sightings, sample_size = n())

#####################################################################
#Piping 

#Pipes take the output from one function and feed it to 
#the first argument of the next function. 





