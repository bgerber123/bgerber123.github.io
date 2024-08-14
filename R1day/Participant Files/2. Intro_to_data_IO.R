### Introduction to R Workshop ###
### August 15th, 2024 ###
### Gerber, Horton, Titcomb ###

##  Data input and output overview

#install packages
#install.packages("readxl")

#turn on a library 
library(readxl) #this package will allow you to read in an Excel spreadsheet (i.e., a file ending in .xlsx)

#set working directory
setwd() #we need to set our working directory. This will tell R where to look to read in a file 
        #into the R environment

#input Excel
sightings = read_excel("./wildlife_sightings.xlsx") #read in an Excel spreadsheet

#output CSV
write.csv(sightings, "./wildlife_sightings_freshly_written.csv", row.names = F)
rm("sightings")

#input CSV
sightings=read.csv("./wildlife_sightings_freshly_written.csv")

#indexing
sightings$species #the `$` will index the sightings dataframe and return the `species` column
sightings$species[1] #the `[1]` will index to the first row of the `species` column
sightings[1,1] #the `[1,1]` will index the dataframe to the first row and the first column

#checking the class of the data
str(sightings)

class(sightings$species)
class(sightings$count)

#changing the class of a column of data
sightings$timestamp=as.POSIXct(sightings$timestamp, format="%Y-%m-%d %H:%M:%S")
class(sightings$timestamp)
