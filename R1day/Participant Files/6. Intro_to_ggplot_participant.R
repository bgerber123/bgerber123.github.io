
### Introduction to R Workshop ###
### August 15th, 2024 ###
### Gerber, Horton, Titcomb ###


## Digging in to ggplot2

# > Objectives:
# 1. Understand the grammar and format of ggplot code.
# 2. Explore several different plot types that are appropriate for different data types.
# 3. Expand upon basic plotting functions with more intermediate data visualization tips and tricks for wildlife biologists. 


### There are multiple ways to plot data in R, including using R's base plotting
### functions.

### First, let's practice by bringing a dataset on wildlife sightings.
sightings = read.csv("wildlife_sightings.csv")

head(sightings)


# plot it using base R
plot(count ~ hour, data=sightings)

### While there is a lot of functionality and options using R base plots, a package called
### ggplot2 streamlines nicely with the data manipulation we've learned using dplyr, is
### more readable, and is more intuitive (to some!).

# to get started, load the tidyverse suite of packages:
library(tidyverse)

### ggplot2 works by building up a figure as though it is composed of layers of elements.
### Just like a painting, we need to start with a canvas! The first step of ggplot is initiate
### the plot:

ggplot(sightings)


### oops! This is not very helpful. We obviously need x and y! To plot x and y, we need to 
### nest a function called 'aes' (short for aesthetic) inside our ggplot function.
### Here we will try to visualize animal counts per sighting for each hour of the day.

ggplot(sightings, aes(x = hour, y = count))

### notice that we now have x and y axes that are scaled appropriately for our data,
### but still no data points. We've successfully set up our canvas and now need to add our first
### layer of data. To do this, we use the geom_point() function:

ggplot(sightings, aes(x = hour, y = count))+
  geom_point()
# looks like there is a pattern of larger groups during the middle of the day and smaller groups at night


### Now, say we want to add a smooth line, we can do so using the geom_smooth() function:
ggplot(sightings, aes(x = hour, y = count))+
  geom_point()+
  geom_smooth()


### to force this to be a linear regression line (which admittedly, does not make much sense in this example), we can add an argument:
ggplot(sightings, aes(x = hour, y = count))+
  geom_point()+
  geom_smooth(method = "lm")


### What are some things we should do to make this a 'better' graph?







### Let's tackle the first improvement on the list -- jittering points. This is often very useful when we have count/discrete data. 

ggplot(sightings, aes(x=hour, y=count))+
  geom_jitter(width = 0.2, height=0.2, alpha=0.3)+  # can control the width and height appropriately, as well as controlling transparency
  geom_smooth()


### Now let's edit the axis labels

ggplot(sightings, aes(x=hour, y=count))+
  geom_jitter(width = 0.2, height=0.2, alpha=0.3)+
  geom_smooth()+
  labs(x="Hour of the day", y="Count of individuals \n per sighting") # You can split lines like so


### We can also switch up the theme so that we don't have a gray background:

ggplot(sightings, aes(x=hour, y=count))+
  geom_jitter(width = 0.2, height=0.2, alpha=0.3)+
  geom_smooth()+
  labs(x="Hour of the day", y="Count of individuals \n per sighting")+
  theme_classic(base_size = 14)


### And alter the x-axis:

ggplot(sightings, aes(x=hour, y=count))+
  geom_jitter(width = 0.2, height=0.2, alpha=0.3)+
  geom_smooth()+
  labs(x="Hour of the day", y="Count of individuals \n per sighting")+
  theme_classic(base_size = 14)+
  scale_x_continuous(breaks = c(1, 3, 6, 9, 12, 15, 18, 21, 24))


### We can visualize differences among species in several ways. First, let's color the points by species:

ggplot(sightings, aes(x=hour, y=count))+
  geom_jitter(width = 0.2, height=0.2, alpha=0.3, aes(col = species))+
  geom_smooth()+
  labs(x="Hour of the day", y="Count of individuals \n per sighting")+
  theme_classic(base_size = 14)+
  scale_x_continuous(breaks = c(1, 3, 6, 9, 12, 15, 18, 21, 24))


### If we wanted to plot a smoothed line for each species, how would we do this?

ggplot(sightings, aes(x=hour, y=count))+
  geom_jitter(width = 0.2, height=0.2, alpha=0.3, aes(col = species))+
  geom_smooth(aes(col = species, fill = species))+
  labs(x="Hour of the day", y="Count of individuals \n per sighting")+
  theme_classic(base_size = 14)+
  scale_x_continuous(breaks = c(1, 3, 6, 9, 12, 15, 18, 21, 24))
# !!!!! Yuck!


### We can also visualize patterns on separate panels to make things a little cleaner:

ggplot(sightings, aes(x=hour, y=count))+
  geom_jitter(width = 0.2, height=0.2, alpha=0.3, aes(col = species))+
  geom_smooth(aes(col = species, fill = species), se = F)+
  labs(x="Hour of the day", y="Count of individuals \n per sighting")+
  theme_classic(base_size = 14)+
  scale_x_continuous(breaks = c(1, 3, 6, 9, 12, 15, 18, 21, 24))+
  facet_wrap(~ species, scales = "free_y")+
  guides(col="none", fill="none")
# When we parse by species, number of individuals per sighting seems independent from time of day


### If we wanted to show day vs. night, we can also add a geom_rect:

ggplot(sightings, aes(x=hour, y=count))+
  geom_jitter(width = 0.2, height=0.2, alpha=0.3, aes(col = species))+
  geom_smooth()+
  labs(x="Hour of the day", y="Count of individuals \n per sighting")+
  theme_classic(base_size = 14)+
  scale_x_continuous(breaks = c(1, 3, 6, 9, 12, 15, 18, 21, 24))+
  geom_rect(aes(xmin = 0, xmax = 6, ymin = 0, ymax = max(count) + 1), fill="gray80", alpha = 0.02)


### Can you add a rectangle for hours 18-24? How would you plot the rectangles behind the points?






### Now that we've done a lot of work with a scatterplot, let's explore some other plotting types:

### One way to visualize these data is using a barplot, which can cleanly show counts per species and total counts for each hour.
### We will start with our base ggplot:

ggplot(sightings, aes(x = hour, y=count))+
  geom_col()  # note that geom_col is used when you have counts stored in a column. geom_bar functions a bit like a histogram and will count the rows for you


### How would we show counts for different species?





### How can we make this 'better'?





### Let's first change the color scheme
### You can do this manually, where you pick and type in the colors, or you can use a preset theme. 
### Preset themes are often already optimized for colorblindness, although they often have limit to the number of colors that can be shown at once.

ggplot(sightings, aes(x = hour, y=count))+
  geom_col(aes(fill = species), position = "stack")+
  scale_fill_viridis_d()

# I'm a fan of the viridis family of colors

ggplot(sightings, aes(x = hour, y=count))+
  geom_col(aes(fill = species), position = "stack")+
  scale_fill_manual(values = c("black", "brown4", "brown1", "lightyellow2", "gray80", "gray30"))+
  theme_classic()+
  labs(x = "Hour of the day", y="Count of individuals \n per sighting", fill = "Species")


### Moving the legend
### ggplot legends can be moved by specifying general positions (e.g., bottom), or using plot coordinates:

ggplot(sightings, aes(x = hour, y=count))+
  geom_col(aes(fill = species), position = "stack")+
  scale_fill_manual(values = c("black", "brown4", "brown1", "lightyellow2", "gray80", "gray30"))+
  theme_classic()+
  labs(x = "Hour of the day", y="Count of individuals \n per sighting", fill = "Species")+
  theme(legend.position = "bottom")

# manual (on the plot):
ggplot(sightings, aes(x = hour, y=count))+
  geom_col(aes(fill = species), position = "stack")+
  scale_fill_manual(values = c("black", "brown4", "brown1", "lightyellow2", "gray80", "gray30"))+
  theme_classic()+
  labs(x = "Hour of the day", y="Count of individuals \n per sighting", fill = "Species")+
  theme(legend.position = c(0.8, 0.8)) # "percent of the x and y axes"


# We explored adding rectangles to show nighttime for the scatter plot. We can also visualize these data in a circular manner:

ggplot(sightings, aes(x = hour, y=count))+
  geom_col(aes(fill = species), position = "stack")+
  scale_fill_manual(values = c("black", "brown4", "brown1", "lightyellow2", "gray80", "gray30"))+
  theme_classic()+
  scale_x_continuous(breaks=1:24)+
  labs(x = "Hour of the day", y="Count of individuals \n per sighting", fill = "Species")+
  coord_polar() 


### Now that we've looked at animal counts per hour, we can also explore other aspects of the dataset:

head(sightings)

### Let's now look at average counts per habitat and species using a boxplot
### Take a second to sketch this out (either mentally or on paper)
### How would you start to visualize this using ggplot? What variable is x? What is y? How would you show species/habitat differences?




### How would you make this plot 'better'?




### Let's show the individual points
ggplot(sightings, aes(x= species, y= count))+
  geom_boxplot(aes(fill = habitat, col=habitat), alpha=0.2)+
  geom_point(aes(col=habitat), 
             position = position_jitterdodge(jitter.width = 0.1, jitter.height=0.1, dodge.width = 0.75),
             alpha = 0.2)+
  theme_classic()

### Challenge: change the color scheme using scale_fill_manual() and scale_color_manual()





### We don't necessarily need to rotate our x-axis labels here, but there is often the need to reduce text crowding at the bottom of our plots. 
### The following code will rotate your labels so that the x-axis is not a mess!

ggplot(sightings, aes(x= species, y= count))+
  geom_boxplot(aes(fill = habitat, col=habitat), alpha=0.2)+
  geom_point(aes(col=habitat), 
             position = position_jitterdodge(jitter.width = 0.1, jitter.height=0.1, dodge.width = 0.75),
             alpha = 0.2)+
  theme_classic()+
  scale_color_manual(values = c("darkgreen", "greenyellow", "brown"))+
  scale_fill_manual(values = c("darkgreen", "greenyellow", "brown"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0))  # can make the angle anything you want; hjust and vjust control justification



### Notice that the labels on the x-axis are automatically ordered alphabetically. This often doesn't make much sense and isn't very readable.
### We can reorder using another variable, or we can specify factor levels manually.

### Using counts to put in descending order
ggplot(sightings, aes(x = reorder(species, -count), count))+
  geom_boxplot(aes(fill=habitat))

### Using manual placement (some dplyr here!)

sightings %>%  
  mutate(species = factor(species, levels = c("Owl", "Rabbit", "Deer", "Bear", "Wolf", "Fox"))) %>%
  ggplot(aes(x = species, y=count))+
  geom_boxplot(aes(fill=habitat))

# notice how the dplyr output is piped directly into ggplot



### Often, we will want to include text annotations (e.g., p-values) on our plots
### We can place a p-value annotation like so:

sightings %>%  
  mutate(species = factor(species, levels = c("Owl", "Rabbit", "Deer", "Bear", "Wolf", "Fox"))) %>%
  ggplot(aes(x = species, y=count))+
  geom_boxplot(aes(fill=habitat))+
  annotate("text", x = 2, y = 6, label = "p < 0.01")


### Challenge: Add a 'p = 0.34' above the boxplots for deer






### If there is information that we'd like to plot for each animal, we can do this in one line (rather than writing the annotate line for each species).
### First, make a little summary data:
sumry_sight = data.frame(species = levels(as.factor(sightings$species)),
                         p_val = c(0.1, 0.001, 0.4, 0.5, 0.9, 0.8))

sightings %>%  
  mutate(species = factor(species, levels = c("Owl", "Rabbit", "Deer", "Bear", "Wolf", "Fox"))) %>%
  ggplot(aes(x = species, y=count))+
  geom_boxplot(aes(fill=habitat))+
  geom_text(data = sumry_sight, aes(label = p_val, x = species, y = 8.5))



### There are several companion packages that will automate plot annotations depending on the statistical test that you conduct. 
### Some examples include:
# ggpubr
# statannotations
# ggstatsplot
# ggsignif


### We will explore ridgeplots, which are very useful for visualizing data distributions:

head(sightings)

### We would like to visualize occurrences of each species over time.
### To do this, we need to convert the timestamp column to a 'datetime' data type (rather than a character)
str(sightings$timestamp)

# we will use the lubridate package and the as_datetime function to do this:
sightings$timestamp  = as_datetime(sightings$timestamp)

# see if it worked:
str(sightings$timestamp)

# load library
library(ggridges)

### now we can make the ridgeplot:
ggplot(sightings, aes(x = timestamp, y=species))+
  geom_density_ridges()


### We can add visually add habitat:
ggplot(sightings, aes(x = timestamp, y=species))+
  geom_density_ridges(aes(fill=habitat, col=habitat),
                      alpha=0.2,
                      rel_min_height=0.02)+
  theme_classic()+
  scale_fill_manual(values = c("darkgreen","greenyellow","brown"))+
  scale_color_manual(values = c("darkgreen","greenyellow","brown"))



### Challenge: make a ridgeplot of animal frequency (just as above), but with time of day on the x-axis. Pick your own color scheme!








### The last thing we'll cover is plotting multiple graphs together (i.e., multipanel figures).
### Let's say that we want to combine a column graph of activity over time and activity by habitat:

### Plot 1 (we made this above)

plot1 = ggplot(sightings, aes(x = hour, y=count))+
  geom_col(aes(fill = species), position = "stack")+
  scale_fill_manual(values = c("black", "brown4", "brown1", "lightyellow2", "gray80", "gray30"))+
  theme_classic()+
  labs(x = "Hour of the day", y="Count of individuals \n per sighting", fill = "Species")+
  theme(legend.position = "bottom")

# note that there is no automatic output

### Plot 2
plot2 = ggplot(sightings, aes(x= species, y= count))+
  geom_boxplot(aes(fill = habitat, col=habitat), alpha=0.2)+
  geom_point(aes(col=habitat), 
             position = position_jitterdodge(jitter.width = 0.1, jitter.height=0.1, dodge.width = 0.75),
             alpha = 0.2)+
  theme_classic()+
  scale_color_manual(values = c("darkgreen", "greenyellow", "brown"))+
  scale_fill_manual(values = c("darkgreen", "greenyellow", "brown"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0),
        legend.position = "bottom")



gridExtra::grid.arrange(plot1, plot2, ncol=2)
# check out ?grid.arrange() to explore many different options for combining plots


### Finally, we will save our plot 1 using ggsave

ggsave("activity_by_hour.png", plot = plot1, dpi = 600, width = 5, height = 7, units = "in")


