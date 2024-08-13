
### Introduction to R Workshop ###
### August 15th, 2024 ###
### Gerber, Horton, Titcomb ###


## Finding help

# > Objectives:
# 1. Gain familiarity with the different options for finding help with R.
# 2. Practice asking for help (and shamelessly using and adapting existing code!)
# 3. Explore the effectiveness of different types of help for different coding problems.


### As you learn R, you'll notice that it is constantly evolving, with new packages and functionality added on a regular basis. 
### Even if you're a pro in R, you need to be equipped with the tools to learn new functions quickly. 
### 'Being good at R' is often a matter of 'being good at troubleshooting problems in R', and luckily we have more options than ever to learn
### how to read, write, and troubleshoot code.

### 1. The Help Tab

### The help tab is your go-to spot for finding quick answers to questions about functions. For example, let's say that you have a vector of numbers
### that you'd like to take the mean of. You know that you should use the 'mean()' function, but you're really not sure how to format your code to make it
### run properly:

vec = c(10, 2, 25, 1, 2, 5, 609)

### We can use ? to look up a function:
?mean()

### Notice the handy help tab that pops up on the right!
### The format of these entries is a description of a function, followed by an example of the usage (i.e., how is this function written,
### what are the arguments, and what are the defaults?), and then a very important section detailing the arguments to the function.

### Here we can see that we need to supply 'x', which should be a numerical or logical vector (dates are also an option). Other arguments give
### flexibility on usage (and how to deal with NA values).

mean(vec)



### We can also use ?? to look up near matches to a function name:
??mean()

### Notice that there are several options for computing different types of mean, depending on the packages in your system.
### If you run into an error when running code, one of the best things to do is to check the help tab for the function's information.


### Challenge: Using the help tab, identify the problem with the following code and fix it:

### Goal: calculate row sums for a matrix:
matrix_data = matrix(1:9, nrow = 3, ncol = 3)
head(matrix_data)
row_sums = apply(matrix_data, 2, sum)
row_sums






### 2. Internet searching

### There's a lot of great content online of people asking for help with their R code. Oftentimes, R will produce an error message that prints out to 
### the console. Copying and pasting that error message (or the generic parts of the error message) in your search bar will often help to identify errors.

### Example: Error running a linear model

# make dummy data:
df2 = data.frame(x = seq(1:11), y=c(rnorm(10,0,2),"I"))
head(df2)

# fit linear model:
mod1 = lm(y ~ x, data=df2)

# based on the error message (and maybe some googling), can you figure out what's going on?



### 3. ChatGPT

### ChatGPT is extremely helpful for troubleshooting code, especially for commonly-used functions.
### Let's try pasting our code above (including the lines where we make dummy data) to see if ChatGPT comes up with a solution.

### At least for me, ChatGPT easily figures out the issue in the code above and explains the problem fully.

### ChatGPT can also be very effective for coming up with code for a given purpose. 
### For example, I might ask it to simulate a dataset on three tree species with different leaf sizes to use for an R workshop.



### Challenge: create a ChatGPT-assisted illustration of your choosing using R

# > This is what ChatGPT came up with for me (and scolded me for trying)
library(ggplot2)

# Create some data to represent the abstract unicorn
unicorn_data <- data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7, 8),
  y = c(3, 6, 7, 6, 7, 9, 10, 8),
  color = c("purple", "purple", "purple", "pink", "pink", "blue", "blue", "white"),
  size = c(5, 5, 5, 4, 4, 6, 6, 10)
)

# Create some data to represent the wings
wing_data <- data.frame(
  x = c(4, 5, 6, 4.5, 5.5),
  y = c(6, 8, 6, 7, 7),
  color = c("lightblue", "lightblue", "lightblue", "white", "white"),
  size = c(4, 4, 4, 5, 5)
)

# Plot the abstract unicorn
ggplot() +
  geom_point(data = unicorn_data, aes(x = x, y = y, color = color, size = size), shape = 21, stroke = 1.5) +
  geom_point(data = wing_data, aes(x = x, y = y, color = color, size = size), shape = 21, stroke = 1.5) +
  scale_color_identity() +
  scale_size_identity() +
  theme_void() +
  coord_fixed(ratio = 1) +
  ggtitle("Abstract Representation of a Flying Unicorn") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))



### As you can see, ChatGPT does really well with some things, and not others. The code it generates can have errors, so you should always know what
### you are doing/asking for when working with code-writing AI tools. However, for the R-savvy user, ChatGPT opens up many doorways for fast ways to learn
### new tools. (E.g., I used ChatGPT to help me to build this Shiny app (https://gtitcomb.shinyapps.io/harvest_models/) in about 15 mins)

### For wildlife biologists, ChatGPT will be most useful for helping to troubleshoot plotting, running basic statistics, and data
### manipulation. However, it is not very effective for troubleshooting more niche tools (e.g., RMark). For specialized software, online fora, help tabs, 
### and (gasp!) reaching out to software developers might be more effective.







