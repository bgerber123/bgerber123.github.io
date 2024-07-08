

# y is an 'object' that is assigned the value 3
y = 3
y

# Same operation '=' '<-'
y <- 3

# We can create new objects from objects
y2 = y-2
y2

# We can do math with our objects
# Mind your parentheses (order of operation)
y*2 / y*4

y*2 / (y*4)



sign(-5)
sign(54)

# function - 'c' - concatenate
y = c(1,2,3,4,5,6)

is.numeric(y)

# The function 'class' has the argument 'x'
is.numeric(x = y)

## # How to find out the arguments of a function?
## ?is.numeric

# Functions can be wrapped around each other
# Functions commonly have multiple arguments

x = matrix( 
            data = c(1,2,3,4,5,6),
            nrow = 2,
            ncol = 3
            )
x


y = 3.3
class(y)

y = as.integer(3)
class(y)

y = "habitat"
class(y)

y = factor("habitat")
class(y)

# An ordered collection indexed 1,2,...n
# Using the function 'c' to concetanate
z1 = c(4,5,6)
z1

# 4 is in element/index/position 1 of the vector
# 6 is in element/index/position 3 of the vector

# the dimension of a vector
length(z1)

# A vector of characters
z2 = c("dog","cat","horse")
z2

z3 = c("dog","1","horse")
z3

z3 = c("dog",1,"horse")
z3

z3 = c("dog","1","horse")
z3[1]

z3[2]

z3[2:3]

z3[c(2,3)]

z3[-1]

z4 = factor(
            c("dog", "dog", "cat","horse")
           )

z4


levels(z4)


summary(z4)


x = matrix(
            c(1,2,3,4,5,6),
            nrow = 2, 
            ncol = 3
           )

x

dim(x)

# get element of row 1 and column 2
x[1,2]

# get element of row 2 and column 6
x[2,3]

# get element all elements of row 2
x[2,]

# same as
x[2,1:3]

# ARRAY - more than just two dimensions
z5 = array(
            c("a","b","c","d","e","f"), 
            dim=c(2,2,2)
           )

dim(z5)


z5




# LIST - a bucket - will take anything
my.list = list(z1,z2,z3,z4,z5)

#Subset a list
my.list[[1]]

my.list[[4]]

x = data.frame(outcome = c(1,0,1,1),
               exposure = c("yes", "yes", "no", "no"),
               age = c(24, 55, 39, 18)
               )
x

x$exposure

x['exposure']

x[,2]

