#######################################################
#Author: Brian Gerber
#
#Goal: To understand matrix algebra and notation in linear and generalized linear models
####################################################


#Imagine there are n=10 data points. You want to do linear regression, 
#and predict y-hat from your betas and covariates with the design matrix (X).
#We will ignore the estimation of betas and just pretend that we know what
#they are exactly.

#We have one intercept and two slopes
betas=matrix(c(2,1,5),nrow=3)
betas
#We have one intercept and two continuous covariates
set.seed(34524254)
X=matrix(cbind(rep(1,10),rnorm(10,2,1),rpois(10,10)),nrow=10)


#I want to multiple the betas by the covariates and intercept to predict y-hat
#But, how do I relate X and betas

#what are the dimensions of the matrices
dim(X)
dim(betas)

#Can I multiply them together and what does that even mean?

X*betas
betas*X

#Nope

#I need to do matrix muliplication (called a dot product)
X%*%betas

#Notice the dimensions of the output is the number of samples, n = 10

#But what is the dot product exactly doing?
#Lets take one row at a time of X. Start with the first row
X[1,]

#I want to multiply betas[1] by 1, and betas[2] by 2.91596 and betas[3] by 9
X[1,]*betas

#And then I want to sum them
sum(X[1,]*betas)

#this is the same as the first element of 
(X%*%betas)[1]

#Remember what the betas are- they are the partial effects of a known variable
#or covariate. The betas are slope of a line or magnitude of effect that all
#together sum (linear apporximation) to relate to the data, y. 


#######################################
#Calculate manually the matrix multiplication of row 7 of X and the betas.
#is your answer the same as (X%*%betas)[7]?



####################################
#So what is X%*%betas actually doing? It's multiplying all the elements of betas
#by each row of X and then summing it. Make sure this is clear. Ask questions!

######################################
#Transpose is an important matrix operation
#it take a matrix and turns it on its side.

X
t(X)

#note the dimension change
dim(X)
dim(t(X))

#what about betas?
dim(betas)
dim(t(betas))

#########################################
#If you multiply two matrices together, you should know the dimensions of the ouput matrix
#before the multiplication..

X%*%t(X)

#X by transpose X is 10x3 * 3*10.
#We know we can multiply them because the columns of the first matrix (3)
#is the same number as the rows of the second matrix (3). The matrix that is
#produced has the dimensios of rows of the first matrix (10) and the columns
#of the second matrix (10). Thus, the outcome is a 10x10 matrix

dim(X)
dim(t(X))
dim(X%*%t(X))

#we can not multiple matrices that don't align in terms of the dimensions
X%*%X

#Why is X%*%X non-conformable?

#Can you always multiply a square matrix by itself? Try it.

#######################################################
#The Identity matrix is a special matrix, in which the diagnols are all one,
#and everything else are zeros

diag(10)
diag(3)

#What is the outcome of multiplying betas by an identity matrix
diag(3)%*%(betas)

#nothing changed.

#In GLM's an identity link is the identity matrix- basically, no
#transformation. In contrast to a log-transformation for the Poisson or the logit for the Binomial.

#########################################################
#If I multiply a matrix that is 8x10 times a matrix that is 8x10, what is 
#the dimension of the produced matrix?

#If I multiply a matrix that is 9x3 times a matrix that is 3x1000, what is 
#the dimension of the produced matrix?

##########################################################
#Can we multiply a matrix by a scalar?
scalar=3
diag(3)*scalar

#OR
X*100

#Yes

#Can we divide, add, and substract by a scalar?
diag(3)/scalar

#OR
diag(3)+1000
diag(3)-1000

#Yes

#Can we do these in a different order?
3/diag(3)
1000+diag(3)
1000-diag(3)

#Yes, but the order of operations matters, so the output is different
################################################################
#What about the order of operations for matrix multiplication? Does it matter?

A=matrix(c(10,2,1,5),nrow=2,ncol=2)
B=matrix(c(1,5,2,10),nrow=2,ncol=2)

A
B

A%*%B
B%*%A

#Are the output matrices the same - no- we can't assume A*B is equal to B*A when dealing with matrices,
#which we can assume with regular multiplication - 3*2 = 2*3

############################################################
#A more applied example....
#What if we don't have continiuos variables, but categorical?
#Imagine a categorical variable (habtat) with three levels: grassland, forest, shrub

#We want to relate elk density (y; our data) to habitat with these three levels. We thus need a 
#design matrix (X) that has 3 columns, to translate which row of data correspond to each level

#Lets consider 12 elk densities in which 4 of each correspond to each of the 
#habitat levels

#Here is the variable
habitat=factor(c(rep("Forest",4),rep("Grass",4),rep("Shrub",4)))
habitat

#Let's turn this into a design matrix
X=model.matrix(~habitat)
X

#Notice what it did. The first column is the intercept, the second and third columns
#are called partial intercpts or dummy codes to indicate the different levels.

#For each row, there is a unique combination of 1's and 0's that indicate
#which is forest, grassland, and shrub. The first row is 1 0 0, and we know
#looking at 'habitat' that the first element is 'forest'. The 5th row is 1 1 0,
#which from 'habitat', we know is grassland. Lastly, row 9 is 1 0 1, which we
#can see is Shrub.

#How do we interpret beta1 or the intercept?
#The intercept alone ( 1 0 0 ), indicates Forest because no matter what
#the other beta values are we will always only be adding 1*beta1 + 0*beta2 + 0*beta3,
#which is equal to just 1*beta1. Thus, the intercept and beta1 indicate the
#density of elk in forest. 

#What does the 2nd column indicate? The beta2 corresponds to the difference
#in elk density between forest and grassland, so if grassland has less elk than
#forests beta2 will be negative, and if grassland has more elk than forests
#it will be positive. If there is no difference, we would expect beta2 to be zero.

#Same is true for column three. beta3 is the difference from the intercept (forest).
#because anytime we mutiply the betas by the rows of X, we only ever add the 
#intercept and one of the other habitat types.

#What if you didn't care that much about forest, but wanted to know the 
#effect difference between grassland and shrub. Then, just use a different
#design matrix

#here, we reassign which is the "refernce" category 
habitat2=relevel(habitat, ref="Shrub")
X2=model.matrix(~habitat2)
X2

#Notice how the design matrix has chaged. Now, we have the 1 0 0 be the last 
#part of the data, thus the intercept corresponds to "Shrub", 

#A set of betas and the prediction of y (elk density) using the new design matrix
betas2=c(0.5,1,2)

X2%*%betas2

#By changing the design matrix (X) you can manipulate what betas are estimate and what they mean,
#but not necessarily the predictions of y-hat. Below is an example

#####################################
#This is our data...
y=c(3, 3, 3, 3, 1.5, 1.5, 1.5, 1.5, 1,  1,  1,  1)
  
#Let's relate our data to two diffeent betas and sets of design matrices  
y.hat.1=t(X%*%c(3,-1.5,-2))
y.hat.2=t(X2%*%c(1,2,0.5))


y==y.hat.1
y==y.hat.2
y.hat.1==y.hat.2

#Is it an issue that our betas are not unique solutions, but can change depending on our design matrix?
#No, not at all. We just need to be certain that we understand the design matrix so that we can understnad
#the meaning of out betas. But, irregardless of thier meaning, we still predict the same y-hat's.

#When using design matrix X what is the definition of the intercept (beta1)?
#Indicates the elk desnity in Forests

#When using design matrix X2 what is the definition of the intercept (beta1)?
#Indicates the elk desnity in Shrubs

#When using design matrix X what is the definition of slope 1 (beta2)?
#It is the difference in elk density between forests and .....

#When using design matrix X2 what is the definition of slope 1 (beta2)?
#It is the difference in elk density between shrubs and .....

#When using design matrix X what is the definition of slope 2 (beta3)?

#When using design matrix X2 what is the definition of slope 2 (beta3)?


#####################################
#How does this relate to lm and glm?
#Does using variable habitat or habitat2 change the inference on the coefficients?
#Does the variable habitat or habitat2 change the predictions from the model?

#fit a linear regression model with the variable habitat
fit1=lm(y~habitat)
fit1
fit1$model

#do the predictions match y? 
predict(fit1)
y
#Now use habitat2 instead of habitat. Did the coefficients change? What about the predictions?
fit2=lm(y~habitat2)
fit2
predict(fit2)
y

#Instead of doing y~habitat, lets specify the design matrix we want to use.
X1
X2

#If we fit the y data with the design matrix, do they match with above?
#Meaning do we get the same results by using habitat or by using X?
#Do we alsoget the same results by using habitat2 or by using X2?
lm(y~X+0)
lm(y~X2+0)

############
#Do the same as above but for glm
fit3=glm(y~habitat,family=gaussian)
fit3
predict(fit3)
y

#Same results as y~habitat?
glm(y~X+0,family=gaussian)


fit4=glm(y~habitat2,family=gaussian)
fit4
predict(fit4)
y

#Same results as y~habitat2?
glm(y~X2+0,family=gaussian)


#Does it make more sense how the design matrix works to lm and glm when using categorical variables? 