## OLR example from UCLA website

### Part 0: Setting up the data

# Load packages
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

# Read in data
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
head(dat)

## Vizualize the data

# See number of responses in apply, pared, and public categories 
lapply(dat[, c("apply", "pared", "public")], table)

# Three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ public + apply + pared, data = dat))

# Get summary of GPA data
summary(dat$gpa)

# View standard deviation of GPA data
sd(dat$gpa)

## Plot the data

# Examine the distribution of GPA at every level of apply and broken
  # down by public and pared
ggplot(dat, aes(x = apply, y = gpa)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


### Part 1: Running OLR and analyzing outputs 

# Fit ordered logistic regression model and store results in 'm'
  # command polr comes from MASS package representing proportional 
  # odds logistic regression
  # Hess = TRUE allows us to see information when we use summary()
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)

# View a summary of the model
summary(m)

# Default method gives profiled CIs - profiled from the log-likelihood function
ci <- confint(m)
ci

# CIs assuming normality- uses standard errors and assumes a normal distribution
# Units are in ordered logits (or ordered log odds)
confint.default(m) 

# Exponentiate the log odds to get odds ratios
# Turning coefficients into odds ratios makes interpretation easier 
exp(coef(m))

# Create a table with odds ratios and their CIs
exp(cbind(OR = coef(m), ci))



### Part 2: Testing Proportional Odds Assumption

# Create the function that estimates the values that will be graphed
# The sf function calculates the log odds of being >= each value of target variable
# qlogis transforms probability to logit
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

# Calculate the means of apply for each variable and 
  # and use sf to put them into one table together
s <- with(dat, summary(as.numeric(apply) ~ pared + public + gpa, fun=sf))
s

# transform the original, ordinal, dependent variable into a new, binary variable, 
  # dependent variable which is equal to zero if the original, ordinal 
  # dependent variable (here, apply) is less than some value a, and 1 
  # if the ordinal variable is greater than or equal to a

glm(I(as.numeric(apply) >= 2) ~ pared, family="binomial", data = dat)

glm(I(as.numeric(apply) >= 3) ~ pared, family="binomial", data = dat)

# These values should match with the 's' table
# We can use these summary tables to test for consistency with the proportional odds assumption

## Testing the pared variable

  # When pared is equal to "no" (0), the difference between the predicted value 
  # for apply >=2 and apply >=3 is:

  -0.378--2.440=2.062 # intercept of >=2 model minus intercept of >=3 model

  # For pared equal to "yes" (1), the difference in predicted values for apply >=2
  # and apply >= 3 is:
  0.765--1.347=2.112

  # Both are close to 2, therefore the parallel slopes assumption is reasonable

## If we are to test the next independent variable "public", here is what we get:

  # When public is set to "no" (0), the difference in coefficients is:
  -0.204--2.345=2.141

  # When public is set to "yes" the difference between coefficients is:
  -0.175--1.547=1.372

  # Differing distance between the sets of coefficients, suggesting that the
  # effect of attending a public vs private school is different for the transition 
  # from unlikely to somewhat likely and somewhat likely to very likely


## Another way to test for consistency with the proportional odds assumption

# Create a table of the values determined in the previous steps
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]

# Print s
s

# Plot s
# If the proportional odds assumption holds, for each predictor variable,
  # distance between the symbols for each set of categories of 
  # the dependent variable, should remain similar
plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))


### Part 3: Using this model to predict probabilities 

# Create a data frame with new data
set.seed(8234)

newdat <- data.frame(
  pared = rep(0:1, 200),
  public = rep(0:1, each = 200),
  gpa = rep(seq(from = 1.9, to = 4, length.out = 100), 4))

# Make predictions from generated data using the model
newdat <- cbind(newdat, predict(m, newdat, type = "probs"))

# Show first few rows
head(newdat)

# Reshape the data
lnewdat <- melt(newdat, id.vars = c("pared", "public", "gpa"),
                variable.name = "Level", value.name="Probability")

# View first few rows
head(lnewdat)

# Plot the predicted probabilities
ggplot(lnewdat, aes(x = gpa, y = Probability, colour = Level)) +
  geom_line() + facet_grid(pared ~ public, labeller="label_both")
