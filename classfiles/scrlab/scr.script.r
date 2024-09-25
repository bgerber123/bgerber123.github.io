#############################
# Case study - Estimating the density/abundance of Javan Rhino's
# 
# Objective: learn to fit a SCR model and use the r package secr
#
############################
# Load packages
  library(secr)
  library(sf)
  
# A great resources for learning about the secr package
# https://www.otago.ac.nz/density/SECRinR.html


########################
# Read in data - make sure to go look at the files as well

  input.CR = read.capthist(captfile="./data/Rhino_SCR_1year.txt",
                           trapfile="./data/Trapfile_monthly2013.txt",
                           detector = "count",
                           fmt="trapID",
                           noccasions=c(10,10),
                           binary.usage = FALSE)


# Read in shapefiles to be used
  boundary.clip1<-st_read("./shapefiles/Boundary_Clip1.shp")
  arenga.palm <- st_read("./shapefiles/PercentArengaPerGrid1km.shp")

########################
# Summarize and plot capture recapture data

 summary(input.CR)

 plot(input.CR[[1]])
 plot(input.CR[[2]])

 plot(boundary.clip1)
 plot(arenga.palm)


#######################
# Create the habitat mask
# Because there are two sessions, two list elements that are the same are created
 mask.adu <- make.mask(traps(input.CR), spacing = 1000, poly.habitat=TRUE,
                       buffer=60000,type = "trapbuffer", poly = boundary.clip1)

# plot of potential activity centers
# We want lots of activity centers to increase accuracce in our approximation
# but the cost is increased computation time
  plot(mask.adu[[1]])
  plot(mask.adu[[2]])



# Add Arenga Palm as a spatial covariate to the mask
# WE will use the covariate to model rhino density by this variable  
  mask.adu = addCovariates(mask.adu,arenga.palm,columns = "Percent")  

# Check for NA/missing values  
 covariates(mask.adu)


# This function can help fix missing values of spatial covariates
  copynearest <- function (mask, covariate) {
    NAcov <- is.na(covariates(mask)[,covariate])
    OK <- subset(mask, !NAcov)
    i <- nearesttrap(mask, OK)
    covariates(mask)[,covariate][NAcov] <- covariates(OK)[i[NAcov],covariate]
    mask
  }

# Fill in the missing values   
  mask.adu[[1]]  <- copynearest(mask.adu[[1]],"Percent")
  mask.adu[[2]]  <- copynearest(mask.adu[[2]],"Percent")

# No more missing values
  covariates(mask.adu[[1]])

  
# Plot the habitat mask with the camera traps and detections by seesion (i.e, females and males)
  plot(mask.adu[[1]])
  plot(traps(input.CR[[1]]), add = TRUE)
  plot(input.CR[[1]], add = TRUE)

  plot(mask.adu[[2]])
  plot(traps(input.CR[[2]]), add = TRUE)
  plot(input.CR[[2]], add = TRUE)
  
  
#Define each session by a covariate, to be used when fitting a model
  session_cov=data.frame(Sex=c("F","M")) 



#######################
# Fit spatial capture recapture models
# If the model is taking a long while to fit on your computer, you can load the files, model1 and model2

# If your model comes back with a warning or optimization error, you can run the code again
# but add in ",start=model1" into the secr.fit function. THis provides starting values for the optimization of where it left off.
model1 = secr.fit(input.adu.annual, 
                  model=list(D~1,g0~1, sigma~1),
                  detectfn=0, 
                  mask=mask.adu, 
                  sessioncov = session_cov,
                  method ="Nelder-Mead")
  

  
# save(model1, file="model1")
# load("model1")

model2 = secr.fit(input.adu.annual, 
                  model=list(D~Percent,g0~Sex, sigma~Sex),
                  detectfn=0, 
                  mask=mask.adu, 
                  sessioncov = session_cov,
                  method ="Nelder-Mead")

# save(model2, file="model2")
# load("model2")

# NOTE: One system of units is used throughout secr. Distances are in meters and areas are in hectares (ha).
# The unit of density is animals per hectare. 
# 1 ha = 10000 m^2 = 0.01 km^2. To convert density to animals / km^2, multiply by 100.


# Look out estimates from each model
  summary(model1)
  summary(model2)


# Compare models using AIC - how do they compare? Look at the AIC weight.
  AIC(secrlist(model1,model2))

# Plot the detection function
  plot(model2,xval=0:5000,lwd=4)

#Get predictions of g0 and sigma
  predict(model2, newdata = data.frame(Sex=c("F","M"),Percent=0.5))

# Get predictions of density by Arenga Palm percentage
  pred.Dsurface <- predictDsurface(model2, mask.adu)
  plot(pred.Dsurface)

  preds.session1 = covariates(pred.Dsurface)[[1]]

  plot(preds.session1$Percent,preds.session1$D.0,col=2,xlab="Predicted Rhino Density",ylab="Percent Cover of Arenga Palm")
  
# Get predictions of Abundance for session 1 (females) and 2 (males)
  N.hat.F = region.N(model2, region = mask.adu, session = 1, se.N = TRUE, alpha = 0.05, loginterval = TRUE)
  N.hat.M = region.N(model2, region = mask.adu, session = 2, se.N = TRUE, alpha = 0.05, loginterval = TRUE)

  N.hat.F
  N.hat.M

# Display contours of the probability density function for the estimated location of one or more range centres,
# i = individual
  plot(boundary.clip1$geometry)
    i=16
  fxi.contour (model2, i = i, sessnum = 1,add=TRUE)


###############################
## Challenge 1


# Consider the effect of the detection function. Using the null model (model1), refit this model
# with a detection function that is not the default ('halfnormal'). Plot the detection function from
# this model and compare it to the default. Second, predict abundance and compare it from the models
# with differing detection functions- is abundance different by a lot or a little or not at all.

  
  
# Challenge 2

# Now consider a new model that incorporates sex effect on density along with the Percent (Arenga Palm) Spatial variable.
# Interpret the model estimates to determine if there is a statistically clear effect of sex on density.
# Next, get the predicted estimates of abundance by sex
# Next, plot predicted density of rhino's by Arenga Palm (Percent variable) for males and females



