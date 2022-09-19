#########################
# Author: Brian D. Gerber
#
# Created: 10/7/2020
#
# Modified 9/14/2022
########################
# Goal: Dale density Analysis - power analysis 
########################
# Objective: plot model results
########################
#################################

#Setup workspace
  rm(list=ls())

#Load dependencies  
  library(plotly)
  
# Load model results

load("./analysis/model.output")

# I plot stuff and save figures

png(file="./plotting/histograms/plot.1.hist.png")
hist(a)
dev.off()
