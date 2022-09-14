#########################
# Author: Brian D. Gerber
#
# Created: 10/7/2020
#
# Modified 9/14/2022
########################
# Goal: Dale density Analysis - power analysis 
########################
# Objective: organize data
########################
# Notes
# 2 sessions, 1 area, 2 sampling periods
# (multi-session analysis)

#Sessions
# Session 1 = SABZ2018-2019
# Session 2 = SABZ2019-2020

#################################

#Setup workspace
  rm(list=ls())

#Load dependencies  
  library(secr)

#load trapfiles
  trapfiles=c("./data/traps/traps.session1.txt",
            "./data/traps/traps.session2.txt")

#Create Input file
  input=read.capthist(captfile="./data/captures/caps.txt",
                    noccasions=rep(90,2), binary.usage=TRUE,
                    trapfile=trapfiles,detector = "multi",
                    fmt="trapID",covnames="Sex")

  save(input,file="./data/secr.input")
  