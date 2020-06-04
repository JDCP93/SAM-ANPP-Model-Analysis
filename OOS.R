# This script is to test a simple PPT model out of sample 
# 
# We will use Konza, as this has the longest time series and has also been shown
# to exhibit significance of antecedent rainfall in previous modelling


#*******************************************************************************
# Initialisation
#*******************************************************************************

# Clear console
cat("\014")
# Delete all variables
rm(list=ls())
# Close plots
graphics.off()

# Source the custom functions
source("SampleSAM_P.R")
source("SampleSAMFunction_P.R")
source("SAMFunction_P.R")

# Source required packages
library(ggplot2)
library(gridExtra)
library(dplyr)

# Set up Konza
Site = "Konza"
load(paste0(Site,"_DailyData.Rdata"))
load(paste0(Site,"_MonthlyData.Rdata"))
load(paste0(Site,"_YearlyData.Rdata"))

#*******************************************************************************
# Initial Test
#*******************************************************************************

# First run the model with minimal weights. Here we have one for a one year lag.

Nlag = 1
block = timeblocks(0,0,0,0,Nlag)$block
