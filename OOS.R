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
source("SampleFunction_P.R")
source("SAMFunction_P.R")

# Source required packages
library(ggplot2)
library(gridExtra)
library(dplyr)

# Set up Konza
Site = "Konza"

#*******************************************************************************
# Initial Test
#*******************************************************************************

# First run the model with minimal weights. Here we have one for a one year lag.

Nlag = 1
block = timeblocks(0,0,0,0,Nlag)$block
SampleLength = 10
Model = "Obs"

# Run model
SampleFunction_P(Site,Model,block,Nlag,SampleLength)

# ---
# Let's first work with the very first start year as our sample.
# ---

# Load the yearly and monthly data
load(paste0(Site,"_YearlyData.Rdata"))
Y = eval(as.name(paste0(Site,"_YearlyData")))
load(paste0(Site,"_MonthlyData.Rdata"))
M = eval(as.name(paste0(Site,"_MonthlyData")))

# Remove any years at the end of the data where ANPP is missing
while(is.na(Y$ANPP[length(Y$ANPP)]) == TRUE){
  Y = Y[-nrow(Y),]
  M = M[-(nrow(Y)*12+1:12),]
}
# Remove any years at the start of the data where ANPP is missing
while(is.na(Y$ANPP[1]) == TRUE){
  Y = Y[-1,]
  M = M[-(1:12),]
}
# Extract start year
StartYear = Y$Year[1]

# Load the model output
load(paste0(Site,"_P_",StartYear,"_",SampleLength,"SL_",Model,"_pos_",Nlag,"_",max(block),".Rdata"))
Data = eval(as.name(paste0(Site,"_P_",StartYear,"_",SampleLength,"SL_",Model,"_pos_",Nlag,"_",max(block))))

# So our sample is StartYear + SampleLength
# Our out of sample years start in StartYear+SampleLength+Nlag
# We need to reconstruct the ANPP from our model values for our OOS years

alpha_1 = Data$alphas$mean[1]
alpha_2 = Data$alphas$mean[2]
weights = Data$monthlyWeights$mean

# Model is alpha_1 + alpha_2 * PPT_ant
# PPT_ant = sum(w_i * PPT_i)
# Note here PPT_ant[1] is the rainfall IN year 1
PPT_ant = rep(0,nrow(Y))
for (i in 1:nrow(Y)){
PPT_ant[i] = sum(rev(weights)*M$PPT[((i-1)*12+1):(((i-1)*12)+12)])
}

# Model the ANPP
ANPP = alpha_1 + alpha_2 * PPT_ant

# Extract IS and OOS modelled and observed data
Model_OOS = ANPP[(SampleLength+Nlag):length(ANPP)]
Obs_OOS = Y$ANPP[(SampleLength+Nlag):length(ANPP)]
Model_IS = ANPP[1:(SampleLength+Nlag-1)]
Obs_IS = Y$ANPP[1:(SampleLength+Nlag-1)]

# Calculate performance metrics
MBE_OOS = sum(Model_OOS-Obs_OOS)/length(Model_OOS)
NME_OOS = sum(abs(Model_OOS-Obs_OOS))/sum(abs(mean(Obs_OOS)-Obs_OOS))
SDD_OOS = abs(1-var(Model_OOS)/var(Obs_OOS))
CC_OOS = cor(Model_OOS,Obs_OOS)

MBE_IS = sum(Model_IS-Obs_IS)/length(Model_IS)
NME_IS = sum(abs(Model_IS-Obs_IS))/sum(abs(mean(Obs_IS)-Obs_IS))
SDD_IS = abs(1-var(Model_IS)/var(Obs_IS))
CC_IS = cor(Model_IS,Obs_IS)

# Combine into dataframe
Metrics = data.frame("MBE"=c(MBE_IS,MBE_OOS),
                     "SDD"=c(SDD_IS,SDD_OOS),
                     "CC"=c(CC_IS,CC_OOS),
                     "NME"=c(NME_IS,NME_OOS),
                     row.names=c("IS","OOS"))

# Save output
name = paste0(Site,"_",StartYear,"_",SampleLength,"SL_",Nlag,"_",max(block),"_Metrics")
assign(name,Metrics)

save(list=c(name),file=paste0(name,".Rdata"))
