ReorderRunSAM = function(Site,Model="Obs",Nlag=3){
#*******************************************************************************
# Function Description
#*******************************************************************************
# This function will run SAM_PT functions for a given site using REORDERED ANPP 
# observations/outputs from a specified model for a specified lag.
# INPUTS:
#   - Site: A character vector with the name of the site
#   - Model: A character vector with the name of the model. If left blank, then
#           ANPP observations will be used instead.
#   - Nlag: The number of years+1 to include in antecedent conditions
#   
# OUTPUTS:
#   -
  
  # Source the functions
  source("ReorderSAMFunction_PT.R")

  # Load the time series

  load(paste0(Site,"_Reorder_MonthlyData.Rdata"))
  load(paste0(Site,"_Reorder_YearlyData.Rdata"))
  
  
  #Create the input data
  
  # Reference the monthly data for the site
  Data = eval(as.name(paste0(Site,"_Reorder_MonthlyData")))
  # Form the precipitation column into a matrix
  PPT = matrix(Data$PPT,ncol=12,byrow = TRUE)
  # Add the years as the first column
  PPT = cbind(unique(Data$Year),PPT)
  # Name the columns for easy reference
  colnames(PPT) = c("Year",
                    "ppt1",
                    "ppt2",
                    "ppt3",
                    "ppt4",
                    "ppt5",
                    "ppt6",
                    "ppt7",
                    "ppt8",
                    "ppt9",
                    "ppt10",
                    "ppt11",
                    "ppt12")
  # Remove any rows with no observed ANPP
  Data = eval(as.name(paste0(Site,"_Reorder_YearlyData")))
  PPT = PPT[!is.na(Data$ANPP),]
  
  # Form the Tair matrix
  Data = eval(as.name(paste0(Site,"_Reorder_MonthlyData")))
  # Form the Tair column into a matrix
  Tair = matrix(Data$Tair,ncol=12,byrow = TRUE)
  # Add the years as the first column
  Tair = cbind(unique(Data$Year),Tair)
  # Name the columns for easy reference
  colnames(Tair) = c("Year",
                     "tair1",
                     "tair2",
                     "tair3",
                     "tair4",
                     "tair5",
                     "tair6",
                     "tair7",
                     "tair8",
                     "tair9",
                     "tair10",
                     "tair11",
                     "tair12")
  # Remove any rows with no observed ANPP
  Data = eval(as.name(paste0(Site,"_Reorder_YearlyData")))
  Tair = Tair[!is.na(Data$ANPP),]
  
  # Create a ANPP matrix
  # Check where we are getting the ANPP data from 
  if (Model == "Obs"){
    ANPPData = Data$ANPP
  } else {
    ANPPData = unlist(unname(as.vector(Data[colnames(Data)==paste0("ANPP_",Model)])))
  }
  #Form into a matrix and give proper names
  ANPP = matrix(Data$Year,ncol=1)
  ANPP = cbind(ANPP, ANPPData, 1:nrow(ANPP))
  colnames(ANPP) = c("Year",
                     "ANPP",
                     "YearID")
  # Remove any rows with no observed ANPP
  ANPP = ANPP[!is.na(Data$ANPP),]
  
  # Create the standardised time block
  block = timeblocks(1,(Nlag>1)*1,(Nlag>2)*(Nlag-2),0,0)$block
  
  # Check if SAM has already been run, and run if not
    if (file.exists(paste0(Site,"_PT_Reorder_",Model,"_pos_",Nlag,"_",max(block),".Rdata"))){
    print(paste0("SAM_PT output already exists for ",Site," with lag ",Nlag," and ",max(block)," blocks, using reordered ",Model," ANPP data"))
  } else {
    ReorderSAM_PT(Site,ANPP,PPT,Tair,Nlag,block,prior=FALSE,Model)
  }

}