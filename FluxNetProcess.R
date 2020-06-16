FluxNetProcess = function(Site){

  # A function to extract the necessary data from the daily FluxNet csv for a 
  # given site. This is designed to work with the model from Liu et al, 2019. 
  # To work correctly, the FullSet FluxNet csv for the site must be saved in a
  # subfolder named as the siteID. 
  # 
  # The below variables are extracted, quality-checked and return in a nice, 
  # usable fashion:
  # 
  #   NEE_VUT_REF     Net Ecosystem Exchange
  #   SW_IN_F         Incoming Shortwave Radiation
  #   TA_F            Air Temperature
  #   VPD_F           Vapour Pressure Deficit
  #   SWC_F_MDS_1     Top-layer Soil Moisture Content
  #   P_F             Precipitation
  # 
  # 
  # ############################################################################
  # Function inputs and outputs
  # ############################################################################
  #
  # INPUTS:
  #  - Site: A character vector with the FluxNet siteID
  #  
  #  OUTPUTS:
  #  -

  # Look in folder "Site" for the daily FullSet data
  File = list.files(Site, pattern = "*FULLSET_DD*")
  # Read the data into R 
  CSV = (read.csv(paste0(Site,"/",File),header=TRUE))
  # Change timestamps into dates
  CSV$TIMESTAMP = as.Date(as.character(CSV$TIMESTAMP),"%Y%m%d")
  # List the variables we want to extract as well as their quality control
  Variables = c("TIMESTAMP",
                "NEE_VUT_REF","NEE_VUT_REF_QC",
                "SW_IN_F","SW_IN_F_QC",
                "TA_F","TA_F_QC",
                "VPD_F","VPD_F_QC",
                "SWC_F_MDS_1","SWC_F_MDS_1_QC",
                "P_F","P_F_QC")
  # Extract the variables we require
  Data = CSV[Variables]
  
  # Check the quality of the data
  # Since all data is daily, _QC variables are percentage of measured/good 
  # quality gapfill data, ranging from 0-1 
  
  # Identify QC columns
  QCcols = grep("QC",colnames(Data))
  # Remove first row if any data is poor - repeat as necessary
  while(any(Data[1,QCcols]==0)){
    Data = Data[-1,]
  }
  # Remove last row if any data is poor - repeat as necessary
  while(any(Data[nrow(Data),QCcols]==0)){
    Data = Data[-nrow(Data),]
  }
  
  
}