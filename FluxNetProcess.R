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

File = paste0(Site,"/FLX_",Site,"_FLUXNET2015_FULLSET_DD_2004-2014_1-4.csv")
CSV = (read.csv(File,header=TRUE))

CSV$TIMESTAMP = as.Date(as.character(CSV$TIMESTAMP),"%Y%m%d")

