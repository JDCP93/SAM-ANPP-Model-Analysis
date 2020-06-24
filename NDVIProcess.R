NDVIProcess = function(Site){
  
  # A function to create the NDVI vector required to run Model_Liu. The function
  # takes the raw NDVI data downloaded from the internet as a csv, and extracts
  # a timeseries of NDVI for the location of the given site.
  # 
  # The function assumes the existence of a csv containing site info for the 
  # FluxNet sites so that it can obtain the required latitude and longitude values
  # for the site, as well as the required NDVI files. These can be downloaded 
  # directly via R by use of the package 'gimms' if they are not available. The
  # NDVI files must be saved in a subfolder named 'NDVI'.
  # 
  # The FluxNet file is from https://fluxnet.fluxdata.org/data/download-data/, 
  # obtained by selecting to download the BADM file.
  #
  # ############################################################################
  # Function inputs and outputs
  # ############################################################################
  #
  # INPUTS:
  #  - Site: A character vector with the FluxNet siteID
  #  
  #  OUTPUTS:
  #  - NDVI: A dataframe consisting of the numeric NDVI values for the site as 
  #          well as year and monthspan


  # First find site longitude and latitude from the site info csv
  
  # Load site info csv
  AllInfo = read.csv("FLX_AA-Flx_BIF_DD_20200501.csv")
  # Extract latitude and longitude
  Sitelat = AllInfo$DATAVALUE[AllInfo$SITE_ID == Site & AllInfo$VARIABLE == "LOCATION_LAT"]
  Sitelon = AllInfo$DATAVALUE[AllInfo$SITE_ID == Site & AllInfo$VARIABLE == "LOCATION_LONG"]
  # Remove any levels from the values
  Sitelat = as.numeric(as.character(Sitelat))
  Sitelon = as.numeric(as.character(Sitelon))
  # Delete site info csv
  rm("AllInfo")
  # Create NDVI vector
  
  # Source required package
  library(ncdf4)
  # Initiliase
  NDVI = data.frame()
  # For each available NDVI file
  for (i in list.files("NDVI")){
    # Open the netcdf
    cdf = nc_open(paste0("NDVI/",i))
    # Extract the NDVI, lat and lon data
    rawNDVI = ncvar_get(cdf,"ndvi")
    NDVIlat = ncvar_get(cdf,"lat")
    NDVIlon = ncvar_get(cdf,"lon")
    # Close the netcdf connection
    nc_close(cdf)
    rm("cdf")
    # Calculate the location of the site
    latIndex = which.min(abs(NDVIlat - Sitelat))
    lonIndex = which.min(abs(NDVIlon + Sitelon))
    # Calculate the NDVI for the site - note this is scaled by 10000
    SiteNDVI = rawNDVI[lonIndex,latIndex,]/10000
    # Calculate dates for these values
    NDVIYear = substring(i,15,18)
    NDVIMonth = substring(i,20,23)
    # Form data frame of new data
    NewData = data.frame(NDVIYear,NDVIMonth,SiteNDVI)
    # Add to NDVI dataframe
    NDVI = rbind(NDVI,NewData)
  }
  return(NDVI)
}