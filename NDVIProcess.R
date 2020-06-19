NDVIProcess = function(Site){



  # First find site longitude and latitude from the site info csv
  
  # Load site info csv
  AllInfo = read.csv("FLX_AA-Flx_BIF_DD_20200501.csv")
  # Extract latitude and longitude
  Sitelat = filter(AllInfo, SITE_ID == Site & VARIABLE == "LOCATION_LAT")$DATAVALUE
  Sitelon = filter(AllInfo, SITE_ID == Site & VARIABLE == "LOCATION_LONG")$DATAVALUE
  # Remove any levels from the values
  Sitelat = as.numeric(as.character(Sitelat))
  Sitelon = as.numeric(as.character(Sitelon))

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
}