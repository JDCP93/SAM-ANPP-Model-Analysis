OrderSites = function(Sites){

#*******************************************************************************
# Function Description
#*******************************************************************************
# This function takes a list of sites and, using their yearly data series that
# are already loaded into the workspace, lists them according to various
# characteristics
# INPUTS:
# - Sites: a character vector consisting of the names of the sites that will 
#         have their data extracted
# OUTPUTS:
# - OrderedSites: A list of 4 character vectors, listing sites by mean annual 
#                 temperature, mean annual precipitation, coefficient of
#                 variation of precipitation and length of the time series. 
#                 In all cases, these are listed in ascending order
  
  
  
  #Initiliase
  SiteVar = data.frame("Site" = rep(0,length(Sites)),
                       "MAT" = rep(0,length(Sites)),
                       "MAP" = rep(0,length(Sites)),
                       "CVP" = rep(0,length(Sites)),
                       "LoS" = rep(0,length(Sites)))
  k = 0 
  # Calculate CVP and MAT for each site
  for (i in Sites){
    Data = eval(as.name(paste0(i,"_YearlyData")))
    k = k + 1
    SiteVar$CVP[k] = sd(Data$PPT[!is.na(Data$ANPP)&Data$PPT>0])/mean(Data$PPT[!is.na(Data$ANPP)&Data$PPT>0])
    SiteVar$MAT[k] = mean(Data$Tair[!is.na(Data$ANPP)&Data$PPT>0])
    SiteVar$MAP[k] = mean(Data$PPT[!is.na(Data$ANPP)&Data$PPT>0])
    SiteVar$LoS[k] = length(Data$ANPP[!is.na(Data$ANPP)&Data$PPT>0])
    SiteVar$Site[k] = i
  }
  
  # Create the variables that are ordered by MAT and CVP respectively
  SitesByMAT = arrange(SiteVar,MAT)$Site
  SitesByMAP = arrange(SiteVar,MAP)$Site
  SitesByCVP = arrange(SiteVar,CVP)$Site
  SitesByLoS = arrange(SiteVar,LoS)$Site
  
  OrderedSites = list("ByMAT" = SitesByMAT,
                      "ByMAP" = SitesByMAP,
                      "ByCVP" = SitesByCVP,
                      "ByLoS" = SitesByLoS)
  
}