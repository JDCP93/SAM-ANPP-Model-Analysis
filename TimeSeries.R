TimeSeries = function(Site){
  
#*******************************************************************************
# Function Description
#*******************************************************************************
# This function takes a site name and, using a daily time series that has
# been created using DataExtraction.R, creates monthly and yearly time series
# of the same data
# INPUTS:
# - Site: a character vector consisting of the name of the site 
# OUTPUTS:
# - Site_MonthlyData.Rdata: A data file containing a monthly time series of met 
#                           and productivity data for the site
# - Site_YearlyData.Rdata: A data file containing a yearly time series of met 
#                         and productivity data for the site 
  
  # Load the data extracted from DataExtraction function
  load(paste0(Site,"_DailyData.Rdata"))
  
  # Load the observations
  Observations = read.csv("ANPPObservations.csv")
  
  # Summarize this into monthly data
  MonthlyData = eval(as.name(paste0(Site,"_DailyData"))) %>% 
    group_by(Year, Month) %>%
    summarize("PPT" = sum(PPT), 
              "Tair" = mean(Tair), 
              "ANPP_CABLE" = sum(ANPP_CABLE,na.rm=TRUE),
              "GPP_CABLE" = sum(GPP_CABLE,na.rm=TRUE),
              "LAI_CABLE" = mean(LAI_CABLE,na.rm=TRUE),
              "ANPP_DLEM" = sum(ANPP_DLEM,na.rm=TRUE),
              "GPP_DLEM" = sum(GPP_DLEM,na.rm=TRUE),
              "LAI_DLEM" = mean(LAI_DLEM,na.rm=TRUE),
              "ANPP_LPX" = sum(ANPP_LPX,na.rm=TRUE),
              "GPP_LPX" = sum(GPP_LPX,na.rm=TRUE),
              "LAI_LPX" = mean(LAI_LPX,na.rm=TRUE),
              "ANPP_TC" = sum(ANPP_TC,na.rm=TRUE),
              "GPP_TC" = sum(GPP_TC,na.rm=TRUE),
              "LAI_TC" = mean(LAI_TC,na.rm=TRUE),
              "ANPP_TECO" = sum(ANPP_TECO,na.rm=TRUE),
              "GPP_TECO" = sum(GPP_TECO,na.rm=TRUE),
              "LAI_TECO" = mean(LAI_TECO,na.rm=TRUE)
    )
  
  # We want to make sure we only look at years where we have a full dataset (or at
  # least where we have some observations for each month)
  
  # First check which years have data for each month
  MonthCount = (table(MonthlyData$Year)==12)
  # Extract the years for which all 12 months have data
  FullYears = as.numeric(names(MonthCount[MonthCount]))
  # Remove all years that don't fit this criteria from the monthly data set
  MonthlyData = MonthlyData[MonthlyData$Year %in% FullYears,]
  # Assign a consistent name
  name = paste0(Site,"_MonthlyData")  
  assign(name,MonthlyData)
  save(list=c(name),file=paste0(name,".Rdata"))
  
  # Summarize into yearly data
  YearlyData = eval(as.name(paste0(Site,"_DailyData"))) %>% group_by(Year) %>%
    summarize("PPT" = sum(PPT), 
              "Tair" = mean(Tair), 
              "ANPP_CABLE" = sum(ANPP_CABLE,na.rm=TRUE),
              "GPP_CABLE" = sum(GPP_CABLE,na.rm=TRUE),
              "LAI_CABLE" = mean(LAI_CABLE,na.rm=TRUE),
              "ANPP_DLEM" = sum(ANPP_DLEM,na.rm=TRUE),
              "GPP_DLEM" = sum(GPP_DLEM,na.rm=TRUE),
              "LAI_DLEM" = mean(LAI_DLEM,na.rm=TRUE),
              "ANPP_LPX" = sum(ANPP_LPX,na.rm=TRUE),
              "GPP_LPX" = sum(GPP_LPX,na.rm=TRUE),
              "LAI_LPX" = mean(LAI_LPX,na.rm=TRUE),
              "ANPP_TC" = sum(ANPP_TC,na.rm=TRUE),
              "GPP_TC" = sum(GPP_TC,na.rm=TRUE),
              "LAI_TC" = mean(LAI_TC,na.rm=TRUE),
              "ANPP_TECO" = sum(ANPP_TECO,na.rm=TRUE),
              "GPP_TECO" = sum(GPP_TECO,na.rm=TRUE),
              "LAI_TECO" = mean(LAI_TECO,na.rm=TRUE)
    )
  
  # Add the observed data to the yearly model outputs
  YearlyData = merge(YearlyData,
                     Observations[Observations$Site==Site,2:3],
                     by="Year",
                     all.x=TRUE)
  
  # For anality, reorder
  # FOR NOW REMOVE GPP AS WE DON'T HAVE OBSERVATIONS
  YearlyData = YearlyData[,c(1,2,3,19,4,6,7,9,10,12,13,15,16,18)]
  
  # Remove all years that don't have a full year's worth of data
  YearlyData = YearlyData[YearlyData$Year %in% FullYears,]
  
  # Assign a consistent name
  name = paste0(Site,"_YearlyData")  
  assign(name,YearlyData)
  save(list=c(name),file=paste0(name,".Rdata"))
}
