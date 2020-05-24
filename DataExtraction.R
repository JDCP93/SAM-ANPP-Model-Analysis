DataExtraction = function(Sites){
  
#*******************************************************************************
# Function Description
#*******************************************************************************
# This function takes a list of sites and reads data from the corresponding 
# netcdf and csv files that contain observations and modelling outputs. These 
# are formed into a data frame of daily data for the site which is then saved in
# a relevantly named file
# INPUTS:
# - Sites: a character vector consisting of the names of the sites that will 
#         have their data extracted
# OUTPUTS:
# - .Rdata files named "*Site*_DailyData" that contain a dataframe of the same 
# name

  
# NOTE: MODEL OUTPUTS ARE SUPER WHACK - AS SUCH, THIS FUNCTION IS BESPOKE TO THE
# OUTPUTS THAT WERE AVAILABLE AT THE TIME
  
# Source the necessary packages
library(dplyr)
library(lubridate)
library(ncdf4)

# For each site
for (Site in Sites){
  # Extract the weather data
  # Open the required netcdf files
  METEO = nc_open(paste0("Meteo_Input/",Site,"_Meteo_Data.nc"))
  
  # Extract the variables we need
  Date = ncvar_get(METEO,"Date")
  PPT = ncvar_get(METEO,"Precip")
  Tair = ncvar_get(METEO,"Temp")

  # Meteo date is stored in the MATLAB format
  # To convert to R, we subtract 1970-01-01 in MATLAB format (719529) from the 
  # date, multiply by 86400 to get it in seconds rather than days, and then add
  # to the R date origin of 1970-01-01
  Date = as.POSIXct((Date - 719529)*86400, origin = "1970-01-01")

  # Transform the hourly meteo data into yearly, monthly and daily values
  # We sum precipitation and average temperature
  Met = data.frame("Date"=Date,"PPT" = PPT, "Tair" = Tair)
  
  # Extract day, month and year ready for grouping
  Met = Met %>% mutate(Day = day(Date), Month = month(Date), Year = year(Date))
  
  DailyMet = Met %>% group_by(Year,Month,Day) %>%
                     summarize("PPT" = sum(PPT), "Tair" = mean(Tair))
  
  MonthlyMet = Met %>% group_by(Year, Month) %>%
                       summarize("PPT" = sum(PPT), "Tair" = mean(Tair))
  
  YearlyMet = Met %>% group_by(Year) %>%
                      summarize("PPT" = sum(PPT), "Tair" = mean(Tair))
  
  # Tidy up
  nc_close(METEO)
  
  # Now we extract the model data
  # All the models have different formats and file names
  # Therefore the extraction must happen manually
   
  #*****************************************************************************
  # 1. CABLE
  #*****************************************************************************

  # Open netcdf
  MODEL = nc_open(paste0(Site,"/CABLE/Pr/",Site,"_experiment_Precip_outputs_CABLE_r5448.nc"))
  
  # Extract variables
  Time = ncvar_get(MODEL,"day")
  ANPP = ncvar_get(MODEL,"NPPabove")
  GPP = ncvar_get(MODEL,"GPP")
  LAI = ncvar_get(MODEL,"LAI")
  
  # Find the time origin for the model data
  time_from = substr(ncatt_get(MODEL, "day")$units, 15, 33)
  Time = as.POSIXct(Time, origin=time_from)
  
  # Create dataframe and extract daily values
  CABLE = data.frame("Date" = Time,"ANPP" = ANPP, "GPP" = GPP, "LAI" = LAI)
  CABLE = CABLE %>% mutate(Day = day(Date), Month = month(Date), Year = year(Date))
  
  DailyCABLE = CABLE %>% group_by(Year,Month,Day) %>%
    summarize("ANPP_CABLE" = sum(ANPP), "GPP_CABLE" = sum(GPP), "LAI_CABLE" = mean(LAI))
  
  #Clean up
  nc_close(MODEL)
  
  #*****************************************************************************
  # 2. DLEM
  #*****************************************************************************

  # Open netcdf
  MODEL = nc_open(paste0(Site,"/DLEM/Pr/output_",Site,".nc"))
  
  # Extract variables
  Day = ncvar_get(MODEL,"day")
  Year = ncvar_get(MODEL,"year")
  ANPP = ncvar_get(MODEL,"above_npp")
  GPP = ncvar_get(MODEL,"gpp")
  LAI = ncvar_get(MODEL,"LAI")
  
  # Create full date values
  Time = as.Date(Day-1,origin=paste0(Year,"-01-01"))
  
  # Create dataframe and extract daily values
  DLEM = data.frame("Date" = Time,"ANPP" = ANPP, "GPP" = GPP, "LAI" = LAI)
  DLEM = DLEM %>% mutate(Day = day(Date), Month = month(Date), Year = year(Date))
  
  DailyDLEM = DLEM %>% group_by(Year,Month,Day) %>%
    summarize("ANPP_DLEM" = sum(ANPP), "GPP_DLEM" = sum(GPP), "LAI_DLEM" = mean(LAI))
  
  #Clean up
  nc_close(MODEL)
  
  
  #*****************************************************************************
  # 3. JSBACH and 4. JULES are both in whack formats - I will come back to get
  # their data if it seems worthwhile
  #*****************************************************************************

  #*****************************************************************************
  # 5. LPX
  #*****************************************************************************
  # Open netcdf
  MODEL = nc_open(paste0(Site,"/LPX/Pr/",Site,"_base_daily.nc"))
  
  # Extract variables
  Time = ncvar_get(MODEL,"time")
  ANPP = ncvar_get(MODEL,"ANPP")
  GPP = ncvar_get(MODEL,"GPP")
  LAI = ncvar_get(MODEL,"LAI")
  
  # Find the time origin for the model data
  time_from = substr(ncatt_get(MODEL, "time")$units, 12, 30)
  Time = as.Date(Time, origin=time_from)
  
  # Create dataframe and extract daily values
  LPX = data.frame("Date" = Time,"ANPP" = ANPP, "GPP" = GPP, "LAI" = LAI)
  LPX = LPX %>% mutate(Day = day(Date), Month = month(Date), Year = year(Date))
  
  DailyLPX = LPX %>% group_by(Year,Month,Day) %>%
    summarize("ANPP_LPX" = sum(ANPP), "GPP_LPX" = sum(GPP), "LAI_LPX" = LAI)
  
  #Clean up
  nc_close(MODEL)
  
  
  #*****************************************************************************  
  # 6. ORC_CNP and 7. ORC_MICT have whack time vectors
  #*****************************************************************************  
  
  #*****************************************************************************
  # 8. ORCHIDEE - this is a bit messed up with whacky time origins
  #*****************************************************************************
  # Open netcdf
  # MODEL = nc_open(paste0(Site,"/ORCHIDEE/Pr/resu_",Site,"_Meteo_Data_precip.nc"))
  # 
  # # Extract variables
  # Time = ncvar_get(MODEL,"time_counter")
  # ANPP = ncvar_get(MODEL,"ANPP")
  # GPP = ncvar_get(MODEL,"gpp")
  # 
  # # Find the time origin for the model data
  # # Note that I assume this is wrong, and they meant 2007 NOT 2207...
  # time_from = substr(ncatt_get(MODEL, "time_counter")$units, 11, 30)
  # Time = as.Date(Time-1, origin="2007-01-01")
  # 
  # # Create dataframe and extract daily values
  # ORCHIDEE = data.frame("Date" = Time,"ANPP" = ANPP, "GPP" = GPP)
  # ORCHIDEE = ORCHIDEE %>% mutate(Day = day(Date), Month = month(Date), Year = year(Date))
  # 
  # DailyORCHIDEE = ORCHIDEE %>% group_by(Year,Month,Day) %>%
  #   summarize("ANPP_ORCHIDEE" = sum(ANPP), "GPP_ORCHIDEE" = sum(GPP))
  # 
  # #Clean up
  # nc_close(MODEL)
  
  
  #*****************************************************************************
  # 9. TC
  #*****************************************************************************
  # Open netcdf
  MODEL = nc_open(paste0(Site,"/TC/Pr/R_",Site,"_Pr.nc"))
  
  # Extract variables
  Time = ncvar_get(MODEL,"Date Daily")
  ANPP = ncvar_get(MODEL,"NPPabove")
  GPP = ncvar_get(MODEL,"GPP")
  LAI = ncvar_get(MODEL,"LAI")
  
  # Find the time origin for the model data
  # Note that I assume this is wrong, and they meant 2007 NOT 2207...
  time_from = substr(ncatt_get(MODEL, "Date Daily")$time_origin, 12, 30)
  Time = as.Date(Time, origin="0000-01-01")
  
  # Create dataframe and extract daily values
  TC = data.frame("Date" = Time,"ANPP" = ANPP, "GPP" = GPP, "LAI" = LAI)
  TC = TC %>% mutate(Day = day(Date), Month = month(Date), Year = year(Date))
  
  DailyTC = TC %>% group_by(Year,Month,Day) %>%
    summarize("ANPP_TC" = sum(ANPP), "GPP_TC" = sum(GPP), "LAI_TC" = mean(LAI))
  
  #Clean up
  nc_close(MODEL)

  
  #*****************************************************************************
  # 10. TECO
  #*****************************************************************************
  # Open netcdf
  MODEL = nc_open(paste0(Site,"/TECO/Pr/",Site,"_Pr.nc"))
  
  # Extract variables
  Time = ncvar_get(MODEL,"Time")
  ANPP = ncvar_get(MODEL,"NPPabove")
  GPP = ncvar_get(MODEL,"GPP")
  LAI = ncvar_get(MODEL,"LAI")
  
  # Find the time origin for the model data
  # Note that I assume this is wrong, and they meant 2007 NOT 2207...
  time_from = substr(ncatt_get(MODEL, "Time")$units, 13, 31)
  Time = as.POSIXct(Time*3600, origin=time_from)
  
  # Create dataframe and extract daily values
  TECO = data.frame("Date" = Time,"ANPP" = ANPP, "GPP" = GPP, "LAI" = LAI)
  TECO = TECO %>% mutate(Day = day(Date), Month = month(Date), Year = year(Date))
  
  DailyTECO = TECO %>% group_by(Year,Month,Day) %>%
    summarize("ANPP_TECO" = sum(ANPP), "GPP_TECO" = sum(GPP), "LAI_TECO" = mean(LAI))
  
  #Clean up
  nc_close(MODEL)
  
  #*****************************************************************************
  # Merge dataframes
  #*****************************************************************************
  
  # Merge each dataframe one at a time on the date values
  # Use all.x=TRUE to ensure we keep all dates with MET data available
  DailyData = merge(DailyMet,DailyCABLE,by = c("Year","Month","Day"),all.x=TRUE)
  DailyData = merge(DailyData,DailyDLEM,by = c("Year","Month","Day"),all.x=TRUE)
  DailyData = merge(DailyData,DailyLPX,by = c("Year","Month","Day"),all.x=TRUE)
#  DailyData = merge(DailyData,DailyORCHIDEE,by = c("Year","Month","Day"),all.x=TRUE)
  DailyData = merge(DailyData,DailyTC,by = c("Year","Month","Day"),all.x=TRUE)
  DailyData = merge(DailyData,DailyTECO,by = c("Year","Month","Day"),all.x=TRUE)
  
  # Save the data with a site-specific name
  name = paste0(Site,"_DailyData")  
  assign(name,DailyData)
  save(list=c(name),file=paste0(name,".Rdata"))
}
}
