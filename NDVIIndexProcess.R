NDVIIndexProcess = function(NDVI){

  
  # Load the lubridate package for date manipulation
  library(lubridate)

  # Calculate start and end date for the NDVI data we've extracted
  StartDate = ymd(paste(NDVI$NDVIYear[1],1,1))
  EndDate = ymd(paste(NDVI$NDVIYear[nrow(NDVI)],12,31))
  # Calculate the month lengths for each month within the time series
  MonthLengths = unname(days_in_month(seq(StartDate,EndDate,by="month")))
  # Calculate the number of days in the first and second halves of each month
  FirstHalfLengths = floor(MonthLengths/2)
  SecondHalfLengths = ceiling(MonthLengths/2)
  # Turn these lengths in a vector expressing the number of each index we need
  Repetitions = c(rbind(FirstHalfLengths,SecondHalfLengths))
  # For each possible NDVI value, create a sequence mapping the value to the
  # dates for which it is valid
  NDVIIndices = c()
  for (i in 1:nrow(NDVI)){
    indices = rep(i,each=Repetitions[i])
    NDVIIndices = c(NDVIIndices,indices)
  }
  # Place into a dateframe with the date alongside
  NDVI_Index = data.frame("Date" = seq(StartDate,EndDate,by="day"),
                          "Index" = NDVIIndices)



}


  # Trim the NDVI indices to the dates from the FluxNet data
  NDVI_Index = NDVI_Index[NDVI_Index$Date %in% Data$TIMESTAMP,]
  # Trim NDVI data to match these indices
  NDVI = NDVI[-(1:NDVI_Index$Index[1]),]
  # Relabel indices to begin at 1
  NDVI_Index$Index = NDVI_Index$Index-NDVI_Index$Index[1]+1
  
  
  # CURRENT ISSUE - NDVI data extends past FluxNet...

  