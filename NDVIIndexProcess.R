NDVIIndexProcess = function(NDVI){

  # A function that takes a dataframe of 2-weekly NDVI values and calculates the
  # indices mapping each day within the data series to the corresponding NDVI 
  # value. 
  # 
  # NDVI data are supplied as half-monthly. The function finds the years spanned
  # by the supplied NDVI data, calculates the length of each month within this
  # time span and then splits each month in half. The first half of the month is
  # assigned an index corresponding to the first NDVI measurement for the month
  # and the second half to the second measurement. The index corresponds to the
  # row of the NDVI dataframe used as input e.g. If 15-05-2008 has index 143, 
  # then the NDVI at this data is equal to the NDVI value in row 143 of 
  # dataframe NDVI.
  #
  #
  # ############################################################################
  # Function inputs and outputs
  # ############################################################################
  #
  # INPUTS:
  #  - NDVI: A dataframe, the output from NDVIProcess for the site in question
  #  
  #  OUTPUTS:
  #  - NDVI_Index: A dataframe, each row contains a day from the data spanned by
  #                NDVI and an index linking this date to the corresponding row
  #                in NDVI
  
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




  