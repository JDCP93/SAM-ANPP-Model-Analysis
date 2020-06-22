PrecipProcess = function(Data){
   # A function to create the antecedent preciptiation matrix required for 
   # Model_Liu.
   
   
  # Load the required packages
  library(lubridate)
  library(magrittr)
  library(dplyr)
  
  # Extract precip data from the current data file
  Precip = Data[,c("TIMESTAMP","P_F")]
  #Create new columns of day, month and year to allow grouping
  Precip = Precip %>% mutate(Day = day(TIMESTAMP), 
                             Month = month(TIMESTAMP), 
                             Year = year(TIMESTAMP)
                             )

  Precip_Monthly = Precip %>% group_by(Year,Month) %>%
                   summarize("P_F" = mean(P_F))

# Email sent to Liu for clarification - I think we actually just want a moving 
# average window over varying time offsets and periods.

}