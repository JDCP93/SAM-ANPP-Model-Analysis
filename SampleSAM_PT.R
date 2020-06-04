SampleSAM_PT = function(Site,Model="Obs",Nlag=3,SampleLength){

  # Load the time series
  load(paste0(Site,"_DailyData.Rdata"))
  load(paste0(Site,"_MonthlyData.Rdata"))
  load(paste0(Site,"_YearlyData.Rdata"))
  
  # Create the block
  block = timeblocks(1,(Nlag>1)*1,(Nlag>2)*(Nlag-2),0,0)$block

  # Remove NaN data found at beginning or end of data
  Y = eval(as.name(paste0(Site,"_YearlyData")))
  M = eval(as.name(paste0(Site,"_MonthlyData")))
  
  # Remove any years at the end of the data where ANPP is missing
  while(is.na(Y$ANPP[length(Y$ANPP)]) == TRUE){
    Y = Y[-nrow(Y),]
    M = M[-(nrow(Y)*12+1:12),]
  }
  # Remove any years at the start of the data where ANPP is missing
  while(is.na(Y$ANPP[1]) == TRUE){
    Y = Y[-1,]
    M = M[-(1:12),]
  }
  
  # For each possible sample of the specified length
  for (j in 1:(nrow(Y)-SampleLength+1)){
    # First extract the data
    SampleY = Y[j:(j+SampleLength-1),]
    SampleM = M[(((j-1)*12)+1):((j+SampleLength-1)*12),]
    # Extract some identifying info
    StartYear = SampleY$Year[1]
  # If the modelling has already been run, let us know and don't run again
    if (file.exists(paste0(Site,"_PT_",StartYear,"_",SampleLength,"SL_",Model,"_pos_",Nlag,"_",max(block),".Rdata"))){
      print(paste0("Sampled SAM_PT output of length ",
                   SampleLength,
                   " starting from ",
                   StartYear,
                   " already exists for ",
                   Site,
                   " with lag ",
                   Nlag,
                   " and ",
                   max(block),
                   " blocks, using ",
                   Model,
                   " ANPP data"))
    } else {
      

      # Form the precipitation column into a matrix
      PPT = matrix(SampleM$PPT,ncol=12,byrow = TRUE)
      # Add the years as the first column
      PPT = cbind(unique(SampleM$Year),PPT)
      # Name the columns for easy reference
      colnames(PPT) = c("Year",
                        "ppt1",
                        "ppt2",
                        "ppt3",
                        "ppt4",
                        "ppt5",
                        "ppt6",
                        "ppt7",
                        "ppt8",
                        "ppt9",
                        "ppt10",
                        "ppt11",
                        "ppt12")
      
      # Form the Tair column into a matrix
      Tair = matrix(SampleM$Tair,ncol=12,byrow = TRUE)
      # Add the years as the first column
      Tair = cbind(unique(SampleM$Year),Tair)
      # Name the columns for easy reference
      colnames(Tair) = c("Year",
                         "tair1",
                         "tair2",
                         "tair3",
                         "tair4",
                         "tair5",
                         "tair6",
                         "tair7",
                         "tair8",
                         "tair9",
                         "tair10",
                         "tair11",
                         "tair12")
    
      # Check which ANPP data we need
      if (Model == "Obs"){
        ANPPData = SampleY$ANPP
      } else {
        ANPPData = unlist(unname(as.vector(SampleY[colnames(SampleY)==paste0("ANPP_",Model)])))
      }
      
      # Create a ANPP matrix
      ANPP = matrix(SampleY$Year,ncol=1)
      ANPP = cbind(ANPP, ANPPData, 1:nrow(ANPP))
      colnames(ANPP) = c("Year",
                         "ANPP",
                         "YearID")
    
    
      # We can now call the SAM function
      SampleSAM_PT(Site,ANPP,PPT,Tair,Nlag,block,prior=FALSE,Model,SampleLength,StartYear)
    
    }
  }
}