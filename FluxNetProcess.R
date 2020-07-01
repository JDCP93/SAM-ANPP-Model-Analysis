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

  # Load in the FluxNet data we need:
  # Look in folder "Site" for the daily FullSet data
  File = list.files(Site, pattern = "*SUBSET_DD*")
  # Read the data into R 
  CSV = (read.csv(paste0(Site,"/",File),header=TRUE))
  # Change timestamps into dates
  CSV$TIMESTAMP = as.Date(as.character(CSV$TIMESTAMP),"%Y%m%d")
  # List the variables we want to extract as well as their quality control
  Variables = c("TIMESTAMP",
                "NEE_VUT_REF","NEE_VUT_REF_QC",
                "SW_IN_F","SW_IN_F_QC",
                "TA_F","TA_F_QC",
                "VPD_F","VPD_F_QC",
                "SWC_F_MDS_1","SWC_F_MDS_1_QC",
                "P_F","P_F_QC")
  # Extract the variables we require
  Data = CSV[Variables]
  
  # Check the quality of the data:
  # Since all data is daily, _QC variables are percentage of measured/good 
  # quality gapfill data, ranging from 0-1 
  
  # Identify QC columns
  QCcols = grep("QC",colnames(Data))
  # Remove first row if any data is poor - repeat as necessary
  while(any(Data[1,QCcols]==0)){
    Data = Data[-1,]
  }
  # Remove last row if any data is poor - repeat as necessary
  while(any(Data[nrow(Data),QCcols]==0)){
    Data = Data[-nrow(Data),]
  }
  
  # Perform checks on the amount of poor data remaining:
  
  # Arbitarily decide that less than 75% measured/good data for a day is 
  # worrying
  # If any QC columns are < 0.75 for a row, count the row as poor data
  QC = sum(apply(Data[,QCcols],MARGIN=1,function(x) any(x < 0.75)))
  # Calculate percentage of remaining data that is poor
  PercentQC = QC*100/nrow(Data)
  # If more than 5% of the dat is poor, print a warning
  if (PercentQC > 5){
    message("Warning! ",
                 round(PercentQC,digits=3),
                 "% of data is poor!")
  }
  # Check for excessive consecutive streaks of poor data
  # Find the sequences of poor/good data
  Seq = rle(apply(Data[,QCcols],MARGIN=1,function(x) any(x < 0.75)))
  # Find the lengths of these sequences for the poor data
  Lengths = Seq$lengths[Seq$values==TRUE]
  # If a run of 5 or more days of poor data exists, print a warning
  if (max(Lengths)>=5){
    message("Warning! There is a run of ",
                 max(Lengths),
                 " consecutive days with poor data!")
  }
  
  # ############################################################################
  # Create inputs required for modelling
  # ############################################################################
  
  # First, create the fixed parameters:
  
  # Number of short-term climatic predictors
  # Tave, SW, VPD, SWCcurrent and antSWC
  Nv = 5
  # Days into past considered for short-term predictors
  Nlag = 14
  # Time lag for precipitation (number of different time periods)
  NlagP = 8
  # Total number of climate covariates (see paper for info)
  Ns = 22
  # Number of blocks for precipitation
  NblocksP = NlagP
  # Time blocks i.e. how the Nlag days are grouped together
  block = c(1:7, rep(8:9, each = 2), rep(10, 3))
  # The size of the block that each day is included in
  BlockSize = c(rep(1, 7), rep(2, 4),rep(3, 3))
  # Number of blocks
  Nblocks = max(block)
  
  # Calculate other parameters:
  
  # Number of days that antecedent conditions can be calculated for
  Nmem = nrow(Data)-365
  # Indices of days that can be modelled
  Mem_records = 366:nrow(Data)
  
  # Create the climate predictor matrix
  # See Model_Liu inputs for correct order
  # SWC is repeated to account for current and antecedent.
  clim = matrix(c(Data$TA_F,
                  Data$SW_IN_F,
                  Data$VPD_F,
                  Data$SWC_F_MDS_1,
                  Data$SWC_F_MDS_1),
                ncol = Nv)
  
  # Create the NEE vector
  NEE = Data$NEE_VUT_REF
  
  ## NDVI
  
  # Source the NDVI processing function
  source("NDVIProcess.R")
  # Extract raw NDVI
  NDVI = NDVIProcess(Site)
  # Source the NDVI indexing function
  source("NDVIIndexProcess.R")
  # Calculate the NDVI indices
  NDVI_index = NDVIIndexProcess(NDVI)
  # Trim the NDVI indices to the dates from the FluxNet data
  NDVI_index = NDVI_index[NDVI_index$Date %in% Data$TIMESTAMP,]
  # Trim the start of the NDVI data to match these indices
  NDVI = NDVI[-(1:NDVI_index$Index[1]),]
  # Relabel indices to begin at 1
  NDVI_index$Index = NDVI_index$Index-NDVI_index$Index[1]+1
  # Trim the end of the NDVI data to match these indices
  NDVI = NDVI[-((NDVI_index$Index[nrow(NDVI_index)]+1):nrow(NDVI)),]
  # Extract just the NDVI values 
  NDVI = NDVI[,3]
  # Extract just the NDVI index values
  NDVI_index = NDVI_index[,2]
  
  ## PPT
  
  # Source the ppt processing function
  source("PPTProcess.R")
  # Extract the ppt matrix
  ppt_multiscale = PPTProcess(Data)
  
  
  # ############################################################################
  # Create output list
  # ############################################################################
  
  output = list("Nv"=Nv,
                "Ns"=Ns,
                "Nlag"=Nlag,
                "Nmem"=Nmem,
                "NlagP"=NlagP,
                "Mem_records"=Mem_records,
                "clim"=clim,
                "ppt_multiscale"=ppt_multiscale,
                "NEE"=NEE,
                "NDVI"=NDVI,
                "NDVI_index"=NDVI_index,
                "NblocksP" = NblocksP,
                "block" = block,
                "BlockSize" = BlockSize,
                "Nblocks" = Nblocks)
  name = paste0(Site,"_LiuInput")
  assign(name,output)
  save(list=c(name),file=paste0(name,".Rdata"))
  
}