ANPPReorder = function(Site){
  
  # This function reorders the data series for a given site
  # Note that re-running this function will give DIFFERENT reorderings
  
  # Load the time series
  load(paste0(Site,"_MonthlyData.Rdata"))
  load(paste0(Site,"_YearlyData.Rdata"))
  
  # Rename to consistent variables
  MonthlyData = eval(as.name(paste0(Site,"_MonthlyData")))  
  YearlyData = eval(as.name(paste0(Site,"_YearlyData")))
  
  # Find the number of years 
  Nyear = nrow(YearlyData)
  # Randomly reorder these 
  Reorder = sample(1:Nyear)
  
  # Initialise a vector
  data = c()
  
  # For the new order of years, reorder the months
  for (i in 1:Nyear){
  j = Reorder[i]
  data = c(data,((j-1)*12)+1:12)
  }
  
  # Reorder the data series
  MonthlyData_Reorder = MonthlyData[data,]
  YearlyData_Reorder = YearlyData[Reorder,]
  
  # Save the reordered data with consistent names
  name = paste0(Site,"_Reorder_MonthlyData")
  assign(name,MonthlyData_Reorder)
  save(list=c(name),file=paste0(name,".Rdata"))
  
  name = paste0(Site,"_Reorder_YearlyData")
  assign(name,YearlyData_Reorder)
  save(list=c(name),file=paste0(name,".Rdata"))
}