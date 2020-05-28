SampleAlphaPlotFunction = function(Site,SampleLength){

  # Load the time series
  load(paste0(Site,"_DailyData.Rdata"))
  load(paste0(Site,"_MonthlyData.Rdata"))
  load(paste0(Site,"_YearlyData.Rdata"))
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
  
  # Create the block
  block = timeblocks(1,(Nlag>1)*1,(Nlag>2)*(Nlag-2),0,0)$block
  
# Initialise dataframe
k = 0
alphas = data.frame("ID" = rep(0,2*(nrow(Y)-SampleLength+1)),
                    "Mean" = rep(0,2*(nrow(Y)-SampleLength+1)),
                    "Min" = rep(0,2*(nrow(Y)-SampleLength+1)),
                    "Max" = rep(0,2*(nrow(Y)-SampleLength+1)),
                    "Significant" = rep(0,2*(nrow(Y)-SampleLength+1)),
                    "Variable" = rep(0,2*(nrow(Y)-SampleLength+1)))


# This is done outside of SAMPlot as all sites will be on one set of axes
for (j in 1:(nrow(Y)-SampleLength+1)){
  # First extract the data
  SampleY = Y[j:(j+SampleLength-1),]
  # Extract some identifying info
  StartYear = SampleY$Year[1]
  # Load the SAM model output for the site
  name = paste0(Site,"_PT_",StartYear,"_",SampleLength,"SL_",Model,"_pos_",Nlag,"_",max(block))
  load(paste0(name,".Rdata"))
  # Extract the data needed
  k = k + 1
  alphas$ID[k] = paste0(StartYear,"to",StartYear+SampleLength-1)
  alphas$Mean[k] = eval(parse(text=name))$alphas$mean[2]/eval(parse(text=name))$alphas$mean[2]
  alphas$Min[k] = eval(parse(text=name))$alphas$min[2]/eval(parse(text=name))$alphas$mean[2]
  alphas$Max[k] = eval(parse(text=name))$alphas$max[2]/eval(parse(text=name))$alphas$mean[2]
  # Check whether significantly different from zero (i.e. min and max have same sign)
  alphas$Significant[k] = 1*(sign(alphas$Min[k])==sign(alphas$Max[k]))
  alphas$Variable[k] = "PPT"
  k = k + 1
  alphas$ID[k] = paste0(StartYear,"to",StartYear+SampleLength-1)
  alphas$Mean[k] = eval(parse(text=name))$alphas$mean[3]/eval(parse(text=name))$alphas$mean[3]
  alphas$Min[k] = eval(parse(text=name))$alphas$min[3]/eval(parse(text=name))$alphas$mean[3]
  alphas$Max[k] = eval(parse(text=name))$alphas$max[3]/eval(parse(text=name))$alphas$mean[3]
  # Check whether significantly different from zero (i.e. min and max have same sign)
  alphas$Significant[k] = 1*(sign(alphas$Min[k])==sign(alphas$Max[k]))
  alphas$Variable[k] = "Tair"
}

alphas$ID = factor(alphas$ID, levels = unique(alphas$ID))

# Plot
alphaPlot = ggplot(data = alphas) +
  geom_hline(yintercept=0, linetype = "dashed",color="grey") +
  geom_pointrange(aes(x=Variable,y=Mean,ymin=Min,ymax=Max,color=as.factor(Significant)),position = position_dodge(width = 0.5)) +
  scale_color_manual(values=c("black","red"),limits=c("0","1")) +
  theme(legend.position="") + 
  labs(title = paste0(Site,
                      " - ",
                      SampleLength,
                      " year sample"),
       y = "Mean Covariate Value") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~ID,
             scales = "free_y")

alphaOutput = list("alphas"=alphas,"alphaPlot"=alphaPlot)
}