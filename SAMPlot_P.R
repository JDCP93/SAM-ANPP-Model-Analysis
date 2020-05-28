SAMPlot_P = function(Site,Nlag){
  
#*******************************************************************************
# Function Description
#*******************************************************************************

# A function that takes the output of the SAMFunction function and plots a 
# selection of graphs for the site
#
# Inputs:
# - Site: A 6 character site ID for the site for which we want to plot the results
# - Nlag: Integer, the number of years of antecedent rainfall that were 
#         considered in the modelling
# 
# Outputs:
# - weightsPlot: A ggplot of the monthly weights
# - ANPPPlot: A ggplot of the observed, linearly modelled and SAM ANPP values


  
# Load the SAM model output for the site
name = paste0(Site,"_P_Obs_pos_",Nlag,"_",12+((Nlag>1)*6)+((Nlag>2)*(Nlag-2)*4))
load(paste0(name,".Rdata"))

# Change name to an expression
data_SAM = eval(parse(text=name))

# Load the observations for the site
name = paste0(Site,"_YearlyData")
load(paste0(name,".Rdata"))

# Change name to an expression
data_Obs = eval(parse(text=name))

#*******************************************************************************
# Plot of Monthly Weights
#*******************************************************************************

# Assemble the dataframe for weight plottings
plotWeights = data.frame("Weights"=as.vector((data_SAM$monthlyWeights$mean)),
                         "YearIntoPast"=rep(0:(Nlag-1),each=12),
                         "Month"=rep(c("Dec","Nov","Oct","Sep", "Aug","Jul",
                                       "Jun", "May","Apr","Mar","Feb","Jan"),Nlag))

# Assign factors so that months appear in order
plotWeights$Month = factor(plotWeights$Month, levels = rev(c("Jan","Feb","Mar","Apr",
                                                         "May","Jun","Jul","Aug",
                                                         "Sep","Oct","Nov","Dec")))
# Create plot
weightsPlot <- ggplot(plotWeights,aes(x=YearIntoPast,y=Weights,fill=Month)) +
               geom_bar(stat="identity", 
                        position=position_dodge(), 
                        linetype = "solid",
                        size=0.5,
                        color="black") +
               theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     legend.position = "none",
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black")) +
               facet_grid(.~YearIntoPast,
                          scales = "free_x",
                          switch = "x", 
                          space = "free_y") +
               labs(title = paste0(Site," - ",length(data_Obs$ANPP[!is.na(data_Obs$ANPP)])," years"),
                    x = "Month/Year into Past")
  
#*******************************************************************************
# Plot of Modelled ANPP vs Observed ANPP
#*******************************************************************************

# Conduct a linear regression between yearly precip and ANPP
PPT = data_Obs$PPT
ANPP = data_Obs$ANPP

Model = lm(ANPP ~ PPT, data = data.frame("PPT"=PPT,"ANPP"=ANPP),na.action=na.exclude)

# Create dataframes for plotting
ANPPobs = data.frame(Year=1:length(ANPP)+as.numeric(data_Obs$Year[1])-1,
                    ANPP_obs = ANPP)


ANPPsam = data.frame(Year=1:length(data_SAM$ANPPmod$mean)+Nlag-2+as.numeric(min(data_Obs$Year[!is.na(data_Obs$ANPP)])),
                    ANPP_SAM = data_SAM$ANPPmod$mean,
                    ANPP_SAMmin = data_SAM$ANPPmod$min,
                    ANPP_SAMmax = data_SAM$ANPPmod$max)

# Linear regression excludes NA values
# Take fitted values
ANPP_lm = Model$fitted.values

# If we omitted NA values
if (length(unname(Model$na.action[]))>0){
  # For each omitted NA value
  for (i in 1:length(unname(Model$na.action[]))){
    # Insert NA back in
    ANPP_lm = append(ANPP_lm,NA,unname(Model$na.action[i])-1)
  }
}

ANPPlm = data.frame(Year=1:length(ANPP)+as.numeric(data_Obs$Year[1])-1,
                   ANPP_lm)

# Plot the graphs

ANPPPlot <- ggplot() +
  geom_ribbon(data=ANPPsam, aes(x=Year, ymin=ANPP_SAMmin, ymax=ANPP_SAMmax), fill="red", alpha=0.4) +
  geom_line(data=ANPPsam,aes(Year,ANPP_SAM,color="SAM"),size = 2) +
  geom_point(data=ANPPlm,aes(x=Year,y=ANPP_lm,color="LRM"),size=2,na.rm=TRUE) +
  geom_line(data=ANPPlm,aes(Year,ANPP_lm,color="LRM"), size = 1,linetype = "solid",na.rm=TRUE) +
  geom_point(data=ANPPobs,aes(x=Year,y=ANPP_obs,color="Obs"),size=3,na.rm=TRUE) +
  geom_line(data=ANPPobs,aes(Year,ANPP_obs,color="Obs"), size = 1,na.rm=TRUE) +
  scale_color_manual(values=c("black", "blue", "red"), breaks = c("Obs","LRM","SAM")) +
  guides(color=guide_legend(title=NULL)) +
  theme_bw() +
  labs(title = Site,
      y = "ANPP",
      subtitle = paste0("SAM MAE = ",
                        signif(data_SAM$MAE,4),
                        ", R2 = ",
                        signif(data_SAM$R2,3),
                        ", DIC = ",
                        signif(data_SAM$DIC,3),
                        "\nLRM MAE = ",
                        signif(mean(abs(Model$residuals[-(1:Nlag-1)])),4),
                        ", R2 = ",
                        signif(summary(Model)$r.squared,3))) +
  scale_x_continuous(breaks = 1:length(ANPP)+as.numeric(data_Obs$Year[1])-1) +
  theme(legend.position = "right") +
  theme(legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Create the output

output = list("ANPPPlot"=ANPPPlot,"weightsPlot"=weightsPlot)
outputName = paste0(Site,"_Plots")
assign(outputName,output)

}
