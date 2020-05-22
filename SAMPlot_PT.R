SAMPlot_PT = function(Site,Nlag,Model){
  
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
name = paste0(Site,"_PT_",Model,"_pos_",Nlag,"_",12+((Nlag>1)*6)+((Nlag>2)*(Nlag-2)*4))
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
plotWeights = data.frame("Weights_P_mean"=as.vector(data_SAM$monthlyWeights_P$mean),
                         "Weights_P_min"=as.vector(data_SAM$monthlyWeights_P$min),
                         "Weights_P_max"=as.vector(data_SAM$monthlyWeights_P$max),
                         "Weights_T_mean"=as.vector(data_SAM$monthlyWeights_T$mean),
                         "Weights_T_min"=as.vector(data_SAM$monthlyWeights_T$min),
                         "Weights_T_max"=as.vector(data_SAM$monthlyWeights_T$max),
                         "YearIntoPast"=rep(0:(Nlag-1),each=12),
                         "Month"=rep(c("Dec","Nov","Oct","Sep", "Aug","Jul",
                                       "Jun", "May","Apr","Mar","Feb","Jan"),Nlag))

# Assign factors so that months appear in order
plotWeights$Month = factor(plotWeights$Month, 
                           levels = rev(c("Jan","Feb","Mar","Apr",
                                          "May","Jun","Jul","Aug",
                                          "Sep","Oct","Nov","Dec")))
# Create plots for each variable
weightsPlot_P = ggplot(plotWeights) +
              geom_errorbar(aes(x = Month, 
                                ymin=Weights_P_min, 
                                ymax = Weights_P_max),
                            linetype = "solid",
                            size=0.5,
                            color="blue") +
             geom_point(aes(x = Month, y = Weights_P_mean), color = "blue") +
             geom_hline(yintercept = 1/(12*Nlag-3),
                        linetype = "dashed") +
             theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   legend.position = "none",
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black")) +
             ylim(0,max(plotWeights$Weights_P_max*1.1)) +
             facet_grid(.~YearIntoPast,
                        scales = "free_x",
                        switch = "x", 
                        space = "free_y") +
             labs(title = paste0(Site," - ",Model),
                  x = "Month/Year into Past",
                  y = "Weight")
 
weightsPlot_T = ggplot(plotWeights) +
                geom_errorbar(aes(x = Month, 
                                  ymin=Weights_T_min, 
                                  ymax = Weights_T_max),
                              linetype = "solid",
                              size=0.5,
                              color="red") +
                geom_point(aes(x = Month, y = Weights_T_mean), color = "red") +
                geom_hline(yintercept = 1/(12*Nlag-3),
                            linetype = "dashed") +
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none",
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      axis.line = element_line(colour = "black")) +
                ylim(0,max(plotWeights$Weights_T_max*1.1)) +
                facet_grid(.~YearIntoPast,
                            scales = "free_x",
                            switch = "x", 
                            space = "free_y") +
                labs(title = paste0(Site," - ",Model),
                      x = "Month/Year into Past",
                      y = "Weight")
  
#*******************************************************************************
# Plot of Modelled ANPP vs Observed ANPP
#*******************************************************************************

# Conduct a linear regression between yearly P + T and ANPP
PPT = data_Obs$PPT[!is.na(data_Obs$ANPP)]
Tair = data_Obs$Tair[!is.na(data_Obs$ANPP)]
ANPP = data_Obs$ANPP[!is.na(data_Obs$ANPP)]

Model = lm(ANPP ~ PPT + Tair, data = data.frame("Tair"=Tair,"PPT"=PPT,"ANPP"=ANPP))

# Create dataframes for plotting
ANPPobs = data.frame(Year=1:length(ANPP)+as.numeric(data_Obs$Year[1])-1,
                    ANPP_obs = ANPP)


ANPPsam = data.frame(Year=1:length(data_SAM$ANPPmod$mean)+Nlag-2+as.numeric(data_Obs$Year[1]),
                    ANPP_SAM = data_SAM$ANPPmod$mean,
                    ANPP_SAMmin = data_SAM$ANPPmod$min,
                    ANPP_SAMmax = data_SAM$ANPPmod$max)

ANPPlm = data.frame(Year=1:length(ANPP[!is.na(ANPP)])+as.numeric(data_Obs$Year[1])-1,
                   ANPP_lm = Model$fitted.values)

# Plot the graphs

ANPPPlot <- ggplot() +
  geom_ribbon(data=ANPPsam, aes(x=Year, ymin=ANPP_SAMmin, ymax=ANPP_SAMmax), fill="red", alpha=0.4) +
  geom_line(data=ANPPsam,aes(Year,ANPP_SAM,color="SAM"),size = 2) +
  geom_line(data=ANPPlm,aes(Year,ANPP_lm,color="LRM"), size = 2,linetype = "dashed") +
  geom_point(data=ANPPobs,aes(x=Year,y=ANPP_obs,color="Obs"),size=3,na.rm=TRUE) +
  geom_line(data=ANPPobs,aes(Year,ANPP_obs,color="Obs"), size = 1) +
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

output = list("ANPPPlot"=ANPPPlot,
              "weightsPlot_P"=weightsPlot_P,
              "weightsPlot_T"=weightsPlot_T)
outputName = paste0(Site,"_Plots")
assign(outputName,output)

}
