#*******************************************************************************
# Initialisation
#*******************************************************************************

# Clear console
cat("\014")
# Delete all variables
rm(list=ls())
# Close plots
graphics.off()

# Source the custom functions
source("DataExtraction.R")
source("TimeSeries.R")
source("OrderSites.R")
source("SAMFunction_P.R")
source("SAMPlot_P.R")
source("SAMFunction_PT.R")
source("SAMPlot_PT.R")
source("RunSAM.R")
source("alphaPlot.R")


# Source required packages
library(ggplot2)
library(gridExtra)
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#*******************************************************************************
# CHOOSE YOUR VARIABLES
#*******************************************************************************
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  List the sites we have available
Sites = c("Brandbjerg",
          "Garraf",
          "Konza",
          "Lahav",
          "Matta",
          "Prades",
          "Puechabon",
          "SGS",
          "Stubai",
          "WB")

# List the models
Models = c("Obs",
           "CABLE",
           "DLEM",
           "LPX",
           "TC",
           "TECO")

# Decide the number of years of rainfall to consider when talking about antecedent
# rainfall/temperature
Nlag = 3

#*******************************************************************************
# Data Extraction - This can be skipped if .Rdata files already exist
#*******************************************************************************

# Make sure we have the data extracted (runs quickly)
k = 0
for (Site in Sites){
  if (file.exists(paste0(Site,"_DailyData.Rdata"))){
    k = k + 1
  }
}

if (k == length(Sites)){
  print("Daily time series already exist for all sites")
} else {
  DataExtraction(Sites)
}

# For each site, create monthly and yearly timeseries data
for (Site in Sites){
  if (file.exists(paste0(Site,"_MonthlyData.Rdata"))&file.exists(paste0(Site,"_YearlyData.Rdata"))){
    print(paste0("Monthly and yearly time series already exist for ",Site))
  } else {
    TimeSeries(Site)
  }
}

#*******************************************************************************
# Load the site time series
#*******************************************************************************

for (Site in Sites){
  load(paste0(Site,"_DailyData.Rdata"))
  load(paste0(Site,"_MonthlyData.Rdata"))
  load(paste0(Site,"_YearlyData.Rdata"))
}

#*******************************************************************************
# Create site lists ordered by characteristics
#*******************************************************************************

# First order the sites by various characteristics
OrderedSites = OrderSites(Sites)

#*******************************************************************************
# Plot Precip vs GPP for each site
#*******************************************************************************

# Initialise indexing variable and list
k = 0
SitePlot = list()

# For each site, plot Precip against GPP
# Note we exclude years where either 0 precip was recorded or ANPP data is missing

for (i in OrderedSites$ByMAT){
  Data = eval(as.name(paste0(i,"_YearlyData")))
  k = k + 1
  plot <- ggplot(Data[!is.na(Data$ANPP)&Data$PPT>0,]) +
    geom_point(aes(PPT[!is.na(ANPP)&PPT>0],ANPP[!is.na(ANPP)&PPT>0]),
               size=4) +
    geom_smooth(aes(PPT[!is.na(ANPP)&PPT>0],ANPP[!is.na(ANPP)&PPT>0]),
                formula = y ~ x,
                method=MASS::rlm,
                na.rm=TRUE) +
    labs(title=paste(i),
         tag=paste(k)) +
    xlab(bquote('P (mm'~yr^-1*')')) +
    ylab(bquote('ANPP (gC'~m^-2~yr^-1*')')) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  # Store the plot in the SitePlot list  
  SitePlot[[k]]=plot
}

# Plot all sites in one figure
grid.arrange(grobs=SitePlot,top="P vs ANPP per Site, ordered by MAT")

# Tidy up
rm(list = c("plot","k","SitePlot"))

#*******************************************************************************
# Now plot ANPP vs previous year precipitation
#*******************************************************************************

# Initialise indexing variable and list
k = 0
SitePlot = list()

# For each site, plot Precip against GPP
# Note we exclude years where either 0 precip was recorded or ANPP data is 
# missing

for (i in OrderedSites$ByMAT){
  Data = eval(as.name(paste0(i,"_YearlyData")))
  Data = Data[!is.na(Data$ANPP)&Data$PPT>0,]
  Data = data.frame("Year"=Data$Year[2:length(Data$ANPP)],
                    "ANPP"=Data$ANPP[2:length(Data$ANPP)],
                    "PPTant"=Data$PPT[1:(length(Data$ANPP)-1)])
  k = k + 1
  plot <- ggplot(Data) +
    geom_point(aes(PPTant,ANPP),
               size=4) +
    geom_smooth(aes(PPTant,ANPP),
                formula = y ~ x,
                method=lm,
                na.rm=TRUE) +
    labs(title=paste(i),
         tag=paste(k)) +
    xlab(bquote('P (mm'~yr^-1*')')) +
    ylab(bquote('ANPP (gC'~m^-2~yr^-1*')')) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  # Store the plot in the SitePlot list  
  SitePlot[[k]]=plot
}

# Plot all sites in one figure
grid.arrange(grobs=SitePlot,top="Prior Year P vs ANPP per Site, order by MAT")

# Tidy up
rm(list = c("plot","k","SitePlot","Data"))

#*******************************************************************************
# Now plot ANPP vs temperature!
#*******************************************************************************

# Initialise indexing variable and list
k = 0
SitePlot = list()

# For each site, plot Precip against GPP
# Note we exclude years where either 0 precip was recorded or ANPP data is 
# missing

for (i in OrderedSites$ByMAT){
  Data = eval(as.name(paste0(i,"_YearlyData")))
  Data = Data[!is.na(Data$ANPP)&Data$PPT>0,]
  Data = data.frame("Year"=Data$Year,
                    "ANPP"=Data$ANPP,
                    "Tair"=Data$Tair)
  k = k + 1
  plot <- ggplot(Data) +
    geom_point(aes(Tair,ANPP),
               size=4) +
    geom_smooth(aes(Tair,ANPP),
                formula = y ~ x,
                method=MASS::rlm,
                na.rm=TRUE) +
    labs(title=paste(i),
         tag=paste(k)) +
         xlab(bquote('T ('*degree*'C)')) +
         ylab(bquote('ANPP (gC'~ m^-2~yr^-1*')')) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  # Store the plot in the SitePlot list  
  SitePlot[[k]]=plot
}

# Plot all sites in one figure
grid.arrange(grobs=SitePlot,top="T vs ANPP per Site, ordered by MAT")

# Tidy up
rm(list = c("plot","k","SitePlot","Data"))

#*******************************************************************************
# Perform SAM_P modelling
#*******************************************************************************

# We should be able to use the SAM function and model that was used for the
# GrasslandLag work 
# To do so, we must create the matrix of precipitation and then call the 
# SAMFunction

# Create the block using the function from SAMFunction
block = timeblocks(1,(Nlag>1)*1,(Nlag>2)*(Nlag-2),0,0)$block

for (i in Sites){
  # If the modelling has already been run, let us know and don't run again
  if (file.exists(paste0(i,"_P_Obs_pos_",Nlag,"_",max(block),".Rdata"))){
    print(paste0("SAM_P output already exists for ",i," with lag ",Nlag," and ",max(block)," blocks"))
  } else {
    #Since all monthly data are full years, we can just straight up form matrices
    # Reference the monthly data for the site
    Data = eval(as.name(paste0(i,"_MonthlyData")))
    # Form the precipitation column into a matrix
    PPT = matrix(Data$PPT,ncol=12,byrow = TRUE)
    # Add the years as the first column
    PPT = cbind(unique(Data$Year),PPT)
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
    # Remove any rows with no observed ANPP
    Data = eval(as.name(paste0(i,"_YearlyData")))
    PPT = PPT[!is.na(Data$ANPP),]
  
    # Create a ANPP matrix

    ANPP = matrix(Data$Year,ncol=1)
    ANPP = cbind(ANPP, Data$ANPP, 1:nrow(ANPP))
    colnames(ANPP) = c("Year",
                      "ANPP",
                      "YearID")
    # Remove any rows with no observed ANPP
    ANPP = ANPP[!is.na(Data$ANPP),]
  
    # We can now call the SAM function
    SAM_P(i,ANPP,PPT,Nlag,block,prior=FALSE)
    
    # Tidy up
    rm(list = c("Data","ANPP","PPT"))
  }
}

#*******************************************************************************
# Plot of alphas to assess significance
#*******************************************************************************

# Initialise dataframe
k = 0
alphas = data.frame("Site" = rep(0,length(Sites)),
                    "Mean" = rep(0,length(Sites)),
                    "Min" = rep(0,length(Sites)),
                    "Max" = rep(0,length(Sites)),
                    "Significant" = rep(0,length(Sites)))

# This is done outside of SAMPlot as all sites will be on one set of axes
for (Site in Sites){
  # Load the SAM model output for the site
  name = paste0(Site,"_P_Obs_pos_",Nlag,"_",12+((Nlag>1)*6)+((Nlag>2)*(Nlag-2)*4))
  load(paste0(name,".Rdata"))
  # Extract the data needed
  k = k + 1
  alphas$Site[k] = Site
  alphas$Mean[k] = eval(parse(text=name))$alphas$mean[2]
  alphas$Min[k] = eval(parse(text=name))$alphas$min[2]
  alphas$Max[k] = eval(parse(text=name))$alphas$max[2]
  # Check whether significantly different from zero (i.e. min and max have same sign)
  alphas$Significant[k] = 1*(sign(alphas$Min[k])==sign(alphas$Max[k]))
  # Remove variables from memory
  rm(list = name)
}

# Plot
alphaPlot = ggplot(data = alphas) +
  geom_hline(yintercept=0, linetype = "dashed",color="grey") +
  geom_pointrange(aes(x=Site,y=Mean,ymin=Min,ymax=Max,color=as.factor(Significant))) +
  scale_color_manual(values=c("black","red"),limits=c("0","1")) +
  theme(legend.position="") + 
  labs(title = paste0("Covariate of antecedent P term from SAM modelling for each site - ",
                      Nlag-1,
                      " year lag"),
       y = "Mean Covariate Value") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Show plot
grid.arrange(alphaPlot)

# Tidy up
rm(list = "alphaPlot")

#*******************************************************************************
# Plot the SAM_P model results
#*******************************************************************************

# Initialise index and lists of plots
k = 0
j = 0
ANPPPlots = list()
weightsPlots = list()

# Run the function for each site
for (i in OrderedSites$ByMAT){
  # Increase index
  k = k + 1
  # Run plotting
  output = SAMPlot_P(i,Nlag)
  # If alpha is significantly different from 0, keep the plot of monthly weights
  if (i %in% alphas$Site[alphas$Significant==1]){
    j = j + 1
    weightsPlots[[j]] = output$weightsPlot
  }
  # Keep SAM modelled ANPP
  ANPPPlots[[k]] = output$ANPPPlot
}

# Display the significant monthly weights
# First make sure there is something to plot
if (length(weightsPlots) == 0){
  print("No significant weights from SAM_P modelling")
} else {
  grid.arrange(grobs=weightsPlots,
             top = paste0("Weights for SAM_P Model per Site, ordered by MAT - ",
                          Nlag-1,
                          " year lag"))
        }
# Display ANPP plots of all sites
grid.arrange(grobs=ANPPPlots, 
             top = paste0("SAM_P Modelled vs 'Observed' ANPP per Site, ordered by MAT - ",
                          Nlag-1,
                          " year lag"))

# Tidy up
rm(list = c("weightsPlots","ANPPPlots","output","k","j"))

#*******************************************************************************
# Perform SAM_PT modelling
#*******************************************************************************

for (i in Sites){
  # If the modelling has already been run, let us know and don't run again
  if (file.exists(paste0(i,"_PT_Obs_pos_",Nlag,"_",max(block),".Rdata"))){
    print(paste0("SAM_PT output already exists for ",i," with lag ",Nlag," and ",max(block)," blocks"))
  } else {
    #Since all monthly data are full years, we can just straight up form matrices
    # Reference the monthly data for the site
    Data = eval(as.name(paste0(i,"_MonthlyData")))
    # Form the precipitation column into a matrix
    PPT = matrix(Data$PPT,ncol=12,byrow = TRUE)
    # Add the years as the first column
    PPT = cbind(unique(Data$Year),PPT)
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
    # Remove any rows with no observed ANPP
    Data = eval(as.name(paste0(i,"_YearlyData")))
    PPT = PPT[!is.na(Data$ANPP),]
    
    # Form the Tair matrix
    Data = eval(as.name(paste0(i,"_MonthlyData")))
    # Form the Tair column into a matrix
    Tair = matrix(Data$Tair,ncol=12,byrow = TRUE)
    # Add the years as the first column
    Tair = cbind(unique(Data$Year),Tair)
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
    # Remove any rows with no observed ANPP
    Data = eval(as.name(paste0(i,"_YearlyData")))
    Tair = Tair[!is.na(Data$ANPP),]
    
    # Create a ANPP matrix
    ANPP = matrix(Data$Year,ncol=1)
    ANPP = cbind(ANPP, Data$ANPP, 1:nrow(ANPP))
    colnames(ANPP) = c("Year",
                       "ANPP",
                       "YearID")
    # Remove any rows with no observed ANPP
    ANPP = ANPP[!is.na(Data$ANPP),]
    
    # We can now call the SAM function
    SAM_PT(i,ANPP,PPT,Tair,Nlag,block,prior=FALSE)
    
    # Tidy up
    rm(list = c("Data","ANPP","PPT","Tair"))
  }
}

#*******************************************************************************
# Plot of alphas to assess significance
#*******************************************************************************

# Initialise dataframe
k = 0
alphas = data.frame("Site" = rep(0,2*length(Sites)),
                    "Mean" = rep(0,2*length(Sites)),
                    "Min" = rep(0,2*length(Sites)),
                    "Max" = rep(0,2*length(Sites)),
                    "Significant" = rep(0,2*length(Sites)),
                    "Variable" = rep(0,2*length(Sites)))


# This is done outside of SAMPlot as all sites will be on one set of axes
for (Site in Sites){
  # Load the SAM model output for the site
  name = paste0(Site,"_PT_Obs_pos_",Nlag,"_",12+((Nlag>1)*6)+((Nlag>2)*(Nlag-2)*4))
  load(paste0(name,".Rdata"))
  # Extract the data needed
  k = k + 1
  alphas$Site[k] = Site
  alphas$Mean[k] = eval(parse(text=name))$alphas$mean[2]/eval(parse(text=name))$alphas$mean[2]
  alphas$Min[k] = eval(parse(text=name))$alphas$min[2]/eval(parse(text=name))$alphas$mean[2]
  alphas$Max[k] = eval(parse(text=name))$alphas$max[2]/eval(parse(text=name))$alphas$mean[2]
  # Check whether significantly different from zero (i.e. min and max have same sign)
  alphas$Significant[k] = 1*(sign(alphas$Min[k])==sign(alphas$Max[k]))
  alphas$Variable[k] = "PPT"
  k = k + 1
  alphas$Site[k] = Site
  alphas$Mean[k] = eval(parse(text=name))$alphas$mean[3]/eval(parse(text=name))$alphas$mean[3]
  alphas$Min[k] = eval(parse(text=name))$alphas$min[3]/eval(parse(text=name))$alphas$mean[3]
  alphas$Max[k] = eval(parse(text=name))$alphas$max[3]/eval(parse(text=name))$alphas$mean[3]
  # Check whether significantly different from zero (i.e. min and max have same sign)
  alphas$Significant[k] = 1*(sign(alphas$Min[k])==sign(alphas$Max[k]))
  alphas$Variable[k] = "Tair"
  # Remove the model output from memory to keep things clean
  rm(list = name)
}

# Create the plot
alphaPlot = ggplot(data = alphas) +
  geom_hline(yintercept=0, linetype = "dashed",color="grey") +
  geom_pointrange(aes(x=Site,
                      y=Mean,
                      ymin=Min,
                      ymax=Max,
                      shape=Variable,
                      color=as.factor(Significant)),
                  position = position_dodge(width = 0.5)) +
  scale_color_manual(values=c("black","red"),limits=c("0","1")) +
  scale_shape_manual(values=c(18,20),limits=c("PPT","Tair")) +
  theme(legend.position="") + 
  labs(title = paste0("Normalised covariates of antecedent terms from SAM_PT modelling for each site - ",
                      Nlag-1,
                      " year lag"),
       y = "Mean Covariate Value") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Show the plot
grid.arrange(alphaPlot)

# Tidy up
rm(list = "alphaPlot","k")

#*******************************************************************************
# Plot monthly weights and modelled ANPP
#*******************************************************************************

# Initialise index and lists of plots
k = 0
j = 0
m = 0
ANPPPlots_PT = list()
weightsPlots_P = list()
weightsPlots_T = list()

# Run the function for each site
for (i in OrderedSites$ByMAT){
  k = k + 1
  # Assign function output to generic variable
  output = SAMPlot_PT(i,Nlag,"Obs")
  # If P alpha is significant, plot the monthly weights for antecedent P
  if (i %in% alphas$Site[alphas$Significant==1 & alphas$Variable=="PPT"]){
    j = j + 1
    weightsPlots_P[[j]] = output$weightsPlot_P
  }
  # If P alpha is significant, plot the monthly weights for antecedent P
  if (i %in% alphas$Site[alphas$Significant==1 & alphas$Variable=="Tair"]){
    m = m + 1
    weightsPlots_T[[m]] = output$weightsPlot_T
  }
  # Plot the SAM modelled ANPP
  ANPPPlots_PT[[k]] = output$ANPPPlot
  # Tidy up
  rm(list = "output")
}

# Display the plots together
# If no significant P alphas, don't plot anything
if (length(weightsPlots_P) == 0){
  print("No significant P weights from SAM_PT modelling")
} else {
  # Otherwise plot the weights
  grid.arrange(grobs=weightsPlots_P,
             top = paste0("Significant P weights for SAM_PT Model per Site, ordered by MAT - ",
                          Nlag-1,
                          " year lag"))
}
# If no significant T alphas, don't plot anything
if (length(weightsPlots_T) == 0){
  print("No significant T weights from SAM_PT modelling")
} else {
  # Otherwise plot the weights
  grid.arrange(grobs=weightsPlots_T,
             top = paste0("Significant T weights for SAM_PT Model per Site, ordered by MAT - ",
                          Nlag-1,
                          " year lag"))
}

# Plot the ANPP results
grid.arrange(grobs=ANPPPlots_PT, 
             top = paste0("SAM_PT Modelled vs 'Observed' ANPP per Site, ordered by MAT - ",
                          Nlag-1,
                          " year lag"))

# Tidy up
rm(list = c("weightsPlots_P",
            "weightsPlots_T",
            "ANPPPlots_PT",
            "k",
            "j",
            "m",
            "alphas"))

#*******************************************************************************
# Begin model comparison
#*******************************************************************************

# RUn SAM for all sites and all model data
for (Site in Sites){
  for (Model in Models){
    RunSAM(Site,Model,Nlag=3)
  }
}

# Tidy up
rm(list = c("Site","Model"))
#*******************************************************************************
# Plot and analyze covariate values
#*******************************************************************************

# Initialise lists and index
k = 0
alphaPlots = list()
alphas = list()

# Run plotting function for each site
for (Site in OrderedSites$ByLoS){
  k = k + 1
  output = alphaPlotFunction(Site,Models)
  # Group the alpha plots
  alphaPlots[[k]] = output$alphaPlot
  # Output the dataframe for future use
  name = paste0(Site,"_alphas")
  assign(name,output$alphas)
}

# Arrange plots
grid.arrange(grobs=alphaPlots, 
             top = paste0("Normalised covariates of antecedent terms from SAM_PT modelling for a ",
                          Nlag-1,
                          " year lag (Nlag = ",
                          Nlag,
                          ")"))

# Tidy up
rm(list = c("output","name","alphaPlots","k","Site"))

#*******************************************************************************
# Plot significant Monthly Weights
#*******************************************************************************

# For each site
for (i in OrderedSites$ByLoS){
  # Initiliase index and output lists
  m = 0
  n = 0
  weightsPlots_P = list()
  weightsPlots_T = list()
  # Assign site alphas to generic variable
  alphas = eval(as.name(paste0(i,"_alphas")))
  for (j in Models){
    # Run SAMPlot_PT for the site and the model and assign to a consistent name
    output = SAMPlot_PT(i,Nlag,j)
    # If the model's P alpha is significant in SAM, plot the monthly P weights 
    if (j %in% alphas$Model[alphas$Significant==1 & alphas$Variable=="PPT"]){
      m = m + 1
      weightsPlots_P[[m]] = output$weightsPlot_P
    }
    # If the model's T alpha is significant in SAM, plot the monthly T weights 
    if (j %in% alphas$Model[alphas$Significant==1 & alphas$Variable=="Tair"]){
      n = n + 1
      weightsPlots_T[[n]] = output$weightsPlot_T
    }
  }
  # If we have at least 1 significant P alpha for the site, let's see it!
  if (length(weightsPlots_P)>0){
  grid.arrange(grobs = weightsPlots_P, bottom = "Significant P weights")
  }
  # If we have at least 1 significant T alpha for the site, let's see it!
  if (length(weightsPlots_T)>0){
  grid.arrange(grobs = weightsPlots_T, bottom = "Significant T weights")
  }
}

# Tidy up
rm(list = c("weightsPlots_P",
            "weightsPlots_T",
            "output",
            "alphas",
            "m","n","i","j"))

