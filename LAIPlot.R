LAIPlot = function(Site,Model){
  
#*******************************************************************************
# Function Description
#*******************************************************************************

# A function that plots the predicted LAI for a given site and model
#
# Inputs:
# - Site: A character site ID for the site for which we want to plot the results
# - Model: A character vector with the name of the model
# 
# Outputs:
# - weightsPlot: A ggplot of the monthly weights
# - ANPPPlot: A ggplot of the observed, linearly modelled and SAM ANPP values


# Load the observations for the site
name = paste0(Site,"_YearlyData")
load(paste0(name,".Rdata"))
# Change name to a consistent expression
data_yearly = eval(parse(text=name))

# Load the observations for the site
name = paste0(Site,"_MonthlyData")
load(paste0(name,".Rdata"))
# Change name to a consistent expression
data_monthly = eval(parse(text=name))

#*******************************************************************************
# Extract LAI data for the model
#*******************************************************************************
LAI = unname(unlist(data_monthly[colnames(data_monthly)==paste0("LAI_",Model)]))
# Put into a matrix
LAI = matrix(LAI,ncol=12,byrow = TRUE)
# Calculate monthly means
LAI = colMeans(LAI)
# Assemble data frame
LAI = data.frame("Month" = c("Jan","Feb","Mar","Apr",
                 "May","Jun","Jul","Aug",
                 "Sep","Oct","Nov","Dec"),"LAI"=LAI,"d"=1)

 # Assign factors so that months appear in order
 LAI$Month = factor(LAI$Month, 
                   levels = (c("Jan","Feb","Mar","Apr",
                               "May","Jun","Jul","Aug",
                               "Sep","Oct","Nov","Dec")))


# Create plots for mean monthly LAI
Plot = ggplot(LAI) +
        geom_point(aes(x=Month,y=LAI,group = d)) +
        geom_line(aes(x=Month,y=LAI,group = d)) +
        #scale_x_continuous(breaks=1:12, 
        #                   labels=c("Jan","Feb","Mar","Apr",
        #                            "May","Jun","Jul","Aug",
        #                            "Sep","Oct","Nov","Dec")) +
        theme(axis.ticks.x = element_blank(),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        ylim(0,7) +
        labs(title = paste0(Site," - ",Model),
             x = "Month",
             y = "Mean LAI")
 

# Create the output
output = list("Plot"=Plot)
outputName = paste0(Site,"_LAIPlot")
assign(outputName,output)

}
