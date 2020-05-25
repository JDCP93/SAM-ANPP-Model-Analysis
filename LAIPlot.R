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
# - Site_LAIPlot: A list consisting of the plot of mean monthly LAI 

# Load the observations for the site
name = paste0(Site,"_MonthlyData")
load(paste0(name,".Rdata"))
# Change name to a consistent expression
data_monthly = eval(parse(text=name))


#*******************************************************************************
# Find max and min LAI for the site from any model
#*******************************************************************************
# This will allow the plots to share the same scale

allLAI = data_monthly[, grepl("LAI",names(data_monthly))]
allLAI$group = rep(1:12,nrow(allLAI)/12)

allLAI = allLAI %>%
        group_by(group) %>%
        summarise_all(mean) %>%
        select(-group)


maxLAI = max(allLAI) 
minLAI = min(allLAI) 
#*******************************************************************************
# Extract LAI data for the model
#*******************************************************************************
LAI = unname(unlist(allLAI[colnames(allLAI)==paste0("LAI_",Model)]))
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
        theme(axis.ticks.x = element_blank(),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        ylim(0.99*minLAI,1.01*maxLAI) +
        labs(title = paste0(Site," - ",Model),
             x = "Month",
             y = "Mean LAI")
 

# Create the output
output = list("Plot"=Plot)
outputName = paste0(Site,"_LAIPlot")
assign(outputName,output)

}
