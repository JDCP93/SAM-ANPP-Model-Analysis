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

# Take out LAI data
allLAI = data_monthly[, grepl("LAI",names(data_monthly))]

# Group by month, taking means
allLAI_mean = allLAI
colnames(allLAI_mean) = paste0(colnames(allLAI_mean),"_mean")
allLAI_mean$group = rep(1:12,nrow(allLAI_mean)/12)
allLAI_mean = allLAI_mean %>%
              group_by(group) %>%
              summarise_all(mean)

# Group by months, taking sd
allLAI_sd = allLAI
colnames(allLAI_sd) = paste0(colnames(allLAI_sd),"_sd")
allLAI_sd$group = rep(1:12,nrow(allLAI)/12)
allLAI_sd = allLAI_sd %>%
  group_by(group) %>%
  summarise_all(sd)

# COmbine into one dataframe
allLAI = merge(allLAI_mean,allLAI_sd,by="group")

# Extract max and min for plotting purposes
maxLAI = max(allLAI_mean[,-1]+allLAI_sd[,-1]) 
minLAI = min(allLAI_mean-allLAI_sd) 
#*******************************************************************************
# Extract LAI data for the model
#*******************************************************************************

LAI = unname(unlist(allLAI[colnames(allLAI)==paste0("LAI_",Model,"_mean")]))
LAI_sd = unname(unlist(allLAI[colnames(allLAI)==paste0("LAI_",Model,"_sd")]))
# Assemble data frame
LAI = data.frame("Month" = c("Jan","Feb","Mar","Apr",
                 "May","Jun","Jul","Aug",
                 "Sep","Oct","Nov","Dec"),"LAI"=LAI,"SD"=LAI_sd,"d"=1)

# Assign factors so that months appear in order
LAI$Month = factor(LAI$Month, 
                   levels = (c("Jan","Feb","Mar","Apr",
                               "May","Jun","Jul","Aug",
                               "Sep","Oct","Nov","Dec")))


# Create plots for mean monthly LAI
Plot = ggplot(LAI) +
        geom_point(aes(x=Month,y=LAI,group=d)) +
        geom_errorbar(aes(x=Month,ymin=LAI-SD,ymax=LAI+SD,group=d)) +
        geom_line(aes(x=Month,y=LAI,group=d)) +
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
