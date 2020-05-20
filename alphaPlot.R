alphaPlot = function(Site,Models){

# Initialise dataframe
k = 0
alphas = data.frame("Model" = rep(0,2*length(Models)),
                    "Mean" = rep(0,2*length(Models)),
                    "Min" = rep(0,2*length(Models)),
                    "Max" = rep(0,2*length(Models)),
                    "Significant" = rep(0,2*length(Models)),
                    "Variable" = rep(0,2*length(Models)))


# This is done outside of SAMPlot as all sites will be on one set of axes
for (Model in Models){
  # Load the SAM model output for the site
  name = paste0(Site,"_PT_",Model,"_pos_",Nlag,"_",12+((Nlag>1)*6)+((Nlag>2)*(Nlag-2)*4))
  load(paste0(name,".Rdata"))
  # Extract the data needed
  k = k + 1
  alphas$Model[k] = Model
  alphas$Mean[k] = eval(parse(text=name))$alphas$mean[2]/eval(parse(text=name))$alphas$mean[2]
  alphas$Min[k] = eval(parse(text=name))$alphas$min[2]/eval(parse(text=name))$alphas$mean[2]
  alphas$Max[k] = eval(parse(text=name))$alphas$max[2]/eval(parse(text=name))$alphas$mean[2]
  # Check whether significantly different from zero (i.e. min and max have same sign)
  alphas$Significant[k] = 1*(sign(alphas$Min[k])==sign(alphas$Max[k]))
  alphas$Variable[k] = "PPT"
  k = k + 1
  alphas$Model[k] = Model
  alphas$Mean[k] = eval(parse(text=name))$alphas$mean[3]/eval(parse(text=name))$alphas$mean[3]
  alphas$Min[k] = eval(parse(text=name))$alphas$min[3]/eval(parse(text=name))$alphas$mean[3]
  alphas$Max[k] = eval(parse(text=name))$alphas$max[3]/eval(parse(text=name))$alphas$mean[3]
  # Check whether significantly different from zero (i.e. min and max have same sign)
  alphas$Significant[k] = 1*(sign(alphas$Min[k])==sign(alphas$Max[k]))
  alphas$Variable[k] = "Tair"
}

# Plot
alphaPlot = ggplot(data = alphas) +
  geom_hline(yintercept=0, linetype = "dashed",color="grey") +
  geom_pointrange(aes(x=Variable,y=Mean,ymin=Min,ymax=Max,color=as.factor(Significant)),position = position_dodge(width = 0.5)) +
  scale_color_manual(values=c("black","red"),limits=c("0","1")) +
  theme(legend.position="") + 
  labs(title = paste0(Site),
       y = "Mean Covariate Value") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap(~Model,
             scales = "free_y")
}