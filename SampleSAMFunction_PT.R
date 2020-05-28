SampleSAM_PT <- function(Site,ANPP,Precip,Tair,Nlag,block,prior=FALSE,model="Obs",SampleLength,StartYear){
   
# Function is identical to SAMFunction_PT.R apart from naming convention which
# uses SampleLength and StartYear to identify that the model has been run for
# a small section of the available data. I couldn't determine how to include 
# these in the standard SAM_PT function as the default values for these change.
# I guess I could have done something like FULL. Oh well.
   
   library(rjags)
   
   # Create input list for the Bayesian model
   Data = list('Nlag' = Nlag
               ,'block'= block
               # number of years for which ANPP data are available,
               ,'N' = nrow(ANPP) 
               # number of years for which monthly precipitation data is available
               ,'Nyrs' = nrow(Precip) 
               # number of time blocks the months are partitioned into
               ,'Nblocks' = max(block)
               # Monthly precip data
               ,'ppt' = (Precip[,2:13])
               # Monthly temperature data
               ,'Tair' = (Tair[,2:13])
               # Year ID for ANPP
               ,'YearID' = 1:nrow(ANPP)
               # Yearly ANPP data - comment this out to obtain the priors
               ,'ANPP' = ANPP[,2] 
   )
   
   # If we're calculating priors, suppress the observed data
   if (prior==TRUE){
      Data$ANPP = NULL
      print(paste0("Running prior SAM_PT model for ",Site," with lag ",Nlag," and ",Data$Nblocks, " blocks, using ",model," data of length ",SampleLength," starting in ",StartYear))
   }else{print(paste0("Running posterior SAM_PT model for ",Site," with lag ",Nlag," and ",Data$Nblocks, " blocks, using ",model," data of length ",SampleLength," starting in ",StartYear))}

   # Define the parameters for the model operation
   # samples to be kept after burn in
   samples = 50000
   # iterations for burn in
   burn = samples * 0.1 
   # number of iterations where samplers adapt behaviour to maximise efficiency
   nadapt = 100  
   # The number of MCMC chains to run
   nchains = 4 
   # thinning rate
   # save every thin-th iteration to reduce correlation between 
   # consecutive values in the chain
   thin = 10 
   
   # Decide the variables to track
   parameters = c('mu','a','weightOrdered_T','cum.weight_T','sumD1_T','weightOrdered_P','cum.weight_P','sumD1_P') 
   
   # Put the model system into a variable
   jags = jags.model('Model_PT.R', data=Data, n.chains=nchains, n.adapt=nadapt) 
   
   # Generate the MCMC chain (this is basically running the Bayesian analysis)
   fit = coda.samples(jags, n.iter=samples, n.burnin=burn, thin=thin,
                      variable.names=parameters)
   # Assign the summary of the model output to a variable
   Summary = summary(fit)
   
   # Assign values for analysis
   N = Data$N
   
   # For each of our tracked variables, compile the mean, 2.5 and 97.5 quantiles.
   for (i in parameters){
      df = data.frame("mean"=Summary$statistics[grep(i,row.names(Summary$statistics)),1],
                      "sd"=Summary$statistics[grep(i,row.names(Summary$statistics)),2],
                      "min"=Summary$quantiles[grep(i,row.names(Summary$quantiles)),1],
                      "max"=Summary$quantiles[grep(i,row.names(Summary$quantiles)),5])
      name = paste(i,"Stats",sep="")
      assign(name,df)
   }
   
   # Normalise the yearly weights
   # Normalise the yearly weights
   sumD1_PStats$sd = sumD1_PStats$sd/sum(sumD1_PStats$mean,na.rm=TRUE)
   sumD1_PStats$min = sumD1_PStats$min/sum(sumD1_PStats$mean,na.rm=TRUE)
   sumD1_PStats$max = sumD1_PStats$max/sum(sumD1_PStats$mean,na.rm=TRUE)
   sumD1_PStats$mean = sumD1_PStats$mean/sum(sumD1_PStats$mean,na.rm=TRUE)
   
   sumD1_TStats$sd = sumD1_TStats$sd/sum(sumD1_TStats$mean,na.rm=TRUE)
   sumD1_TStats$min = sumD1_TStats$min/sum(sumD1_TStats$mean,na.rm=TRUE)
   sumD1_TStats$max = sumD1_TStats$max/sum(sumD1_TStats$mean,na.rm=TRUE)
   sumD1_TStats$mean = sumD1_TStats$mean/sum(sumD1_TStats$mean,na.rm=TRUE)
   
   # if priors are being calculated, the performance metrics are irrelevant
   if (prior==TRUE){ 
      output = list("ANPPmod"=muStats,
                    "alphas"=aStats,
                    "cumulativeWeights_P"=cum.weight_PStats,
                    "yearlyWeights_P"=sumD1_PStats,
                    "monthlyWeights_P"=weightOrdered_PStats,
                    "cumulativeWeights_T"=cum.weight_TStats,
                    "yearlyWeights_T"=sumD1_TStats,
                    "monthlyWeights_T"=weightOrdered_TStats)
      name = paste0(Site,"_PT_",StartYear,"_",SampleLength,"SL_",model,"_pri_",Nlag,"_",Data$Nblocks)  
      assign(name,output)
      save(list=c(name),file=paste0(name,".Rdata"))
   }else{
   # for the posteriors calculate the performance metrics and output
      # Calculate R2
      RSS = sum((muStats$mean-Data$ANPP[Nlag:N])^2,na.rm=TRUE)
      TSS = sum((Data$ANPP-mean(Data$ANPP[Nlag:N],na.rm=TRUE))^2,na.rm=TRUE)
      R2 = 1-RSS/TSS
   
      # Calculate MAE
      MAE = mean(abs(muStats$mean-Data$ANPP[Nlag:N]),na.rm=TRUE)
   
      # Calculate quantiles
      Q5 = abs(quantile(muStats$mean,probs=0.05)-quantile(Data$ANPP[Nlag:N],probs=0.05,na.rm=TRUE))
      Q95 = abs(quantile(muStats$mean,probs=0.95)-quantile(Data$ANPP[Nlag:N],probs=0.95,na.rm=TRUE))
   
      # Calculate NMSE
      num = mean(RSS,na.rm=TRUE)
      den = mean(muStats$mean,na.rm=TRUE)*mean(Data$ANPP[Nlag:N],na.rm=TRUE)
      NMSE = num/den
   
      # Calculate DIC
      dic = dic.samples(jags, n.iter=1000,type="pD")
      dbar = sum(dic$deviance[grep("ANPP",names(dic$deviance))])
      pd = sum(dic$penalty[grep("ANPP",names(dic$penalty))])
      DIC = dbar+pd
   
      output = list("ANPPmod"=muStats,
                    "alphas"=aStats,
                    "cumulativeWeights_P"=cum.weight_PStats,
                    "yearlyWeights_P"=sumD1_PStats,
                    "monthlyWeights_P"=weightOrdered_PStats,
                    "cumulativeWeights_T"=cum.weight_TStats,
                    "yearlyWeights_T"=sumD1_TStats,
                    "monthlyWeights_T"=weightOrdered_TStats,
                    "DIC"=DIC,
                    "R2"=R2,
                    "MAE"=MAE,
                    "Q5"=Q5,
                    "Q95"=Q95,
                    "NMSE"=NMSE)
   
      # Write output file
      name = paste0(Site,"_PT_",StartYear,"_",SampleLength,"SL_",model,"_pos_",Nlag,"_",Data$Nblocks)
      assign(name,output)
      save(list=c(name),file=paste0(name,".Rdata"))
   
   }
}