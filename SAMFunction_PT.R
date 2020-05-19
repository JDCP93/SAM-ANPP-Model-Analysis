SAM_PT <- function(Site,ANPP,Precip,Tair,Nlag,block,prior=FALSE,model="Obs"){
   
   # Function that runs a SAM model as per Ogle et al 2015 and outputs modelled
   # ANPP as well as a variety of performance metrics
   #  
   # Inputs:
   # - Site = 6 character object to identify the site
   # - Precip = a Mx12 matrix where each row corresponds to a year and each column
   #            corresponds to a certain month (i.e. column 1 = Jan, col 2 = Feb)
   # - ANPP = a Nx2 matrix where column 1 is the year and 
   #         column 2 is the NEE for that year
   # - Nlag = number of years of antecedent precipitation to consider
   # - block = a Nlag x 12 matrix where [i,j] is the time block that month i
   #           of year j is assigned to
   # - prior = Boolean operator. Default is FALSE. If TRUE, suppress ANPP 
   #           observation data so that priors are calculated
   # - model = character vector naming the model from ANPP is taken. Default is
   #           "Obs" where observations are used.
   # Outputs:
   # - ANPPmod = 4 vectors of modelled ANPP mean, sd, 2.5th and 97.5quantile
   # - alpha = mean, sd and quantiles for covariates in calculation of ANPP
   # - cumulativeWeights = mean, sd and quantiles for cumulative monthly weights
   # - yearlyWeights = mean, sd and quantiles for normalised yearly weights
   # - monthlyWeights = mean, sq and quantiles for ordered monthly weights
   # - DIC = Deviance Information Criterion 
   # - R2 = R^2 measure of modelled vs observed ANPP
   # - MAE = mean absolute error of modelled vs observed ANPP
   # - Q5 = 5th quantile of mean ANPP
   # - Q95 = 95th quantile of mean ANPP
   # - NMSE = normalised mean square error of modelled vs observed ANPP
   
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
      print(paste0("Running prior SAM_PT model for ",Site," with lag ",Nlag," and ",Data$Nblocks, " blocks, using ",model," data"))
   }else{print(paste0("Running posterior SAM_PT model for ",Site," with lag ",Nlag," and ",Data$Nblocks, " blocks, using ",model," data"))}

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
      name = paste0(Site,"_PT_",model,"_pri_",Nlag,"_",Data$Nblocks)  
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
      name = paste0(Site,"_PT_",model,"_pos_",Nlag,"_",Data$Nblocks)
      assign(name,output)
      save(list=c(name),file=paste0(name,".Rdata"))
   
   }
}




timeblocks <- function(Y1,Y2,Y3,Y6,Y12){
   
   # Function that takes an input of 5 integers and returns a length of lag and
   # a block of monthly weights for the SAM function
   # 
   # Inputs:
   # - Y1 = number of years for which each month is assigned a unique weight 
   # - Y2 = number of years for which every 2 months are grouped under a weight
   # - Y3 = number of years for which every 3 months are grouped under a weight
   # - Y6 = number of years for which every 6 months are grouped under a weight
   # - Y12 = number of years for which the entire year is grouped under a weight
   # 
   # Outputs:
   # - Nlag = the total number of past years we assign weights to
   # - block = a Nlag x 12 matrix of monthly weight identifiers 
   
   # Calculate Nlag as sum of years
   Nlag = Y1+Y2+Y3+Y6+Y12
   # Assign the weight ids, with weight id equal to 0 where category of years
   # isn't used (I believe this could be programmed more efficiently) 
   timeblocks = c((1:(Y1*12))*(Y1>0),
                  ((12*Y1)+rep(1:(6*Y2),each=2))*(Y2>0),
                  ((12*Y1)+(6*Y2)+rep(1:(4*Y3),each=3))*(Y3>0),
                  ((12*Y1)+(6*Y2)+(4*Y3)+rep(1:(2*Y6),each=6))*(Y6>0),
                  ((12*Y1)+(6*Y2)+(4*Y3)+(2*Y6)+rep(1:Y12,each=12))*(Y12>0))
   # Remove the weight ids of 0
   timeblocks = timeblocks[timeblocks!=0]
   # Define block
   block = matrix(timeblocks,nrow = Nlag,ncol = 12, byrow=TRUE)
   # Combine into 1 output
   lag = list("Nlag"=Nlag,"block"=block)
}


SpringBlock <- function(Nlag,Unique.years=TRUE){
  # Function that creates timeblocks where each year is split into spring
  # and non-spring blocks. Spring in Colorado is ~April,May,June
  # 
  # Inputs:
  # - Nlag = number of past years we want to consider
  # - Unique.years = boolean operator. If TRUE, each year has unique weights
  # for its spring and non-spring rainfall. If FALSE, all spring rain received
  # over the lag period is assigned the same weight, and similarly for non-spring
  # 
  # Outputs:
  # - Nlag = same as input. Included for consistency with timeblocks function
  # - block = a Nlag x 12 matrix of monthly weight identifiers
   
  # if we want each years to have unique weights
  if (Unique.years==TRUE){
    block = NULL
    for (i in seq(1,Nlag*2,by=2)){
      row = c(rep(i,3),rep(i+1,3),rep(i,6))
      block = c(block,row)}
    block = matrix(block,nrow=Nlag,ncol=12,byrow=TRUE)
    print(block)
  } else {
  # otherwise we just repeat the first year as many times as needed
      block = rep(c(rep(1,3),rep(2,3),rep(1,6)),Nlag)
      block = matrix(block,nrow=Nlag,ncol=12,byrow=TRUE)
  }
  lag = list("Nlag"=Nlag,"block"=block)
}