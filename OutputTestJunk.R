
# A junk script to test out various diagnostic tools and explore model output
# in a haphazard manner
rm(list=ls())
# load model inputs
load('NEE_project/inputs/US-Wkg_Input.Rdata')

# 50000 iterations on 6 chains 
load('results/NEE_output_site_US-Wkg_2020-07-11.rda')
# data inside is called "nee_daily"
assign('v1',nee_daily)


# 75000 iterations on 3 chains 
load('results/NEE_output_site_US-Wkg_2020-07-15.rda')
# data inside is called "nee_daily"
assign('v2',nee_daily)



# set version
results = v2
library(coda)

# We want all values to be close to 1
gelman.diag(results,multivariate=FALSE)


library(lattice)

library(mcmcplots)
library(superdiag)

# Plots Z-score and we want all the dots to fall within the -2:2 range
geweke.plot(results)





# Summarise for other uses (means, quantiles, etc.)
results.summary=summary(results)
# Check which variables we tracked
unique(substr(rownames(results.summary$statistics),1,8))


# takes 5EVER OMGOSHHHHHH
# testing me
### xyplot(nee_daily)


# Check obs vs predicted
library(ggplot2)
NEE_pred = results.summary$statistics[substr(rownames(results.summary$statistics),1,3)=="NEE",1]
NEE_obs = `US-Wkg_Input`$NEE[-(1:365)]

plot1 <- ggplot(data.frame(NEE_obs,NEE_pred)) +
        geom_point(aes(NEE_obs,NEE_pred)) +
        geom_abline(slope=1,intercept=0) +
        xlim(-3.5,3.5) +
        ylim(-3.5,3.5)


plot1


# Check cumulative weights
cumSWR = results.summary$statistics[substr(rownames(results.summary$statistics),1,13)=="cum_weightA[2",1]
ggplot(data.frame(cumSWR)) + geom_point(aes(1:length(cumSWR),cumSWR)) + ylim(0,1)

cumTair = results.summary$statistics[substr(rownames(results.summary$statistics),1,13)=="cum_weightA[1",1]
ggplot(data.frame(cumTair)) + geom_point(aes(1:length(cumTair),cumTair)) + ylim(0,1)

cumVPD = results.summary$statistics[substr(rownames(results.summary$statistics),1,13)=="cum_weightA[3",1]
ggplot(data.frame(cumVPD)) + geom_point(aes(1:length(cumVPD),cumVPD)) + ylim(0,1)

cumSWC = results.summary$statistics[substr(rownames(results.summary$statistics),1,13)=="cum_weightA[5",1]
ggplot(data.frame(cumSWC)) + geom_point(aes(1:length(cumSWC),cumSWC)) + ylim(0,1)

cumPPT = results.summary$statistics[substr(rownames(results.summary$statistics),1,12)=="cum_weightAP",1]
ggplot(data.frame(cumPPT)) + geom_point(aes(c(0,20,29,59,119,179,269,365),cumPPT)) + ylim(0,1)
