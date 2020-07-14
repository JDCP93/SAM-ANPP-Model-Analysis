
# A junk script to test out various diagnostic tools and explore model output
# in a haphazard manner
# 
# load model inputs
load('NEE_project/inputs/US-Wkg_Input.Rdata')

# 50000 iterations on 6 chains 
load('NEE_output_site_US-Wkg_2020-07-11.rda')
# data inside is called "nee_daily"

library(coda)

# We want all values to be close to 1
gelman.diag(nee_daily,multivariate=FALSE)


library(lattice)

library(mcmcplots)
library(superdiag)

# Plots Z-score and we want all the dots to fall within the -2:2 range
geweke.plot(nee_daily)




# Summarise for other uses (means, quantiles, etc.)
nee_daily.summary=summary(nee_daily)
# Check which variables we tracked
unique(substr(rownames(nee_daily.summary$statistics),1,8))


# takes 5EVER OMGOSHHHHHH
# testing me
### xyplot(nee_daily)


# Check obs vs predicted
NEE_pred = nee_daily.summary$statistics[substr(rownames(nee_daily.summary$statistics),1,3)=="NEE",1]
NEE_obs = `US-Wkg_Input`$NEE

plot(NEE_pred,`US-Wkg_Input`$NEE[-(1:365)])



