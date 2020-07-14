
# A junk script to test out various diagnostic tools and explore model output
# in a haphazard manner



library(coda)

# We want all values to be close to 1
gelman.diag(nee_daily,multivariate=FALSE)


library(lattice)

library(mcmcplots)
library(superdiag)

# Plots Z-score and we want all the dots to fall within the -2:2 range
geweke.plot(nee_daily)




load("Wkg_v1.Rdata")
load("./inputs/US-Wkg_LiuInput.Rdata")

jags.mcmc = as.mcmc(jags)
jags.summary = summary(jags.mcmc)

unique(substr(rownames(jags.summary$statistics),1,3))

xyplot(jags.mcmc)

NEE_pred = jags.summary$statistics[substr(rownames(jags.summary$statistics),1,3)=="NEE",1]
NEE_obs = `US-Wkg_LiuInput`$NEE

plot(NEE_pred)
plot(`US-Wkg_LiuInput`$NEE)



