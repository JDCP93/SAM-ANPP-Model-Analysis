# Bayesian statistical model of daily NEE response as an AR(1) process following the stochastic antecedent
# modeling (SAM) framework (Ogle et al., 2015) and the work of Liu et al (2019). Model is fit to one site at a time.

# ------------------ Inputs ------------------
# Nmem = A scalar that equals the record-length at a site 
# NEE.res = A vector of residuals between the predicted NEE from the SAM approach 
#           and the observed

model {
  # Likelihood and mean model, looping over daily NEE residuals at this site
  for(r in 2:length(NEE.res)){ # r is the t in the supplemental material model description
    # Likelihood for daily NEE.res data:
    NEE.res[r] ~ dnorm(b0 + b1*NEE.res[r-1], sig.res^2)
  }
  # Standard, relatively non-informative priors for the NEE standard deviation 
  # and AR(1) parameters
  sig.res ~ dunif(0, 100)
  b0 ~ dnorm(0,1000)
  b1 ~ dnorm(0,1000)
  
}