## Model Code

# SAM model for replicating Ogle et al, 2015

# We are looking into the effects of antecedent conditions on ANPP
# namely precipitation received over the past five years and then the amount
# of precipitation received in the growing year within 4 classifications of
# rainfall events based on intensity

model{
  
  # Assign priors to the ANPP regression parameters
  # These are very precisely zero i.e. mean of zero and very low variance
  for (k in 1:2) {
    a[k] ~ dnorm(0, 0.0000001)
  }
  
  # Assign prior for the weight for each time block of prior precipitation
  # Ogle uses Dirichlet priors for the weights of time block precipitation.
  # This is coded as a gamma distribution due to some reason relating to R
  for (j in 1:Nblocks) {
    deltaX[j] ~ dgamma(1, 1)
  }
  
  # Compute the monthly weights for the prior precipitation
  for (t in 1:Nlag) {
    for (m in 1:12) {
      # weight for precipitation received after the ANPP harvest for
      # the current year is 0 (Oct,Nov,Dec)
      delta[m, t] <- (deltaX[block[t, m]]) * (1 - equals(t, 1) * step(m - 9.5))
      # normalise the monthly weights (sumD is defined below)
      weight[m, t] <- delta[m, t] / sumD
      # Reorder the weights in order of "recentness" so that
      # they run from Dec of current year, Nov,... through to Feb,
      # Jan of oldest year as a vector rather than a matrix
      weightOrdered[(t - 1) * 12 + (12 - m + 1)] <- weight[m, t]
      # For each year of observations which has 5 previous years of
      # observations, compute the weighted precipitation variable.
      for (i in Nlag:Nyrs) {
        # Antecedent rainfall is the monthly rainfall
        # multiplied by the normalised monthly weight
        antX1[i, m, t] <- weight[m, t] * ppt[i - t + 1, m]
      }
    }
    # Sum the monthly weights for each lag year
    sumD1[t] <- sum(delta[, t])
  }
  
  # Sum the yearly weights to get the total of all weights from over the
  # lag period considered
  sumD <- sum(sumD1[])
  
  # Compute the cumulative monthly weights:
  for (t in 1:(12 * Nlag)) {
    cum.weight[t] <- sum(weightOrdered[1:t])
  }
  
  # Compute antecedent precipitation by summing the weighted precipitation
  # variable over months and past years:
  for (i in Nlag:Nyrs) {
    for (t in 1:Nlag) {
      # antecedent rainfall in each lagged year t is the sum of
      # the monthly rainfall
      ant.sum1[i, t] <- sum(antX1[i, , t])
    }
    # antecedent rainfall in year i is the sum of the rainfall over
    # the previous 5 years
    antX[i] <- sum(ant.sum1[i, ])
  }
  
  # Specify the sd of the ANPP likelihood
  
  # Uniform distribution for the prior of the standard deviation of the
  # observations
  sigma ~ dunif(0, 100)
  # Turn standard deviation into a precision estimate, which is the sd of
  # the modelled ANPP
  tau <- 1 / (sigma ^ 2)
  
  # Define model for latent (mean) ANPP;
  # Event[,k] represents the amount of precipitation in the current growing
  # year received in different size classes, where k indexes the event size
  # class
  # (k=1 for < 5 mm; k=2 for 5-15 mm; k=3 for 15-30 mm; k=4 for >30 mm);
  for (i in Nlag:N) {
    # Calculate mu, the mean of the distribution of ANPP
    # (convert antecedent precipitation (antX) from inches to mm.)
    mu[i] <- a[1] + a[2] * antX[YearID[i]]
    # Likelihood for observed ANPP - it is a normal distribution with
    # mean mu and sd tau
    ANPP[i] ~ dnorm(mu[i], tau)
    # This is replicated to assess the model fit - I don't get this
    # it's run through twice to see whether the fit changes?
    ANPP.rep[i] ~ dnorm(mu[i], tau)
  }
  
  
  # Compute the month within year weights (alphas = wP,m in Box 1 in
  # main text); that is, these weights sum to 1 within each past year
  # I.E. alpha[m,t] gives the importance of month m's contribution to the
  # contribution of year t, ignoring the weight of t's contribution
  for (m in 1:12) {
    for (t in 1:Nlag) {
      alpha[m, t] <- delta[m, t] / sum(delta[, t])
    }
  }
}