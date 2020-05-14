## Model Code

# SAM model for replicating Ogle et al, 2015

# We are looking into the effects of antecedent conditions on ANPP
# namely precipitation received over the past five years and then the amount
# of precipitation received in the growing year within 4 classifications of
# rainfall events based on intensity

model{
  
  # Assign priors to the ANPP regression parameters
  # These are very precisely zero i.e. mean of zero and very low variance
  for (k in 1:3) {
    a[k] ~ dnorm(0, 0.0000001)
  }
  
  
  # TEMPERATURE
  
  # Assign prior for the weight for each time block of prior temperature
  # Ogle uses Dirichlet priors for the weights of time block temperature.
  # This is coded as a gamma distribution due to some reason relating to R
  for (j in 1:Nblocks) {
    deltaX_T[j] ~ dgamma(1, 1)
  }
  
  # Compute the monthly weights for the prior temperature
  for (t in 1:Nlag) {
    for (m in 1:12) {
      # weight for temperature experienced 
      delta_T[m, t] <- (deltaX_T[block[t, m]]) * (1 - equals(t, 1) * step(m - 9.5))
      # normalise the monthly weights (sumD is defined below)
      weight_T[m, t] <- delta_T[m, t] / sumD_T
      # Reorder the weights in order of "recentness" so that
      # they run from Dec of current year, Nov,... through to Feb,
      # Jan of oldest year as a vector rather than a matrix
      weightOrdered_T[(t - 1) * 12 + (12 - m + 1)] <- weight_T[m, t]
      # For each year of observations which has Nlag previous years of
      # observations, compute the weighted temperature variable.
      for (i in Nlag:Nyrs) {
        # Antecedent temperature is the monthly temperature
        # multiplied by the normalised monthly weight
        antX1_T[i, m, t] <- weight_T[m, t] * Tair[i - t + 1, m]
      }
    }
    # Sum the monthly weights for each lag year
    sumD1_T[t] <- sum(delta_T[, t])
  }
  
  # Sum the yearly weights to get the total of all weights from over the
  # lag period considered
  sumD_T <- sum(sumD1_T[])
  
  # Compute the cumulative monthly weights:
  for (t in 1:(12 * Nlag)) {
    cum.weight_T[t] <- sum(weightOrdered_T[1:t])
  }
  
  # Compute antecedent temperature by taking the mean of the weighted temperature
  # variable over months and past years:
  for (i in Nlag:Nyrs) {
    for (t in 1:Nlag) {
      # antecedent temperature in each lagged year t is the mean of
      # the monthly temperature
      ant.sum1_T[i, t] <- mean(antX1_T[i, , t])
    }
    # antecedent temperature in year i is the mean of the temperature over
    # the previous 5 years
    antX_T[i] <- mean(ant.sum1_T[i, ])
  }
  
  
# PRECIPITATION
  
  # Assign prior for the weight for each time block of prior precipitation
  # Ogle uses Dirichlet priors for the weights of time block precipitation.
  # This is coded as a gamma distribution due to some reason relating to R
  for (j in 1:Nblocks) {
    deltaX_P[j] ~ dgamma(1, 1)
  }
  
  # Compute the monthly weights for the prior precipitation
  for (t in 1:Nlag) {
    for (m in 1:12) {
      # weight for precipitation received after the ANPP harvest for
      # the current year is 0 (Oct,Nov,Dec)
      delta_P[m, t] <- (deltaX_P[block[t, m]]) * (1 - equals(t, 1) * step(m - 9.5))
      # normalise the monthly weights (sumD is defined below)
      weight_P[m, t] <- delta_P[m, t] / sumD_P
      # Reorder the weights in order of "recentness" so that
      # they run from Dec of current year, Nov,... through to Feb,
      # Jan of oldest year as a vector rather than a matrix
      weightOrdered_P[(t - 1) * 12 + (12 - m + 1)] <- weight_P[m, t]
      # For each year of observations which has 5 previous years of
      # observations, compute the weighted precipitation variable.
      for (i in Nlag:Nyrs) {
        # Antecedent rainfall is the monthly rainfall
        # multiplied by the normalised monthly weight
        antX1_P[i, m, t] <- weight_P[m, t] * ppt[i - t + 1, m]
      }
    }
    # Sum the monthly weights for each lag year
    sumD1_P[t] <- sum(delta_P[, t])
  }
  
  # Sum the yearly weights to get the total of all weights from over the
  # lag period considered
  sumD_P <- sum(sumD1_P[])
  
  # Compute the cumulative monthly weights:
  for (t in 1:(12 * Nlag)) {
    cum.weight_P[t] <- sum(weightOrdered_P[1:t])
  }
  
  # Compute antecedent precipitation by summing the weighted precipitation
  # variable over months and past years:
  for (i in Nlag:Nyrs) {
    for (t in 1:Nlag) {
      # antecedent rainfall in each lagged year t is the sum of
      # the monthly rainfall
      ant.sum1_P[i, t] <- sum(antX1_P[i, , t])
    }
    # antecedent rainfall in year i is the sum of the rainfall over
    # the previous 5 years
    antX_P[i] <- sum(ant.sum1_P[i, ])
  }
  
  
  
  
  
  
  # COMBINE INTO ANPP
  
  
  # Specify the sd of the ANPP likelihood
  
  # Uniform distribution for the prior of the standard deviation of the
  # observations
  sigma ~ dunif(0, 100)
  # Turn standard deviation into a precision estimate, which is the sd of
  # the modelled ANPP
  tau <- 1 / (sigma ^ 2)
  
  # Define model for latent (mean) ANPP;

  for (i in 1:N) {
    # Calculate mu, the mean of the distribution of ANPP
    mu[i] <- a[1] + a[2] * antX_P[YearID[i]] + a[3] * antX_T[YearID[i]]
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
      alpha_P[m, t] <- delta_P[m, t] / sum(delta_P[, t])
      alpha_T[m, t] <- delta_T[m, t] / sum(delta_T[, t])
    }
  }
}