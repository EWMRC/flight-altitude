sink(here("bayesian_modeling", "height_cutoff_model.jags"))
cat("model{
   # likelihood
    for (i in 1:n_obs){
    HAT[i] ~ dnorm(mu_observed[i], tau_error) # adding gps-specific error
    mu_observed[i] <- mu_bias + (HAT_index[i]-1)*real_alt[i] # on average, the observed altitude should be the real altitude + the average bias
    real_alt[i] ~ dnorm(mu_flight, tau_flight) # distribution of real flight altitudes
    }
    
    # priors
    for (k in 1:n_obs){
    HAT_index[k] ~ dcat(c(0.50, 0.50)) # HAT_index can be either 1 or 2
    }
   
    mu_bias ~ dnorm(0, 0.001) #negative values allowed
    mu_flight ~ dnorm(330, 0.0001) T(0,) #negative values not allowed
    # mu_flight ~ dunif(0, 2500) #negative values not allowed

    tau_error <- pow(sigma_error, -2) #precision
    tau_flight <- pow(sigma_flight, -2) #precision
    
    sigma_error ~ dunif(0, 2500) #standard deviation
    sigma_flight ~ dnorm(330, 0.0001) T(0,) #standard deviation
    # sigma_flight ~ dunif(0, 2500) #standard deviation
}  
    
    ")
sink() 
