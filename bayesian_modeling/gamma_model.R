library(here)

sink(here("bayesian_modeling", "gamma_model.jags"))
cat("model{
   # likelihood
    for (i in 1:n_obs){
    HAT[i] ~ dnorm(mu_observed[i], tau_error) # adding gps-specific error
    mu_observed[i] <- mu_bias + (HAT_index[i]-1)*real_alt[i] # on average, the observed altitude should be the real altitude + the average bias
    real_alt[i] ~ dgamma(shape_flight, rate_flight) # distribution of real flight altitudes
    }
    
    #priors
    for (k in 1:n_obs){
    HAT_index[k] ~ dcat(c(0.67, 0.33)) # HAT_index can be either 1 or 2
    }
   
    mu_bias ~ dnorm(0, 1) #negative values allowed
    tau_error <- pow(sigma_error, -2) #precision
    sigma_error ~ dnorm(0, 1) T(0,) #standard deviation
   
    shape_flight ~ dnorm(0, 0.04) T(0,) #sd 5
    # prec_shape <- pow(5, -2)
    rate_flight ~ dnorm(0, 0.01) T(0,) #sd 10
    # prec_rate <- pow(10, -2)

}  
    ")
sink() 
