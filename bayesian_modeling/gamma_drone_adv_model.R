library(here)
sink(here("bayesian_modeling", "gamma_drone_adv_model.jags"))
cat("model{

    ### woodcock model
    ## likelihood
    for (i in 1:n_obs){
    HAT[i] ~ dnorm(mu_observed[i], tau_error_combined) # adding gps-specific error
    mu_observed[i] <- mu_bias_terrain + mu_bias_transmitter + (HAT_index[i]-1)*real_alt[i] # on average, the observed altitude should be the real altitude + the average bias
    real_alt[i] ~ dgamma(shape_flight, rate_flight) # distribution of real flight altitudes
    }
    
    ### drone model
    for(y in 1:drone_n){
    drone_hat[y] ~ dnorm(mu_bias_transmitter, tau_error_transmiter)
    }
    
    ## priors
    for (k in 1:n_obs){
    HAT_index[k] ~ dcat(c(0.6511628, 0.3488372)) # HAT_index can be either 1 or 2
    }
   
    # measurement error
    mu_bias_terrain ~ dnorm(0, 1) #negative values allowed
    mu_bias_transmitter ~ dnorm(0, 1) #negative values allowed
    
    tau_error_combined <- pow(variance_error_combined, -1) #precision
    variance_error_combined <- variance_error_terrain + variance_error_transmitter
    
    variance_error_terrain <- pow(sigma_error_terrain, 2)
    sigma_error_terrain ~ dnorm(0, 1) T(0,) #standard deviation
    
    tau_error_transmiter <- pow(variance_error_transmitter, -1)
    variance_error_transmitter <- pow(sigma_error_transmitter, 2)
    sigma_error_transmitter ~ dnorm(0, 1) T(0,) #standard deviation

    shape_flight ~ dnorm(0, prec_shape) T(0,)
    prec_shape <- pow(5, -2)
    rate_flight ~ dnorm(0, prec_rate) T(0,)
    prec_rate <- pow(10, -2)

}  
    ")
sink() 
