library(here)

sink(here("bayesian_modeling", "gamma_age_model.jags"))
cat("model{
   # likelihood
    for (i in 1:n_obs){
    HAT[i] ~ dnorm(mu_observed[i], tau_error) # adding gps-specific error
    mu_observed[i] <- mu_bias + (HAT_index[i]-1)*real_alt[i] # on average, the observed altitude should be the real altitude + the average bias
    real_alt[i] ~ dgamma(shape_flight[age[i]], rate_flight[age[i]]) # distribution of real flight altitudes
    }
    
    #priors
    for (k in 1:n_obs){
    HAT_index[k] ~ dcat(c(0.67, 0.33)) # HAT_index can be either 1 or 2
    }
   
    mu_bias ~ dnorm(0, 1) #negative values allowed
    tau_error <- pow(sigma_error, -2) #precision
    sigma_error ~ dnorm(0, 1) T(0,) #standard deviation
   
    shape_flight[1] ~ dnorm(0, 0.04) T(0,) #sd 5
    shape_flight[2] ~ dnorm(0, 0.04) T(0,)

    rate_flight[1] ~ dnorm(0, 0.01) T(0,) #sd 10
    rate_flight[2] ~ dnorm(0, 0.01) T(0,)
   
   # derived parameters
   mean_HAT_index <- mean(HAT_index)
   
   for (i in 1:n_obs){
   HAT_vect_1[i] <- ifelse(age[i]==1, HAT_index[i] - 1, 0)
   HAT_vect_2[i] <- ifelse(age[i]==2, HAT_index[i] - 1, 0)
   }
   age_1_ss <- sum(HAT_vect_1)
   age_2_ss <- sum(HAT_vect_2)
}  
    ")
sink() 
