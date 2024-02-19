library(tidyverse)
library(jagsUI)
library(here)

known_ground <- 13000
unknown_flight <- 150
unknown_ground <- 280

# unknown_flight <- 150*50 #model converges with more samples
# unknown_ground <- 280*50

shape <- 1.2470108
rate <- 7.8222304
mean <- shape/rate
sd <- (shape/(rate^2))^.5

measurement_error <- 50/2183.475 #m

known_ground_df <- tibble(HAT = rnorm(n=known_ground, mean=0, sd=measurement_error), HAT_index = rep(1, known_ground))
unknown_ground_df <- tibble(HAT = rnorm(n=unknown_ground, mean=0, sd=measurement_error), HAT_index = rep(NA, unknown_ground))

unknown_flight_df <- tibble(HAT = rgamma(unknown_flight, shape = shape, rate = rate), HAT_index = rep(NA, unknown_flight))
unknown_flight_df$HAT <- map(unknown_flight_df$HAT, function(x){
  rnorm(n=1, mean = x, sd=measurement_error)
}) %>%
  unlist()

dt <- bind_rows(known_ground_df, unknown_ground_df, unknown_flight_df)

sink(here("bayesian_modeling", "simulation_gamma_scaled_repam.jags"))
cat("model{
   # likelihood
    for (i in 1:n_obs){
    HAT[i] ~ dnorm(mu_observed[i], tau_error) # adding gps-specific error
    mu_observed[i] <- mu_bias + (HAT_index[i]-1)*real_alt[i] # on average, the observed altitude should be the real altitude + the average bias
    real_alt[i] ~ dgamma(shape_flight, rate_flight) # distribution of real flight altitudes
    }
    
    #priors
    for (k in 1:n_obs){
    HAT_index[k] ~ dcat(c(0.67, 0.33)) # 1 is a ground location, 2 is a flight location.
    }
   
    mu_bias ~ dnorm(0, 1) #negative values allowed
    tau_error <- pow(sigma_error, -2) #precision
    sigma_error ~ dnorm(0, 1) T(0,) #standard deviation
   
    shape_flight ~ dnorm(0, prec_shape) T(0,)
    prec_shape <- pow(5, -2)
    rate_flight ~ dnorm(0, prec_rate) T(0,)
    prec_rate <- pow(10, -2)

}  
    ")
sink()

inits <- function(){list(mu_bias = rnorm(1,0,1),
                         sigma_error = runif(1,0,1),
                         shape_flight = runif(1,0,5), #
                         rate_flight = runif(1,0,10))}

parameters <- c("mu_bias", "sigma_error", "shape_flight", "rate_flight") #

jags_data <- list(HAT = dt$HAT,
                  n_obs = nrow(dt),
                  HAT_index = dt$HAT_index)
# running jags
nc <- 3 # number of chains
ni <- 5000 # number of iterations
nb <- 2000 # burnin
nt <- 1 # thin rate (keeps every 5th iteration)
m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("bayesian_modeling", "simulation_gamma_scaled_repam.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=T)

print(m_test)

traceplot(m_test)
