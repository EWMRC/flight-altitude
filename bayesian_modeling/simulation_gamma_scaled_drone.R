library(tidyverse)
library(jagsUI)
library(truncnorm)
library(here)

known_ground <- 13000
unknown_flight <- 150
known_flight <- 0 # 150
unknown_ground <- 430-150
nsim <- known_ground + unknown_flight + known_flight + unknown_ground

drone_n <- 50

shape <- 1.2470108
rate <- 7.8222304
mean <- shape/rate
sd <- (shape/(rate^2))^.5

ground_error <- 50/2183.475 #m
flight_error <- 10/2183.475 #m

known_ground_df <- tibble(HAT = rnorm(n=known_ground, mean=0, sd=ground_error), HAT_index = rep(1, known_ground))
unknown_ground_df <- tibble(HAT = rnorm(n=unknown_ground, mean=0, sd=ground_error), HAT_index = rep(NA, unknown_ground))

# known_flight_df <- tibble(HAT = rtruncnorm(n=known_flight, mean=330, sd=250, a = 0), HAT_index = rep(2, known_flight))

known_flight_df <- tibble(HAT = rgamma(known_flight, shape = shape, rate = rate), HAT_index = rep(2, known_flight))
known_flight_df$HAT <- map(known_flight_df$HAT, function(x){
  rnorm(n=1, mean = x, sd=flight_error)
}) %>%
  unlist()

# unknown_flight_df <- tibble(HAT = rtruncnorm(n=unknown_flight, mean=330, sd=250, a = 0), HAT_index = rep(NA, unknown_flight))
unknown_flight_df <- tibble(HAT = rgamma(unknown_flight, shape = shape, rate = rate), HAT_index = rep(NA, unknown_flight))
unknown_flight_df$HAT <- map(unknown_flight_df$HAT, function(x){
  rnorm(n=1, mean = x, sd=flight_error)
}) %>%
  unlist()

dt <- bind_rows(known_ground_df, unknown_ground_df, known_flight_df, unknown_flight_df)

drone_data <- tibble(drone_hat = rnorm(n=drone_n, mean=0, sd=flight_error) )

sink(here("bayesian_modeling", "simulation_gamma_repam2.jags"))
cat("model{

    ### woodcock model
    ## likelihood
    for (i in 1:n_obs){
    HAT[i] ~ dnorm(mu_observed[i], tau_error[HAT_index[i]]) # adding gps-specific error
    mu_observed[i] <- mu_bias_ground + (HAT_index[i]-1)*mu_bias_add + (HAT_index[i]-1)*real_alt[i] # on average, the observed altitude should be the real altitude + the average bias
    real_alt[i] ~ dgamma(shape_flight, rate_flight) # distribution of real flight altitudes
    }
    
    ## priors
    for (k in 1:n_obs){
    HAT_index[k] ~ dcat(c(0.50, 0.50)) # HAT_index can be either 1 or 2
    }
   
    mu_bias_ground ~ dnorm(0, 1) #negative values allowed
    mu_bias_add ~ dnorm(0, 1) #negative values allowed
    mu_bias_flight <- mu_bias_ground + mu_bias_add
    
    # ground error
    tau_error[1] <- pow(sigma_error[1], -2) #precision
    sigma_error[1] ~ dnorm(0, 1) T(0,) #standard deviation
    
    # flight error
    tau_error[2] <- pow(sigma_error[2], -2) #precision
    sigma_error[2] ~ dnorm(0, 1) T(0,) #standard deviation
   
    shape_flight ~ dnorm(0, prec_shape) T(0,)
    prec_shape <- pow(5, -2)
    rate_flight ~ dnorm(0, prec_rate) T(0,)
    prec_rate <- pow(10, -2)
    
    ### drone model (shared priors with woodcock model)
    for(y in 1:drone_n){
    drone_hat[y] ~ dnorm(mu_bias_flight, tau_error[2])
    }
}  
    ")
sink()

inits <- function(){list(mu_bias_ground = rnorm(1,0,1),
                         mu_bias_add = rnorm(1,0,1),
                         sigma_error = runif(2,0,1),
                         shape_flight = runif(1,0,5), #
                         rate_flight = runif(1,0,10))}

parameters <- c("mu_bias", "mu_bias_add", "mu_bias_flight", 
                "sigma_error", "shape_flight", "rate_flight") #

jags_data <- list(HAT = dt$HAT,
                  n_obs = nrow(dt),
                  HAT_index = dt$HAT_index,
                  drone_hat = drone_data$drone_hat,
                  drone_n = drone_n)
# running jags
nc <- 3 # number of chains
ni <- 20000 # number of iterations
nb <- 10000 # burnin
nt <- 1 # thin rate (keeps every 5th iteration)

m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("bayesian_modeling", "simulation_gamma_repam2.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=T)

print(m_test)

traceplot(m_test)
