library(tidyverse)
library(sf)
library(suncalc)
library(here)
library(jagsUI)
library(truncnorm)

# importing drone data
drone_data <- read.csv(here("drone_data", "drone_data_corrected.csv"))

drone_data <- drone_data %>% 
  mutate(error_corrected = error/2183.475)

sink(here("bayesian_modeling", "gamma_droneonly_model.jags"))
cat("model{

    ### drone model
    for(y in 1:drone_n){
    drone_hat[y] ~ dnorm(mu_bias_flight, tau_error)
    }
    
    ## priors

    mu_bias_flight ~ dnorm(0, 1)
    
    # flight error
    tau_error <- pow(sigma_error, -2) #precision
    sigma_error ~ dnorm(0, 1) T(0,) #standard deviation

}  
    ")
sink() 

inits <- function(){list(mu_bias_flight = rnorm(1,0,1),
                         sigma_error = runif(1,0,1))}

parameters <- c("mu_bias_flight", "sigma_error") #

jags_data <- list(drone_hat = drone_data$error_corrected,
                  drone_n = nrow(drone_data))

# running jags
nc <- 3 # number of chains
ni <- 5000 # number of iterations
nb <- 2000 # burnin
nt <- 1 # thin rate (keeps every 5th iteration)

m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("bayesian_modeling", "gamma_droneonly_model.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=T)

print(m_test)

traceplot(m_test)

0.005*2183.475
