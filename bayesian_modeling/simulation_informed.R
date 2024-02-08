library(tidyverse)
library(jagsUI)
library(truncnorm)
library(here)

known_ground <- 13000
unknown_flight <- 220*2
known_flight <- 0
unknown_ground <- 220*2
nsim <- known_ground + unknown_flight + known_flight + unknown_ground

known_ground_df <- tibble(HAT = rnorm(n=known_ground, mean=0, sd=10), HAT_index = rep(1, known_ground))
unknown_ground_df <- tibble(HAT = rnorm(n=unknown_ground, mean=0, sd=10), HAT_index = rep(NA, unknown_ground))

# known_flight_df <- tibble(HAT = rtruncnorm(n=known_flight, mean=330, sd=250, a = 0), HAT_index = rep(2, known_flight))

known_flight_df <- tibble(HAT = rnorm(n=known_flight, mean=330, sd=250), HAT_index = rep(2, known_flight))
known_flight_df$HAT <- map(known_flight_df$HAT, function(x){
  rnorm(n=1, mean = x, sd=10)
}) %>%
  unlist()

# unknown_flight_df <- tibble(HAT = rtruncnorm(n=unknown_flight, mean=330, sd=250, a = 0), HAT_index = rep(NA, unknown_flight))
unknown_flight_df <- tibble(HAT = rnorm(n=unknown_flight, mean=330, sd=250), HAT_index = rep(NA, unknown_flight))
unknown_flight_df$HAT <- map(unknown_flight_df$HAT, function(x){
  rnorm(n=1, mean = x, sd=10)
}) %>%
  unlist()

dt <- bind_rows(known_ground_df, unknown_ground_df, known_flight_df, unknown_flight_df)

# dt <- data.frame(HAT=rep(NA, nsim), HAT_index=rep(NA, nsim))
# dt$HAT_index <- c(rep(1, known_ground), rep(NA, unknown_flight), rep(NA, unknown_ground))
# dt$HAT[1:known_ground] <- rnorm(n=known_ground, mean=0, sd=10)
# dt$HAT[(known_ground+1):(known_ground+unknown_flight)] <- rnorm(n=unknown_flight, mean=330, sd=250)
# dt$HAT[(known_ground+unknown_flight+1):nsim] <- rnorm(n=unknown_ground, mean=0, sd=10)

hist(dt$HAT)

# dt %>%
#   filter(is.na(HAT_index)) %>%
#   pull(HAT) %>%
#   density() %>%
#   plot()

sink(here("bayesian_modeling", "simulation_informed.jags"))
cat("model{
   # likelihood
    for (i in 1:n_obs){
    HAT[i] ~ dnorm(mu_observed[i], tau_error) # adding gps-specific error
    mu_observed[i] <- mu_bias + (HAT_index[i]-1)*real_alt[i] # on average, the observed altitude should be the real altitude + the average bias
    real_alt[i] ~ dnorm(mu_flight, tau_flight) # distribution of real flight altitudes
    }
    
    #priors
    for (k in 1:n_obs){
    HAT_index[k] ~ dcat(c(0.50, 0.50)) # HAT_index can be either 1 or 2
    }
   
    mu_bias ~ dnorm(0, 0.001) #negative values allowed
    mu_flight ~ dnorm(0, 0.000001) T(0,) #negative values not allowed
    # mu_flight ~ dunif(0, 2500) #negative values not allowed

    tau_error <- pow(sigma_error, -2) #precision
    tau_flight <- pow(sigma_flight, -2) #precision
    
    sigma_error ~ dunif(0, 2500) #standard deviation
    sigma_flight ~ dnorm(0, 0.000001) T(0,) #standard deviation
    # sigma_flight ~ dunif(0, 2500) #standard deviation
}  
    
    ")
sink() 

inits <- function(){list(mu_bias=rnorm(1,0,1),
                         mu_flight=runif(1,0,500),
                         sigma_error=runif(1,0,100),
                         sigma_flight=runif(1,0,200))}

parameters <- c("mu_bias", "sigma_error", "mu_flight", "sigma_flight") #~ dnorm(0, 0.000001)

jags_data <- list(HAT = dt$HAT,
                  n_obs = nrow(dt),
                  HAT_index = dt$HAT_index)
# running jags
nc <- 3 # number of chains
ni <- 5000 # number of iterations
nb <- 2000 # burnin
nt <- 1 # thin rate (keeps every 5th iteration)

m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("bayesian_modeling", "simulation_informed.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=T)

print(m_test)

traceplot(m_test)
