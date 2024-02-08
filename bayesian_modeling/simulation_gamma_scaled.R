library(tidyverse)
library(jagsUI)
library(truncnorm)
library(here)

known_ground <- 13000
unknown_flight <- 0
known_flight <- 150 # 150
unknown_ground <- 0 #430-150
nsim <- known_ground + unknown_flight + known_flight + unknown_ground

shape <- 1.2470108
rate <- 7.8222304
mean <- shape/rate
sd <- (shape/(rate^2))^.5

measurement_error <- 50/2183.475 #m

known_ground_df <- tibble(HAT = rnorm(n=known_ground, mean=0, sd=measurement_error), HAT_index = rep(1, known_ground))
unknown_ground_df <- tibble(HAT = rnorm(n=unknown_ground, mean=0, sd=measurement_error), HAT_index = rep(NA, unknown_ground))

# known_flight_df <- tibble(HAT = rtruncnorm(n=known_flight, mean=330, sd=250, a = 0), HAT_index = rep(2, known_flight))

known_flight_df <- tibble(HAT = rgamma(known_flight, shape = shape, rate = rate), HAT_index = rep(2, known_flight))
known_flight_df$HAT <- map(known_flight_df$HAT, function(x){
  rnorm(n=1, mean = x, sd=measurement_error)
}) %>%
  unlist()

# unknown_flight_df <- tibble(HAT = rtruncnorm(n=unknown_flight, mean=330, sd=250, a = 0), HAT_index = rep(NA, unknown_flight))
unknown_flight_df <- tibble(HAT = rgamma(unknown_flight, shape = shape, rate = rate), HAT_index = rep(NA, unknown_flight))
unknown_flight_df$HAT <- map(unknown_flight_df$HAT, function(x){
  rnorm(n=1, mean = x, sd=measurement_error)
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

sink(here("bayesian_modeling", "simulation_gamma.jags"))
cat("model{
   # likelihood
    for (i in 1:n_obs){
    HAT[i] ~ dnorm(mu_observed[i], tau_error) # adding gps-specific error
    mu_observed[i] <- mu_bias + (HAT_index[i]-1)*real_alt[i] # on average, the observed altitude should be the real altitude + the average bias
    real_alt[i] ~ dgamma(shape_flight, rate_flight) # distribution of real flight altitudes
    }
    
    # priors
    # for (k in 1:n_obs){
    # HAT_index[k] ~ dcat(c(0.50, 0.50)) # HAT_index can be either 1 or 2
    # }
   
    mu_bias ~ dnorm(0, 1) #negative values allowed
    tau_error <- pow(sigma_error, -2) #precision
    sigma_error ~ dnorm(0, 1) T(0,) #standard deviation
   
    shape_flight <- pow(mean_flight, 2) / pow(sd_flight, 2)
    rate_flight <- mean_flight / pow(sd_flight, 2)
   
    mean_flight ~ dnorm(0, prec_mean_flight) T(0,)
    prec_mean_flight <- pow(1, -2)
    sd_flight ~ dnorm(0, prec_sd_flight) T(0,)
    prec_sd_flight <- pow(1, -2)

}  
    ")
sink()

inits <- function(){list(mu_bias = rnorm(1,0,1),
                         sigma_error = runif(1,0,1),
                         mean_flight = runif(1,0,1), #Node inconsistent with parents
                         sd_flight = runif(1,0,1))}

parameters <- c("mu_bias", "sigma_error", "shape_flight", "rate_flight", "mean_flight", "sd_flight") #

jags_data <- list(HAT = dt$HAT,
                  n_obs = nrow(dt),
                  HAT_index = dt$HAT_index)
# running jags
nc <- 3 # number of chains
ni <- 5000 # number of iterations
nb <- 2000 # burnin
nt <- 1 # thin rate (keeps every 5th iteration)

m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("bayesian_modeling", "simulation_gamma.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=T)

print(m_test)

traceplot(m_test)
