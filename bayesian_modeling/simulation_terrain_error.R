library(tidyverse)
library(jagsUI)
library(truncnorm)
library(here)

#working on the real sample size, although it probably needs to run at 150-200k iterations to fully converge

known_ground <- 13000
unknown_flight <- 150*10 #150
known_flight <- 0 # 150
unknown_ground <- (430-150)*10 #430-150
nsim <- known_ground + unknown_flight + known_flight + unknown_ground

ground_test_n <- 50 # 50
flight_test_n <- 50 # 50

shape <- 1.2470108
rate <- 7.8222304
mean <- shape/rate
sd <- (shape/(rate^2))^.5

measurement_error <- 50/2183.475 #m

ground_test_mean <- -2/2183.475
ground_test_sd <- 17/2183.475

flight_test_mean <- -2/2183.475
flight_test_sd <- 10/2183.475

# simulating field data

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

# simulating test data- assuming deviations from a known "true" altitude
ground_test <- tibble(error = rnorm(ground_test_n, mean = ground_test_mean, sd = ground_test_sd))
flight_test <- tibble(error = rnorm(flight_test_n, mean = flight_test_mean, sd = flight_test_sd))

# dt %>%
#   filter(is.na(HAT_index)) %>%
#   pull(HAT) %>%
#   density() %>%
#   plot()

sink(here("bayesian_modeling", "simulation_terrain_error.jags"))
cat("model{
    # field model
    for (i in 1:n_obs){
    HAT[i] ~ dnorm(mu_observed[i], tau_combined_error[i]) # adding gps-specific error
    mu_observed[i] <- mu_GPS_bias[HAT_index[i]] + mu_terrain_bias + (HAT_index[i]-1)*real_alt[i] # on average, the observed altitude should be the real altitude + the average bias
    real_alt[i] ~ dgamma(shape_flight, rate_flight) # distribution of real flight altitudes
    }
    
    # test model
    for (y in 1:ground_test_n){
    ground_test[y] ~ dnorm(mu_GPS_bias[1], tau_GPS_error[1])
    }
    
    for (z in 1:flight_test_n){
    flight_test[z] ~ dnorm(mu_GPS_bias[2], tau_GPS_error[2])
    }
    
    # priors
    for (k in 1:n_obs){
    HAT_index[k] ~ dcat(c(0.6511628, 0.3488372)) # HAT_index can be either 1 or 2
    }
    
    for (i in 1:n_obs){
    tau_combined_error[i] <- pow(variance_combined_error[i], -1)
    variance_combined_error[i] <- variance_terrain_error + variance_GPS_error[HAT_index[i]]
    }
    
    mu_terrain_bias ~ dnorm(0, 1) #negative values allowed
    variance_terrain_error <- pow(sigma_terrain_error, 2)
    sigma_terrain_error ~ dnorm(0, 1) T(0,) #standard deviation
    
    mu_GPS_bias[1] ~ dnorm(0, 1) #negative values allowed
    tau_GPS_error[1] <- pow(variance_GPS_error[1], -1) #precision
    variance_GPS_error[1] <- pow(sigma_GPS_error[1], 2)
    sigma_GPS_error[1] ~ dnorm(0, 1) T(0,) #standard deviation
    
    mu_GPS_bias[2] ~ dnorm(0, 1) #negative values allowed
    tau_GPS_error[2] <- pow(variance_GPS_error[2], -1) #precision
    variance_GPS_error[2] <- pow(sigma_GPS_error[2], 2)
    sigma_GPS_error[2] ~ dnorm(0, 1) T(0,) #standard deviation
   
    shape_flight ~ dnorm(0, prec_shape) T(0,)
    prec_shape <- pow(5, -2)
    rate_flight ~ dnorm(0, prec_rate) T(0,)
    prec_rate <- pow(10, -2)

}  
    ")
sink()

inits <- function(){list(mu_GPS_bias = rnorm(2,0,1),
                         mu_terrain_bias = rnorm(1,0,1),
                         sigma_GPS_error = runif(2,0,1),
                         sigma_terrain_error = runif(1,0,1),
                         shape_flight = runif(1,0,5), #
                         rate_flight = runif(1,0,10))}

parameters <- c("mu_GPS_bias", "mu_terrain_bias", "sigma_GPS_error", 
                "sigma_terrain_error", "shape_flight", "rate_flight") #

jags_data <- list(HAT = dt$HAT,
                  n_obs = nrow(dt),
                  HAT_index = dt$HAT_index,
                  ground_test_n = ground_test_n,
                  flight_test_n = flight_test_n,
                  ground_test = ground_test$error,
                  flight_test = flight_test$error)

# running jags
nc <- 3 # number of chains
ni <- 80000 #100000 # number of iterations
nb <- 10000 #10000 # burnin
nt <- 5 # thin rate (keeps every 5th iteration)

m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("bayesian_modeling", "simulation_terrain_error.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=T)

print(m_test)

traceplot(m_test)
