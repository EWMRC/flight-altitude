library(tidyverse)
library(jagsUI)
library(truncnorm)
library(here)

known_ground <- 0
unknown_flight <- 0
known_flight <- 300 # 150
unknown_ground <- 0
nsim <- known_ground + unknown_flight + known_flight + unknown_ground

known_ground_df <- tibble(HAT = rnorm(n=known_ground, mean=0, sd=50), HAT_index = rep(1, known_ground))
unknown_ground_df <- tibble(HAT = rnorm(n=unknown_ground, mean=0, sd=50), HAT_index = rep(NA, unknown_ground))

# known_flight_df <- tibble(HAT = rtruncnorm(n=known_flight, mean=330, sd=250, a = 0), HAT_index = rep(2, known_flight))

known_flight_df <- tibble(HAT = rgamma(known_flight, shape = 1.247259, rate = 0.003583134), HAT_index = rep(2, known_flight))
# known_flight_df$HAT <- map(known_flight_df$HAT, function(x){
#   rnorm(n=1, mean = x, sd=50)
# }) %>%
#   unlist()

# unknown_flight_df <- tibble(HAT = rtruncnorm(n=unknown_flight, mean=330, sd=250, a = 0), HAT_index = rep(NA, unknown_flight))
unknown_flight_df <- tibble(HAT = rgamma(unknown_flight, shape = 1.247259, rate = 0.003583134), HAT_index = rep(NA, unknown_flight))
# unknown_flight_df$HAT <- map(unknown_flight_df$HAT, function(x){
#   rnorm(n=1, mean = x, sd=50)
# }) %>%
#   unlist()

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

sink(here("bayesian_modeling", "simulation_gamma_simple.jags"))
cat("model{
   # likelihood
    for (i in 1:n_obs){
    HAT[i] ~ dgamma(shape_flight, rate_flight) # distribution of real flight altitudes
    }

    # priors (see https://doingbayesiandataanalysis.blogspot.com/2012/08/gamma-likelihood-parameterized-by-mode.html)
    shape_flight <- pow(mean_flight, 2) / pow(sd_flight, 2)
    rate_flight <- mean_flight / pow(sd_flight, 2)
   
    mean_flight ~ dnorm(348.0916, prec_mean_flight)
    prec_mean_flight <- pow(50, -2)
    sd_flight ~ dnorm(311.6845, prec_sd_flight)
    prec_sd_flight <- pow(50, -2)
    
}  
    ")
sink() 

inits <- function(){list(mean_flight = rtruncnorm(n = 1, a = 0, mean = 348.0916, sd = 50),
                         sd_flight = rtruncnorm(n = 1, a = 0, mean = 311.6845, sd = 50))}
# 1.247259/0.003583134
# (1.247259/(0.003583134^2))^.5

parameters <- c("shape_flight", "rate_flight", "mean_flight", "sd_flight") #

jags_data <- list(HAT = dt$HAT,
                  n_obs = nrow(dt))
# running jags
nc <- 3 # number of chains
ni <- 5000 # number of iterations
nb <- 2000 # burnin
nt <- 1 # thin rate (keeps every 5th iteration)

m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("bayesian_modeling", "simulation_gamma_simple.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=T)

print(m_test)

traceplot(m_test)
