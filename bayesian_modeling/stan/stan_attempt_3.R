library(tidyverse)
library(shinystan)
library(here)

options(mc.cores = 4)

## testing code to derive flight parameters from known flight locations

known_flight <- 150 #eventually bring down to 150
shape <- 1.2470108
rate <- 7.8222304
mean <- shape/rate
sd <- (shape/(rate^2))^.5


known_flight_df <- tibble(HAT = rgamma(known_flight, shape = shape, rate = rate), HAT_index = rep(2, known_flight))

hist(known_flight_df$HAT)


model_compiled <- stan_model(here("bayesian_modeling", "stan", "stan_attempt_3.stan"))

init <- function(){list(shape = runif(1,0,5),
                        rate = runif(1,0,10))}

fit <- sampling(model_compiled, data = list(n_obs = nrow(known_flight_df), HAT = known_flight_df$HAT), 
                init = init, 
                iter = 5000, 
                chains = 4)

print(fit)
