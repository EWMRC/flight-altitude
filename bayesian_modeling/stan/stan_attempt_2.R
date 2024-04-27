library(tidyverse)
library(shinystan)
library(here)

options(mc.cores = 4)

## testing code to derive mu_bias and sigma_error from known ground locations

known_ground <- 13000
measurement_error <- 50/2183.475 #m

known_ground_df <- tibble(HAT = rnorm(n=known_ground, mean=0, sd=measurement_error), HAT_index = rep(1, known_ground))

hist(known_ground_df$HAT)

model_compiled <- stan_model(here("bayesian_modeling", "stan", "stan_attempt_2.stan"))

init <- function(){list(mu_bias = rnorm(1,0,1),
                        sigma_error = runif(1,0,1))}

fit <- sampling(model_compiled, data = list(n_obs = nrow(known_ground_df), HAT = known_ground_df$HAT), 
                init = init, 
                iter = 2000, 
                chains = 4)

print(fit)
