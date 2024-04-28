library(tidyverse)
library(rstan)
library(here)
library(shinystan)

options(mc.cores = 4)

## Now subbing in a gamma distribution for the normal dist we've been using

# Known ground locations
known_ground <- 13000
measurement_error <- 50/2200

known_ground_df <- tibble(HAT = rnorm(n=known_ground, mean=0, sd=measurement_error), HAT_index = rep(1, known_ground))

#unknown flight locations

unknown_flight <- 165 #2500
unknown_ground <- 335 #2500
# mu_flight <- 1000/2200
# sigma_flight <- 50/2200

shape <- 1.25
rate <- 7.82

unknown_flight_df <- tibble(HAT = rgamma(unknown_flight, shape = shape, rate = rate), HAT_index = rep(0, unknown_flight))
unknown_flight_df$HAT <- unknown_flight_df$HAT %>%
  map(.f = function(x){ #incorporate measurement error
    rnorm(n = 1, mean = x, sd = measurement_error)
  }) %>%
  unlist()

unknown_ground_df <- tibble(HAT = rnorm(n=unknown_ground, mean=0, sd=measurement_error), HAT_index = rep(0, unknown_ground))
unknown_df <- bind_rows(unknown_flight_df, unknown_ground_df)

model_compiled <- stan_model(here("bayesian_modeling", "stan", "stan_attempt_7.stan"))

init <- function(){list(mu_bias = rnorm(1,0,0.2),
                        sigma_error = runif(1,0,0.2),
                        shape = runif(1,3,5),
                        rate = runif(1,5,10))}

fit <- sampling(model_compiled, data = list(n_obs_known = nrow(known_ground_df),
                                            HAT_known = known_ground_df$HAT,
                                            n_obs_unknown = nrow(unknown_df),
                                            HAT_unknown = unknown_df$HAT), 
                init = init,
                pars = c("mu_bias", "sigma_error", "shape", "rate", "pState"),
                iter = 15000, #just bumping up the ESS here- converges on as few as 2000 iter
                chains = 4)

print(fit)
launch_shinystan(fit)

# fit@sim$samples[[1]][["pState[1]"]] %>%
#   median()

# fit@sim$samples[[1]][["pState[500]"]] %>%
#   median()
