library(tidyverse)
library(rstan)
library(here)
library(shinystan)

options(mc.cores = 4)

## Determining whether the model can parse out nested error structures.
## By some miracle this works. Last step is to add in the gamma distribution

# Known ground locations
known_ground <- 13000
measurement_error <- 50

known_ground_df <- tibble(HAT = rnorm(n=known_ground, mean=0, sd=measurement_error), HAT_index = rep(1, known_ground))

#unknown flight locations

unknown_flight <- 2500
unknown_ground <- 2500
mu_flight <- 1000
sigma_flight <- 50

unknown_flight_df <- tibble(HAT = rnorm(unknown_flight, mean=mu_flight, sd=sigma_flight), HAT_index = rep(0, unknown_flight))
unknown_flight_df$HAT <- unknown_flight_df$HAT %>%
  map(.f = function(x){ #incorporate measurement error
    rnorm(n = 1, mean = x, sd = measurement_error)
  }) %>%
  unlist()

unknown_ground_df <- tibble(HAT = rnorm(n=unknown_ground, mean=0, sd=measurement_error), HAT_index = rep(0, unknown_ground))
unknown_df <- bind_rows(unknown_flight_df, unknown_ground_df)

model_compiled <- stan_model(here("bayesian_modeling", "stan", "stan_attempt_6.stan"))

init <- function(){list(mu_bias = rnorm(1,0,10),
                        sigma_error = runif(1,40,60),
                        mu_flight = rnorm(1,1000,10),
                        sigma_flight = runif(1,40,60))}

fit <- sampling(model_compiled, data = list(n_obs_known = nrow(known_ground_df),
                                            HAT_known = known_ground_df$HAT,
                                            n_obs_unknown = nrow(unknown_df),
                                            HAT_unknown = unknown_df$HAT), 
                init = init,
                pars = c("mu_bias", "sigma_error", "mu_flight", "sigma_flight", "pState"),
                iter = 2000, 
                chains = 4)

print(fit)
#launch_shinystan(fit)

# fit@sim$samples[[1]][["pState[1]"]] %>%
#   median()

# fit@sim$samples[[1]][["pState[5000]"]] %>%
#   median()
