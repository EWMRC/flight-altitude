library(tidyverse)
library(rstan)
library(here)

options(mc.cores = 4)

# testing ability to recognize a single discrete parameter
# (number of coin flips) while also estimating the weight of the coin

sample_size <- 5000
coin_flips_small <- tibble(flips = rbinom(n = sample_size, size = 20, prob = 0.7), size = 20)
coin_flips_big <- tibble(flips = rbinom(n = sample_size, size = 20, prob = 0.7), size = 20)

coin_flips <- bind_rows(coin_flips_small, coin_flips_big)

model_compiled <- stan_model(here("bayesian_modeling", "stan", "marginalize_coin_flips_2.stan"))

# init <- function(){list(
#   p = runif(1, 0, 1)
# )}

fit <- sampling(model_compiled, data = list(n_obs = nrow(coin_flips),
                                            flips = coin_flips$flips), 
                #init = init,
                iter = 2000, 
                chains = 4) #algorithm = "Fixed_param"

print(fit)
