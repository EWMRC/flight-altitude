library(tidyverse)
library(rstan)
library(here)

options(mc.cores = 4)

sample_size <- 5000
coin_flips <- tibble(flips = rbinom(n = 5000, size = 20, prob = 0.4))

model_compiled <- stan_model(here("bayesian_modeling", "stan", "marginalize_coin_flips_1.stan"))

init <- function(){list(p = runif(1,0,1))}

fit <- sampling(model_compiled, data = list(n_obs = nrow(coin_flips),
                                            flips = coin_flips$flips), 
                init = init, 
                iter = 2000, 
                chains = 4)

print(fit)
