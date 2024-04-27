library(tidyverse)
library(rstan)
library(shinystan)
library(here)

# generate fake data
N <- 100
y <- rnorm(N, 1.6, 0.2)
hist(y)

# compile model
model <- stan_model(here("bayesian_modeling", "stan", "first_model.stan"))

# pass data to stan and run model
options(mc.cores = 4)
fit <- sampling(model, list(N = N, y = y), iter = 200, chains = 4)

# diagnose (lp is log probability)
print(fit)

# graph
params <- rstan::extract(fit)
hist(params$mu)

launch_shinystan(fit)
