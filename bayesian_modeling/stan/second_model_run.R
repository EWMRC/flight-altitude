#// described in https://www.youtube.com/watch?v=KOIudAB6vJ0&ab_channel=BenLambert

library(rstan)
library(here)

options(mc.cores = 4)

X <- c(2,4,3,3,3,2,3,3,4,4)
K <- length(X)

aModel <- stan_model(here("bayesian_modeling", "stan", "second_model.stan"))

fit <- sampling(aModel, data = list(X=X, K=K), iter = 200, chains = 4)
print(fit)
