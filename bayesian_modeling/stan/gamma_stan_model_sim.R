library(tidyverse)
library(rstan)
library(truncnorm)
library(shinystan)
library(here)

options(mc.cores = 4)

known_ground <- 13000
unknown_flight <- 150*10
known_flight <- 0 # 150
unknown_ground <- (430-150)*10
nsim <- known_ground + unknown_flight + known_flight + unknown_ground

shape <- 1.2470108
rate <- 7.8222304
mean <- shape/rate
sd <- (shape/(rate^2))^.5

measurement_error <- 50/2183.475 #m

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

HAT_known <- dt %>% 
  filter(HAT_index == 1) %>% 
  pull(HAT)

HAT_unknown <- dt %>% 
  filter(is.na(HAT_index)) %>% 
  pull(HAT)

pars <- c("mu_bias", "sigma_error", "shape", "rate")

init <- function(){list(mu_bias = rnorm(1,0,1),
                        sigma_error = runif(1,0,1),
                        real_alt = runif(length(HAT_unknown),0,1),
                        shape = runif(1,0,5), 
                        rate = runif(1,0,10)
)}

# shape = runif(1,0,5), 
# rate = runif(1,0,10)

data_stan <- list(
  n_obs_known = length(HAT_known),
  n_obs_unknown = length(HAT_unknown),
  HAT_known = HAT_known,
  HAT_unknown = HAT_unknown
)

model_compiled <- stan_model(here("bayesian_modeling", "gamma_stan_model_sim.stan"))

fit <- sampling(model_compiled, 
                data = data_stan, 
                init = init, 
                pars = pars,
                iter = 1000, 
                warmup = 500,
                chains = 4)
print(fit)

launch_shinystan(fit)
