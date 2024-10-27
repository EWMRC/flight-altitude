library(tidyverse)
library(rstan)
library(here)
library(shinystan)
library(furrr)
plan(multisession)

# options(mc.cores = 4)
rstan_options(auto_write = TRUE)

model_compiled <- stan_model(here("bayesian_modeling", "stan_original_sim.stan"))

percentile_samples <- future_map(1:1000, function(i){
  # Known ground locations
  known_ground <- 13000
  measurement_error <- 50/2200
  
  known_ground_df <- tibble(HAT = rnorm(n=known_ground, mean=0, sd=measurement_error), HAT_index = rep(1, known_ground))
  
  #unknown flight locations
  
  unknown_flight <- 141 #141.24+22
  unknown_ground <- 287 #-120#2500
  # mu_flight <- 1000/2200
  # sigma_flight <- 50/2200
  
  shape <- 1.25
  rate <- 7.82
  
  unknown_flight_df <- tibble(HAT = rgamma(unknown_flight, shape = shape, rate = rate), HAT_index = rep(1, unknown_flight))
  # for comparison
  true_params <- unknown_flight_df$HAT %>% 
    MASS::fitdistr("gamma")
  
  unknown_flight_df$HAT <- unknown_flight_df$HAT %>%
    map(.f = function(x){ #incorporate measurement error
      rnorm(n = 1, mean = x, sd = measurement_error)
    }) %>%
    unlist()
  
  unknown_ground_df <- tibble(HAT = rnorm(n=unknown_ground, mean=0, sd=measurement_error), HAT_index = rep(0, unknown_ground))
  unknown_df <- bind_rows(unknown_flight_df, unknown_ground_df)
  
  init <- function(){list(mu_bias = rnorm(1,0,0.2),
                          sigma_error = runif(1,0,0.2),
                          shape = runif(1,3,5),
                          rate = runif(1,5,10),
                          flight_prior = 0.33)}
  
  fit <- sampling(model_compiled, data = list(n_obs_known = nrow(known_ground_df),
                                              HAT_known = known_ground_df$HAT,
                                              n_obs_unknown = nrow(unknown_df),
                                              HAT_unknown = unknown_df$HAT), 
                  init = init,
                  pars = c("mu_bias", "sigma_error", "shape", "rate", "flight_prior"),
                  iter = 10000, #just bumping up the ESS here- converges on as few as 2000 iter
                  seed = i,
                  verbose = FALSE,
                  show_messages = FALSE,
                  #control = list(adapt_delta = 0.99),
                  chains = 4)
  
  shape_tbl <- bayestestR::eti(extract(fit, "shape")$shape, ci = c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 0.99)) %>% 
    mutate(seed = i,
           true = true_params$estimate[["shape"]],
           type = "shape")
  rate_tbl <- bayestestR::eti(extract(fit, "rate")$rate, ci = c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 0.99)) %>% 
    mutate(seed = i,
           true = true_params$estimate[["rate"]],
           type = "rate")
  
  bind_rows(shape_tbl, rate_tbl) %>% 
    return()
  
  # percentile_shape <- ecdf(extract(fit, "shape")$shape)
  # percentile_rate <- ecdf(extract(fit, "rate")$rate)
  
  # return(tibble(shape_pctile = percentile_shape(true_params$estimate[["shape"]]), rate_pctile = percentile_rate(true_params$estimate[["rate"]])))
}, .progress = TRUE) %>% 
  bind_rows()

saveRDS(percentile_samples, here("bayesian_modeling", "stan_original_bootstrap.rds"))

t <- percentile_samples %>% 
  mutate(wthn = if_else(true >= CI_low & true <= CI_high, 1, 0)) %>% 
  group_by(CI, type) %>% 
  summarise(proportion = mean(wthn))


#Comparison function
# percentile_norm <- ecdf(rnorm(5000))
# norm_samples <- map(1:500, .f = function(i){
#   set.seed(i)
#   test_value <- rnorm(1)
#   return(tibble(norm_pctile = percentile_norm(test_value)))
# }) %>% 
#   bind_rows()
# 
# hist(norm_samples$norm_pctile)
