data {
  int<lower=0> n_obs_known;
  vector[n_obs_known] HAT_known;
  int<lower=0> n_obs_unknown;
  vector[n_obs_unknown] HAT_unknown;
}

parameters {
  real mu_bias;
  real<lower=0> sigma_error;
  real mu_flight;
  real<lower=0> sigma_flight;
  real real_alt[n_obs_unknown];
}

transformed parameters {
  real unknown_q[n_obs_unknown, 2];
  
  for(i in 1:n_obs_unknown){
    unknown_q[i, 1] = normal_lpdf(HAT_unknown[i]| mu_bias, sigma_error) + log(0.5);
    unknown_q[i, 2] = normal_lpdf(HAT_unknown[i]| real_alt[i] + mu_bias, sigma_error) + log(0.5);
  }
}

model {
  // likelihood of known locations
  target += normal_lpdf(HAT_known| mu_bias, sigma_error); 
  
  // likelihood of unknown locations
  for (i in 1:n_obs_unknown){ 
    target += log_sum_exp(unknown_q[i, 1], unknown_q[i, 2]);
  }
  
  //priors
  for(i in 1:n_obs_unknown){
    real_alt[i] ~ normal(mu_flight, sigma_flight);
  }
  mu_bias ~ normal(0, 1000); //can be negative
  sigma_error ~ uniform(0, 2000); //cannot be negative
  mu_flight ~ normal(1000, 500); //can be negative
  sigma_flight ~ uniform(0, 2000); //cannot be negative
}

generated quantities {
  real pState[n_obs_unknown];

  for (i in 1:n_obs_unknown){
    pState[i] = exp(unknown_q[i, 1] - log_sum_exp(unknown_q[i, 1], unknown_q[i, 2]));
    //probability of state 1. Probability of state 2 is the inverse.
  }
}
