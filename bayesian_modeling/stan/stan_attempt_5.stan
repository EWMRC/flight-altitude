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
}

transformed parameters {
  real known_q; //unnormalized log probability (q means unnormalized, p means normalized)
  real unknown_q[n_obs_unknown,2];
  
  known_q = normal_lpdf(HAT_known| mu_bias, sigma_error);
  
  for(i in 1:n_obs_unknown){
    unknown_q[i, 1] = normal_lpdf(HAT_unknown[i]| mu_bias, sigma_error) + log(0.5);
    unknown_q[i, 2] = normal_lpdf(HAT_unknown[i]| mu_flight, sigma_flight) + log(0.5);
  }
}

model {
  target += known_q; // likelihood based on the known data
  for (i in 1:n_obs_unknown){ // likelihood based on the unknown data
    target += log_sum_exp(unknown_q[i, 1], unknown_q[i, 2]);
  }
  
  //priors
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
