data {
  int<lower=0> n_obs_known;
  vector[n_obs_known] HAT_known;
  int<lower=0> n_obs_unknown;
  vector[n_obs_unknown] HAT_unknown;
  int age[n_obs_unknown];
}

parameters {
  real mu_bias;
  real<lower=0> sigma_error;
  real<lower=0> shape[2];
  real<lower=0> rate[2];
  real<lower=0> real_alt[n_obs_unknown];
}

transformed parameters {
  real unknown_q[n_obs_unknown, 2];
  
  for(i in 1:n_obs_unknown){
    unknown_q[i, 1] = normal_lpdf(HAT_unknown[i]| mu_bias, sigma_error) + log(0.67);
    unknown_q[i, 2] = normal_lpdf(HAT_unknown[i]| real_alt[i] + mu_bias, sigma_error) + log(0.33);
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
    real_alt[i] ~ gamma(shape[age[i]], rate[age[i]]);
  }
  
  mu_bias ~ normal(0, 1); //can be negative
  sigma_error ~ uniform(0, 1); //cannot be negative
  shape ~ normal(0, 5) T[0,];
  rate ~ normal(0, 10) T[0,];
}

generated quantities {
  real p_flight[n_obs_unknown];
  real sample_size;

  for (i in 1:n_obs_unknown){ //probability of state 2 (flight state)
    p_flight[i] = exp(unknown_q[i, 2] - log_sum_exp(unknown_q[i, 1], unknown_q[i, 2]));
  }
  
  sample_size = sum(p_flight);
  
}
