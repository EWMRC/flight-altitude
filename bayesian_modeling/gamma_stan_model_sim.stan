data {
  int<lower=0> n_obs_known;
  int<lower=0> n_obs_unknown;
  vector[n_obs_known] HAT_known;
  vector[n_obs_unknown] HAT_unknown;
}

parameters {
  real mu_bias;
  real<lower=0> sigma_error;
  vector[n_obs_unknown] real_alt;
  real<lower=0> shape;
  real<lower=0> rate;
}

transformed parameters {
  vector[1] lq_known; //unnormalized log probability (q means unnormalized, p means normalized)
  vector[2] lq_unknown;
  vector[n_obs_unknown] mu_flight;
  
  mu_flight = mu_bias + real_alt;
  
  lq_known[1] = normal_lpdf(HAT_known | mu_bias, sigma_error);

  lq_unknown[1] = normal_lpdf(HAT_unknown | mu_bias, sigma_error) + log(0.67); // the final term represents the probability of a flight/ground location, which is an unstated condition
  lq_unknown[2] = normal_lpdf(HAT_unknown | mu_flight, sigma_error) + log(0.33);
  //+ normal_lpdf(mu_bias | 0, 1) 
  // normal_lpdf(mu_bias | 0, 1) + gamma_lpdf(real_alt | shape, rate)
}

model {
  // model
  real_alt ~ gamma(shape, rate);
  
  target += log_sum_exp(lq_known);
  target += log_sum_exp(lq_unknown);
  
  // priors
  mu_bias ~ normal(0, 1); //can be negative
  sigma_error ~ uniform(0, 1); //cannot be negative
  //shape ~ uniform(0, 10);
  //rate ~ uniform(0, 20);
  shape ~ normal(0, 5) T[0,];
  rate ~ normal(0, 10) T[0,];
}

