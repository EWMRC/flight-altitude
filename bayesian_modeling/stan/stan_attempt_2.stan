data {
  int<lower=0> n_obs;
  vector[n_obs] HAT;
}

parameters {
  real mu_bias;
  real<lower=0> sigma_error;
}

transformed parameters {
  vector[1] lq; //unnormalized log probability (q means unnormalized, p means normalized)

  lq[1] = normal_lpdf(HAT| mu_bias, sigma_error);
  
}

model {
  target += log_sum_exp(lq);
  
  mu_bias ~ normal(0, 1); //can be negative
  sigma_error ~ uniform(0, 1); //cannot be negative
}
