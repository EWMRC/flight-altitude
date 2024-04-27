data {
  //int<lower=0> n_obs_known;
  //vector[n_obs_known] HAT_known;
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
  //real known_q; //unnormalized log probability (q means unnormalized, p means normalized)
  vector[2] unknown_q; //unnormalized log probability (q means unnormalized, p means normalized)
  
  //known_q = normal_lpdf(HAT_known| mu_bias, sigma_error);
  unknown_q[1] = normal_lpdf(HAT_unknown| mu_bias, sigma_error) + log(0.5);
  unknown_q[2] = normal_lpdf(HAT_unknown| mu_flight, sigma_flight) + log(0.5);
  
}

model {
  // target += known_q;
  target += log_sum_exp(unknown_q);
  
  mu_bias ~ normal(0, 10); //can be negative
  sigma_error ~ uniform(40, 60); //cannot be negative
  mu_flight ~ normal(1000, 10); //can be negative
  sigma_flight ~ uniform(40, 60); //cannot be negative
}
