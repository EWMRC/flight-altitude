data {
  int<lower=0> n_obs;
  vector[n_obs] HAT;
}

parameters {
  real<lower=0> shape;
  real<lower=0> rate;
}

transformed parameters {
  vector[1] lq; //unnormalized log probability (q means unnormalized, p means normalized)

  lq[1] = gamma_lpdf(HAT| shape, rate);
  
}

model {
  target += log_sum_exp(lq);
  
  shape ~ normal(0, 5) T[0,];
  rate ~ normal(0, 10) T[0,];
}
