data {
  int<lower=0> n_obs;
  int flips[n_obs];
}

parameters {
  real<lower=0, upper=1> p;
}

transformed parameters {
  vector[2] lq;
  lq[1] = binomial_lpmf(flips| 20, p) + log(0.5);
  lq[2] = binomial_lpmf(flips| 25, p) + log(0.5);
}

model {
  target += log_sum_exp(lq);
  
  //priors
  p ~ uniform(0, 1);
  // p ~ normal(0.3, 0.1) T[0,1];
}

generated quantities {
  vector[2] pState;
  pState = exp(lq - log_sum_exp(lq)); // calculates the probability of each value of n
}
