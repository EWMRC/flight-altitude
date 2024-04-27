data {
  int<lower=0> n_obs;
  int flips[n_obs];
}

parameters {
  real<lower=0, upper=1> p;
}

transformed parameters {
  real lq[n_obs,2];
  for (i in 1:n_obs){
    lq[i, 1] = binomial_lpmf(flips[i]| 20, p) + log(0.5);
    lq[i, 2] = binomial_lpmf(flips[i]| 25, p) + log(0.5);
  }
}

model {
  for (i in 1:n_obs){
    target += log_sum_exp(lq[i, 1], lq[i, 2]);
  }
  //priors
  p ~ uniform(0, 1);
}

generated quantities {
  real pState[n_obs];
  
  for (i in 1:n_obs){
    pState[i] = exp(lq[i, 1] - log_sum_exp(lq[i, 1], lq[i, 2]));
    //probability of state 1. Probability of state 2 is the inverse.
  }
}
