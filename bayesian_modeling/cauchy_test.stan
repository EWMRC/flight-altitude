data {
  int<lower=0> n_obs_known;
  vector[n_obs_known] HAT_known;
}

parameters {
  real mu_obs;
  real<lower=0> sigma_obs;
}

model {
  // likelihood of known locations
  target += cauchy_lpdf(HAT_known | mu_obs, sigma_obs); 
  
  // priors
  mu_obs ~ normal(0, 1); 
  sigma_obs ~ normal(0, 1) T[0,]; //truncation shouldn't be necessary, but including it anyway
}


generated quantities {
  vector[n_obs_known] HAT_known_ppc;
  
  // known ground
  for(k in 1:n_obs_known){
    HAT_known_ppc[k] = normal_rng(mu_obs, sigma_obs);
  }
}
