functions {
  int bin_search(real x, int min_val, int max_val){ //https://discourse.mc-stan.org/t/real-to-integer-conversion/5622/7
  // This assumes that min_val >= 0 is the minimum integer in range,
  //  max_val > min_val,
  // and that x has already been rounded.
  //  It should find the integer equivalent to x.
  int range = (max_val - min_val+1)/2; // We add 1 to make sure that truncation doesn't exclude a number
  int mid_pt = min_val + range;
  int out;
  while(range > 0) {
    if(x == mid_pt){
      out = mid_pt;
      range = 0;
    } else {
      // figure out if range == 1
      range =  (range+1)/2;
      mid_pt = x > mid_pt ? mid_pt + range: mid_pt - range;
    }
  }
  return out;
  }
}

data {
  int<lower=0> n_obs_known;
  vector[n_obs_known] HAT_known;
  int<lower=0> n_obs_unknown;
  vector[n_obs_unknown] HAT_unknown;
}

transformed data { // exclusively for posterior predictive checks
  real HAT_known_mean;
  real HAT_known_sd;
  real HAT_unknown_mean;
  real HAT_unknown_sd;

  HAT_known_mean = mean(HAT_known);
  HAT_known_sd = sd(HAT_known);
  HAT_unknown_mean = mean(HAT_unknown);
  HAT_unknown_sd = sd(HAT_unknown);
}

parameters {
  real mu_bias;
  real<lower=0> sigma_error;
  real<lower=0> shape;
  real<lower=0> rate;
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
  // for(i in 1:n_obs_unknown){
    //   real_alt[i] ~ gamma(shape, rate);
    // }
    
    real_alt ~ gamma(shape, rate);
    
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
  
  // posterior predictive checks following Meng 1994
  real HAT_known_ppc[n_obs_known];
  real HAT_known_mean_ppc;
  real HAT_known_sd_ppc;
  int<lower=0, upper=1> HAT_known_mean_gte;
  int<lower=0, upper=1> HAT_known_sd_gte;
  
  // known ground
  for(k in 1:n_obs_known){
    HAT_known_ppc[k] = normal_rng(mu_bias, sigma_error);
  }
  
  HAT_known_mean_ppc = mean(HAT_known_ppc);
  HAT_known_sd_ppc = sd(HAT_known_ppc);
  
  HAT_known_mean_gte = (HAT_known_mean_ppc >= HAT_known_mean);
  HAT_known_sd_gte = (HAT_known_sd_ppc >= HAT_known_sd);
  
  //unknown
  //vector[n_obs_unknown-145] HAT_unknown_ground_ppc; //using an estimated sample size from earlier versions of the model
  real<lower=0> real_alt_ppc[n_obs_unknown];
  // vector[145] HAT_unknown_flight_ppc;
  vector[n_obs_unknown] HAT_unknown_ppc;
  real HAT_unknown_mean_ppc;
  real HAT_unknown_sd_ppc;
  int<lower=0, upper=1> HAT_unknown_mean_gte;
  int<lower=0, upper=1> HAT_unknown_sd_gte;
  int sample_size_int = bin_search(round(sample_size), 0, n_obs_unknown); //integer version of flight sample size
  // int sample_size_int = 145; //integer version of flight sample size
  
  //generate potential vales of real_alt at random
  for(f in 1:n_obs_unknown){
    real_alt_ppc[f] = gamma_rng(shape, rate);
  }
  
  //unknown flight
  for(g in 1:sample_size_int){ // number of presumed flight locations
  // real_alt_ppc[g] = gamma_rng(shape, rate);
  HAT_unknown_ppc[g] = normal_rng(real_alt_ppc[g] + mu_bias, sigma_error);
  }
  
  //unknown ground
  for(h in (sample_size_int + 1):n_obs_unknown){
    HAT_unknown_ppc[h] = normal_rng(mu_bias, sigma_error);
  }
  
  //combined unknown
  // HAT_unknown_ppc = append_row(HAT_unknown_ground_ppc, HAT_unknown_flight_ppc);
  
  HAT_unknown_mean_ppc = mean(HAT_unknown_ppc);
  HAT_unknown_sd_ppc = sd(HAT_unknown_ppc);
  
  HAT_unknown_mean_gte = (HAT_unknown_mean_ppc >= HAT_unknown_mean);
  HAT_unknown_sd_gte= (HAT_unknown_sd_ppc >= HAT_unknown_sd);
  
}
