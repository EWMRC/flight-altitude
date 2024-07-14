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
  int<lower=0> n_obs_unknown_juv;
  int<lower=0> n_obs_unknown_adult;
  vector[n_obs_unknown_juv] HAT_unknown_juv;
  vector[n_obs_unknown_adult] HAT_unknown_adult;

}

transformed data { // exclusively for posterior predictive checks
  real HAT_known_mean;
  real HAT_known_sd;
  real HAT_unknown_juv_mean;
  real HAT_unknown_juv_sd;
  real HAT_unknown_adult_mean;
  real HAT_unknown_adult_sd;

  HAT_known_mean = mean(HAT_known);
  HAT_known_sd = sd(HAT_known);
  HAT_unknown_juv_mean = mean(HAT_unknown_juv);
  HAT_unknown_juv_sd = sd(HAT_unknown_juv);
  HAT_unknown_adult_mean = mean(HAT_unknown_adult);
  HAT_unknown_adult_sd = sd(HAT_unknown_adult);
}

parameters {
  real mu_bias;
  real<lower=0> sigma_error;
  real<lower=0> shape_juv;
  real<lower=0> rate_juv;
  real<lower=0> shape_adult;
  real<lower=0> rate_adult;
  real<lower=0> real_alt_juv[n_obs_unknown_juv];
  real<lower=0> real_alt_adult[n_obs_unknown_adult];
}

transformed parameters {
  //calculating likelihood of unknown states through marginalization
  real unknown_q_juv[n_obs_unknown_juv, 2];
  real unknown_q_adult[n_obs_unknown_adult, 2];
  
  //juv
  for(i in 1:n_obs_unknown_juv){
    unknown_q_juv[i, 1] = normal_lpdf(HAT_unknown_juv[i]| mu_bias, sigma_error) + log(0.67);
    unknown_q_juv[i, 2] = normal_lpdf(HAT_unknown_juv[i]| real_alt_juv[i] + mu_bias, sigma_error) + log(0.33);
  }
  
  //adult
  for(i in 1:n_obs_unknown_adult){
    unknown_q_adult[i, 1] = normal_lpdf(HAT_unknown_adult[i]| mu_bias, sigma_error) + log(0.67);
    unknown_q_adult[i, 2] = normal_lpdf(HAT_unknown_adult[i]| real_alt_adult[i] + mu_bias, sigma_error) + log(0.33);
  }
}

model {
  // likelihood of known locations
  target += normal_lpdf(HAT_known| mu_bias, sigma_error); 
  
  // likelihood of unknown locations
  for (i in 1:n_obs_unknown_juv){ 
    target += log_sum_exp(unknown_q_juv[i, 1], unknown_q_juv[i, 2]);
  }
  
  for (i in 1:n_obs_unknown_adult){ 
    target += log_sum_exp(unknown_q_adult[i, 1], unknown_q_adult[i, 2]);
  }
  
  //priors
  for(i in 1:n_obs_unknown_juv){
    real_alt_juv[i] ~ gamma(shape_juv, rate_juv);
  }
  
  for(i in 1:n_obs_unknown_adult){
    real_alt_adult[i] ~ gamma(shape_adult, rate_adult);
  }
  
  mu_bias ~ normal(0, 1); //can be negative
  sigma_error ~ uniform(0, 1); //cannot be negative
  shape_juv ~ normal(0, 5) T[0,];
  rate_juv ~ normal(0, 10) T[0,];
  shape_adult ~ normal(0, 5) T[0,];
  rate_adult ~ normal(0, 10) T[0,];
}

generated quantities {
  real p_flight_juv[n_obs_unknown_juv];
  real p_flight_adult[n_obs_unknown_adult];
  real sample_size_juv;
  real sample_size_adult;

  for (i in 1:n_obs_unknown_juv){ //probability of state 2 (flight state)
    p_flight_juv[i] = exp(unknown_q_juv[i, 2] - log_sum_exp(unknown_q_juv[i, 1], unknown_q_juv[i, 2]));
  }
  
  for (i in 1:n_obs_unknown_adult){ //probability of state 2 (flight state)
    p_flight_adult[i] = exp(unknown_q_adult[i, 2] - log_sum_exp(unknown_q_adult[i, 1], unknown_q_adult[i, 2]));
  }
  
  sample_size_juv = sum(p_flight_juv);
  sample_size_adult = sum(p_flight_adult);
  
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
  
  //unknown juv
  real<lower=0> real_alt_juv_ppc[n_obs_unknown_juv];
  vector[n_obs_unknown_juv] HAT_unknown_juv_ppc;
  real HAT_unknown_mean_juv_ppc;
  real HAT_unknown_sd_juv_ppc;
  int<lower=0, upper=1> HAT_unknown_mean_juv_gte;
  int<lower=0, upper=1> HAT_unknown_sd_juv_gte;
  int sample_size_int_juv = bin_search(round(sample_size_juv), 0, n_obs_unknown_juv);
  
  for(f in 1:n_obs_unknown_juv){
    real_alt_juv_ppc[f] = gamma_rng(shape_juv, rate_juv);
  }
  
  //unknown flight juv
  for(g in 1:sample_size_int_juv){ // number of presumed flight locations
    HAT_unknown_juv_ppc[g] = normal_rng(real_alt_juv_ppc[g] + mu_bias, sigma_error);
  }
  
  //unknown ground juv
  for(h in (sample_size_int_juv + 1):n_obs_unknown_juv){
    HAT_unknown_juv_ppc[h] = normal_rng(mu_bias, sigma_error);
  }
  
  HAT_unknown_mean_juv_ppc = mean(HAT_unknown_juv_ppc);
  HAT_unknown_sd_juv_ppc = sd(HAT_unknown_juv_ppc);
  
  HAT_unknown_mean_juv_gte = (HAT_unknown_mean_juv_ppc >= HAT_unknown_juv_mean);
  HAT_unknown_sd_juv_gte= (HAT_unknown_sd_juv_ppc >= HAT_unknown_juv_sd);
  
  //unknown adult
  real<lower=0> real_alt_adult_ppc[n_obs_unknown_adult];
  vector[n_obs_unknown_adult] HAT_unknown_adult_ppc;
  real HAT_unknown_mean_adult_ppc;
  real HAT_unknown_sd_adult_ppc;
  int<lower=0, upper=1> HAT_unknown_mean_adult_gte;
  int<lower=0, upper=1> HAT_unknown_sd_adult_gte;
  int sample_size_int_adult = bin_search(round(sample_size_adult), 0, n_obs_unknown_adult);
  
  for(f in 1:n_obs_unknown_adult){
    real_alt_adult_ppc[f] = gamma_rng(shape_adult, rate_adult);
  }
  
  //unknown flight adult
  for(g in 1:sample_size_int_adult){ // number of presumed flight locations
    HAT_unknown_adult_ppc[g] = normal_rng(real_alt_adult_ppc[g] + mu_bias, sigma_error);
  }
  
  //unknown ground adult
  for(h in (sample_size_int_adult + 1):n_obs_unknown_adult){
    HAT_unknown_adult_ppc[h] = normal_rng(mu_bias, sigma_error);
  }
  
  HAT_unknown_mean_adult_ppc = mean(HAT_unknown_adult_ppc);
  HAT_unknown_sd_adult_ppc = sd(HAT_unknown_adult_ppc);
  
  HAT_unknown_mean_adult_gte = (HAT_unknown_mean_adult_ppc >= HAT_unknown_adult_mean);
  HAT_unknown_sd_adult_gte= (HAT_unknown_sd_adult_ppc >= HAT_unknown_adult_sd);
  
}
