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
  int<lower=0> n_obs_unknown_spring;
  int<lower=0> n_obs_unknown_fall;
  vector[n_obs_unknown_spring] HAT_unknown_spring;
  vector[n_obs_unknown_fall] HAT_unknown_fall;

}

transformed data { // exclusively for posterior predictive checks
  real HAT_known_mean;
  real HAT_known_sd;
  real HAT_unknown_spring_mean;
  real HAT_unknown_spring_sd;
  real HAT_unknown_fall_mean;
  real HAT_unknown_fall_sd;

  HAT_known_mean = mean(HAT_known);
  HAT_known_sd = sd(HAT_known);
  HAT_unknown_spring_mean = mean(HAT_unknown_spring);
  HAT_unknown_spring_sd = sd(HAT_unknown_spring);
  HAT_unknown_fall_mean = mean(HAT_unknown_fall);
  HAT_unknown_fall_sd = sd(HAT_unknown_fall);
}

parameters {
  real mu_bias;
  real<lower=0> sigma_error;
  real<lower=0> shape_spring;
  real<lower=0> rate_spring;
  real<lower=0> shape_fall;
  real<lower=0> rate_fall;
  real<lower=0> real_alt_spring[n_obs_unknown_spring];
  real<lower=0> real_alt_fall[n_obs_unknown_fall];
}

transformed parameters {
  //calculating likelihood of unknown states through marginalization
  real unknown_q_spring[n_obs_unknown_spring, 2];
  real unknown_q_fall[n_obs_unknown_fall, 2];
  
  //spring
  for(i in 1:n_obs_unknown_spring){
    unknown_q_spring[i, 1] = normal_lpdf(HAT_unknown_spring[i]| mu_bias, sigma_error) + log(0.67);
    unknown_q_spring[i, 2] = normal_lpdf(HAT_unknown_spring[i]| real_alt_spring[i] + mu_bias, sigma_error) + log(0.33);
  }
  
  //fall
  for(i in 1:n_obs_unknown_fall){
    unknown_q_fall[i, 1] = normal_lpdf(HAT_unknown_fall[i]| mu_bias, sigma_error) + log(0.67);
    unknown_q_fall[i, 2] = normal_lpdf(HAT_unknown_fall[i]| real_alt_fall[i] + mu_bias, sigma_error) + log(0.33);
  }
}

model {
  // likelihood of known locations
  target += normal_lpdf(HAT_known| mu_bias, sigma_error); 
  
  // likelihood of unknown locations
  for (i in 1:n_obs_unknown_spring){ 
    target += log_sum_exp(unknown_q_spring[i, 1], unknown_q_spring[i, 2]);
  }
  
  for (i in 1:n_obs_unknown_fall){ 
    target += log_sum_exp(unknown_q_fall[i, 1], unknown_q_fall[i, 2]);
  }
  
  //priors
  for(i in 1:n_obs_unknown_spring){
    real_alt_spring[i] ~ gamma(shape_spring, rate_spring);
  }
  
  for(i in 1:n_obs_unknown_fall){
    real_alt_fall[i] ~ gamma(shape_fall, rate_fall);
  }
  
  mu_bias ~ normal(0, 1); //can be negative
  sigma_error ~ uniform(0, 1); //cannot be negative
  shape_spring ~ normal(0, 5) T[0,];
  rate_spring ~ normal(0, 10) T[0,];
  shape_fall ~ normal(0, 5) T[0,];
  rate_fall ~ normal(0, 10) T[0,];
}

generated quantities {
  real p_flight_spring[n_obs_unknown_spring];
  real p_flight_fall[n_obs_unknown_fall];
  real sample_size_spring;
  real sample_size_fall;

  for (i in 1:n_obs_unknown_spring){ //probability of state 2 (flight state)
    p_flight_spring[i] = exp(unknown_q_spring[i, 2] - log_sum_exp(unknown_q_spring[i, 1], unknown_q_spring[i, 2]));
  }
  
  for (i in 1:n_obs_unknown_fall){ //probability of state 2 (flight state)
    p_flight_fall[i] = exp(unknown_q_fall[i, 2] - log_sum_exp(unknown_q_fall[i, 1], unknown_q_fall[i, 2]));
  }
  
  sample_size_spring = sum(p_flight_spring);
  sample_size_fall = sum(p_flight_fall);
  
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
  
  //unknown spring
  real<lower=0> real_alt_spring_ppc[n_obs_unknown_spring];
  vector[n_obs_unknown_spring] HAT_unknown_spring_ppc;
  real HAT_unknown_mean_spring_ppc;
  real HAT_unknown_sd_spring_ppc;
  int<lower=0, upper=1> HAT_unknown_mean_spring_gte;
  int<lower=0, upper=1> HAT_unknown_sd_spring_gte;
  int sample_size_int_spring = bin_search(round(sample_size_spring), 0, n_obs_unknown_spring);
  
  for(f in 1:n_obs_unknown_spring){
    real_alt_spring_ppc[f] = gamma_rng(shape_spring, rate_spring);
  }
  
  //unknown flight spring
  for(g in 1:sample_size_int_spring){ // number of presumed flight locations
    HAT_unknown_spring_ppc[g] = normal_rng(real_alt_spring_ppc[g] + mu_bias, sigma_error);
  }
  
  //unknown ground spring
  for(h in (sample_size_int_spring + 1):n_obs_unknown_spring){
    HAT_unknown_spring_ppc[h] = normal_rng(mu_bias, sigma_error);
  }
  
  HAT_unknown_mean_spring_ppc = mean(HAT_unknown_spring_ppc);
  HAT_unknown_sd_spring_ppc = sd(HAT_unknown_spring_ppc);
  
  HAT_unknown_mean_spring_gte = (HAT_unknown_mean_spring_ppc >= HAT_unknown_spring_mean);
  HAT_unknown_sd_spring_gte= (HAT_unknown_sd_spring_ppc >= HAT_unknown_spring_sd);
  
  //unknown fall
  real<lower=0> real_alt_fall_ppc[n_obs_unknown_fall];
  vector[n_obs_unknown_fall] HAT_unknown_fall_ppc;
  real HAT_unknown_mean_fall_ppc;
  real HAT_unknown_sd_fall_ppc;
  int<lower=0, upper=1> HAT_unknown_mean_fall_gte;
  int<lower=0, upper=1> HAT_unknown_sd_fall_gte;
  int sample_size_int_fall = bin_search(round(sample_size_fall), 0, n_obs_unknown_fall);
  
  for(f in 1:n_obs_unknown_fall){
    real_alt_fall_ppc[f] = gamma_rng(shape_fall, rate_fall);
  }
  
  //unknown flight fall
  for(g in 1:sample_size_int_fall){ // number of presumed flight locations
    HAT_unknown_fall_ppc[g] = normal_rng(real_alt_fall_ppc[g] + mu_bias, sigma_error);
  }
  
  //unknown ground fall
  for(h in (sample_size_int_fall + 1):n_obs_unknown_fall){
    HAT_unknown_fall_ppc[h] = normal_rng(mu_bias, sigma_error);
  }
  
  HAT_unknown_mean_fall_ppc = mean(HAT_unknown_fall_ppc);
  HAT_unknown_sd_fall_ppc = sd(HAT_unknown_fall_ppc);
  
  HAT_unknown_mean_fall_gte = (HAT_unknown_mean_fall_ppc >= HAT_unknown_fall_mean);
  HAT_unknown_sd_fall_gte= (HAT_unknown_sd_fall_ppc >= HAT_unknown_fall_sd);
  
}
