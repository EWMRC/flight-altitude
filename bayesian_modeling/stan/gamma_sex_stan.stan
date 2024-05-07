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
  int<lower=0> n_obs_unknown_male;
  int<lower=0> n_obs_unknown_female;
  vector[n_obs_unknown_male] HAT_unknown_male;
  vector[n_obs_unknown_female] HAT_unknown_female;

}

transformed data { // exclusively for posterior predictive checks
  real HAT_known_mean;
  real HAT_known_sd;
  real HAT_unknown_male_mean;
  real HAT_unknown_male_sd;
  real HAT_unknown_female_mean;
  real HAT_unknown_female_sd;

  HAT_known_mean = mean(HAT_known);
  HAT_known_sd = sd(HAT_known);
  HAT_unknown_male_mean = mean(HAT_unknown_male);
  HAT_unknown_male_sd = sd(HAT_unknown_male);
  HAT_unknown_female_mean = mean(HAT_unknown_female);
  HAT_unknown_female_sd = sd(HAT_unknown_female);
}

parameters {
  real mu_bias;
  real<lower=0> sigma_error;
  real<lower=0> shape_male;
  real<lower=0> rate_male;
  real<lower=0> shape_female;
  real<lower=0> rate_female;
  real<lower=0> real_alt_male[n_obs_unknown_male];
  real<lower=0> real_alt_female[n_obs_unknown_female];
}

transformed parameters {
  //calculating likelihood of unknown states through marginalization
  real unknown_q_male[n_obs_unknown_male, 2];
  real unknown_q_female[n_obs_unknown_female, 2];
  
  //male
  for(i in 1:n_obs_unknown_male){
    unknown_q_male[i, 1] = normal_lpdf(HAT_unknown_male[i]| mu_bias, sigma_error) + log(0.67);
    unknown_q_male[i, 2] = normal_lpdf(HAT_unknown_male[i]| real_alt_male[i] + mu_bias, sigma_error) + log(0.33);
  }
  
  //female
  for(i in 1:n_obs_unknown_female){
    unknown_q_female[i, 1] = normal_lpdf(HAT_unknown_female[i]| mu_bias, sigma_error) + log(0.67);
    unknown_q_female[i, 2] = normal_lpdf(HAT_unknown_female[i]| real_alt_female[i] + mu_bias, sigma_error) + log(0.33);
  }
}

model {
  // likelihood of known locations
  target += normal_lpdf(HAT_known| mu_bias, sigma_error); 
  
  // likelihood of unknown locations
  for (i in 1:n_obs_unknown_male){ 
    target += log_sum_exp(unknown_q_male[i, 1], unknown_q_male[i, 2]);
  }
  
  for (i in 1:n_obs_unknown_female){ 
    target += log_sum_exp(unknown_q_female[i, 1], unknown_q_female[i, 2]);
  }
  
  //priors
  for(i in 1:n_obs_unknown_male){
    real_alt_male[i] ~ gamma(shape_male, rate_male);
  }
  
  for(i in 1:n_obs_unknown_female){
    real_alt_female[i] ~ gamma(shape_female, rate_female);
  }
  
  mu_bias ~ normal(0, 1); //can be negative
  sigma_error ~ uniform(0, 1); //cannot be negative
  shape_male ~ normal(0, 5) T[0,];
  rate_male ~ normal(0, 10) T[0,];
  shape_female ~ normal(0, 5) T[0,];
  rate_female ~ normal(0, 10) T[0,];
}

generated quantities {
  real p_flight_male[n_obs_unknown_male];
  real p_flight_female[n_obs_unknown_female];
  real sample_size_male;
  real sample_size_female;

  for (i in 1:n_obs_unknown_male){ //probability of state 2 (flight state)
    p_flight_male[i] = exp(unknown_q_male[i, 2] - log_sum_exp(unknown_q_male[i, 1], unknown_q_male[i, 2]));
  }
  
  for (i in 1:n_obs_unknown_female){ //probability of state 2 (flight state)
    p_flight_female[i] = exp(unknown_q_female[i, 2] - log_sum_exp(unknown_q_female[i, 1], unknown_q_female[i, 2]));
  }
  
  sample_size_male = sum(p_flight_male);
  sample_size_female = sum(p_flight_female);
  
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
  
  //unknown male
  real<lower=0> real_alt_male_ppc[n_obs_unknown_male];
  vector[n_obs_unknown_male] HAT_unknown_male_ppc;
  real HAT_unknown_mean_male_ppc;
  real HAT_unknown_sd_male_ppc;
  int<lower=0, upper=1> HAT_unknown_mean_male_gte;
  int<lower=0, upper=1> HAT_unknown_sd_male_gte;
  int sample_size_int_male = bin_search(round(sample_size_male), 0, n_obs_unknown_male);
  
  for(f in 1:n_obs_unknown_male){
    real_alt_male_ppc[f] = gamma_rng(shape_male, rate_male);
  }
  
  //unknown flight male
  for(g in 1:sample_size_int_male){ // number of presumed flight locations
    HAT_unknown_male_ppc[g] = normal_rng(real_alt_male_ppc[g] + mu_bias, sigma_error);
  }
  
  //unknown ground male
  for(h in (sample_size_int_male + 1):n_obs_unknown_male){
    HAT_unknown_male_ppc[h] = normal_rng(mu_bias, sigma_error);
  }
  
  HAT_unknown_mean_male_ppc = mean(HAT_unknown_male_ppc);
  HAT_unknown_sd_male_ppc = sd(HAT_unknown_male_ppc);
  
  HAT_unknown_mean_male_gte = (HAT_unknown_mean_male_ppc >= HAT_unknown_male_mean);
  HAT_unknown_sd_male_gte= (HAT_unknown_sd_male_ppc >= HAT_unknown_male_sd);
  
  //unknown female
  real<lower=0> real_alt_female_ppc[n_obs_unknown_female];
  vector[n_obs_unknown_female] HAT_unknown_female_ppc;
  real HAT_unknown_mean_female_ppc;
  real HAT_unknown_sd_female_ppc;
  int<lower=0, upper=1> HAT_unknown_mean_female_gte;
  int<lower=0, upper=1> HAT_unknown_sd_female_gte;
  int sample_size_int_female = bin_search(round(sample_size_female), 0, n_obs_unknown_female);
  
  for(f in 1:n_obs_unknown_female){
    real_alt_female_ppc[f] = gamma_rng(shape_female, rate_female);
  }
  
  //unknown flight male
  for(g in 1:sample_size_int_female){ // number of presumed flight locations
    HAT_unknown_female_ppc[g] = normal_rng(real_alt_female_ppc[g] + mu_bias, sigma_error);
  }
  
  //unknown ground male
  for(h in (sample_size_int_female + 1):n_obs_unknown_female){
    HAT_unknown_female_ppc[h] = normal_rng(mu_bias, sigma_error);
  }
  
  HAT_unknown_mean_female_ppc = mean(HAT_unknown_female_ppc);
  HAT_unknown_sd_female_ppc = sd(HAT_unknown_female_ppc);
  
  HAT_unknown_mean_female_gte = (HAT_unknown_mean_female_ppc >= HAT_unknown_female_mean);
  HAT_unknown_sd_female_gte= (HAT_unknown_sd_female_ppc >= HAT_unknown_female_sd);
  
}
