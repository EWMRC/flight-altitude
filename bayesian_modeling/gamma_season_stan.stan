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
  int<lower=0> n_obs_unknown_fall;
  int<lower=0> n_obs_unknown_spring;
  vector[n_obs_unknown_fall] HAT_unknown_fall;
  vector[n_obs_unknown_spring] HAT_unknown_spring;

}

transformed data { // exclusively for posterior predictive checks
  real HAT_known_mean = mean(HAT_known);
  real HAT_known_sd = sd(HAT_known);
  real HAT_unknown_fall_mean = mean(HAT_unknown_fall);
  real HAT_unknown_fall_sd = sd(HAT_unknown_fall);
  real HAT_unknown_spring_mean = mean(HAT_unknown_spring);
  real HAT_unknown_spring_sd = sd(HAT_unknown_spring);
}

parameters {
  //observation parameters
  real mu_obs;
  real<lower=0> sigma_obs;
  real<lower=0, upper=1> flight_prior_fall;
  real<lower=0, upper=1> flight_prior_spring;
  //process parameters (fall)
  real mu_alt_fall;
  real<lower=0> sigma_alt_fall;
  vector<offset=mu_alt_fall, multiplier=sigma_alt_fall>[n_obs_unknown_fall] log_real_alt_fall;
  //process parameters (spring)
  real mu_alt_spring;
  real<lower=0> sigma_alt_spring;
  vector<offset=mu_alt_spring, multiplier=sigma_alt_spring>[n_obs_unknown_spring] log_real_alt_spring;
}

transformed parameters {
  vector<lower=0>[n_obs_unknown_fall] real_alt_fall = exp(log_real_alt_fall);
  vector<lower=0>[n_obs_unknown_spring] real_alt_spring = exp(log_real_alt_spring);
  
  //calculating likelihood of unknown states through marginalization
  real unknown_q_fall[n_obs_unknown_fall, 2];
  real unknown_q_spring[n_obs_unknown_spring, 2];
  
  //fall
  for(i in 1:n_obs_unknown_fall){
    unknown_q_fall[i, 1] = normal_lpdf(HAT_unknown_fall[i] | mu_obs, sigma_obs) + log(1 - flight_prior_fall);
    unknown_q_fall[i, 2] = normal_lpdf(HAT_unknown_fall[i] | real_alt_fall[i] + mu_obs, sigma_obs) + log(flight_prior_fall);
  }
  
  //spring
  for(i in 1:n_obs_unknown_spring){
    unknown_q_spring[i, 1] = normal_lpdf(HAT_unknown_spring[i]| mu_obs, sigma_obs) + log(1 - flight_prior_spring);
    unknown_q_spring[i, 2] = normal_lpdf(HAT_unknown_spring[i]| real_alt_spring[i] + mu_obs, sigma_obs) + log(flight_prior_spring);
  }
}

model {
  // likelihood of known locations
  target += normal_lpdf(HAT_known | mu_obs, sigma_obs); 
  
  // likelihood of unknown locations
  for (i in 1:n_obs_unknown_fall){ 
    target += log_sum_exp(unknown_q_fall[i, 1], unknown_q_fall[i, 2]);
  }
  
  for (i in 1:n_obs_unknown_spring){ 
    target += log_sum_exp(unknown_q_spring[i, 1], unknown_q_spring[i, 2]);
  }
  
  //describing the altitude distribution https://discourse.mc-stan.org/t/gamma-regression-in-stan-vs-frequentist-approach/16274/3
  log_real_alt_fall ~ normal(mu_alt_fall, sigma_alt_fall);
  log_real_alt_spring ~ normal(mu_alt_spring, sigma_alt_spring);
  
  //priors
  mu_obs ~ normal(0, 1); 
  sigma_obs ~ normal(0, 1) T[0,]; //truncation shouldn't be necessary, but including it anyway
  flight_prior_fall ~ beta(2, 2); //peak at 0.5, with a mild slope towards 0 and 1
  flight_prior_spring ~ beta(2, 2); //peak at 0.5, with a mild slope towards 0 and 1
  mu_alt_fall ~ normal(0, 1);
  mu_alt_spring ~ normal(0, 1);
  sigma_alt_fall ~ normal(0, 1) T[0,];
  sigma_alt_spring ~ normal(0, 1) T[0,];
}

generated quantities {
  real p_flight_fall[n_obs_unknown_fall];
  real p_flight_spring[n_obs_unknown_spring];
  real sample_size_fall;
  real sample_size_spring;

  for (i in 1:n_obs_unknown_fall){ //probability of state 2 (flight state)
    p_flight_fall[i] = exp(unknown_q_fall[i, 2] - log_sum_exp(unknown_q_fall[i, 1], unknown_q_fall[i, 2]));
  }

  for (i in 1:n_obs_unknown_spring){ //probability of state 2 (flight state)
    p_flight_spring[i] = exp(unknown_q_spring[i, 2] - log_sum_exp(unknown_q_spring[i, 1], unknown_q_spring[i, 2]));
  }

  sample_size_fall = sum(p_flight_fall);
  sample_size_spring = sum(p_flight_spring);
  int sample_size_int_fall = bin_search(round(sample_size_fall), 0, n_obs_unknown_fall);
  int sample_size_int_spring = bin_search(round(sample_size_spring), 0, n_obs_unknown_spring);

  // posterior predictive checks following Meng 1994
  real HAT_known_ppc[n_obs_known];
  real HAT_known_mean_ppc = mean(HAT_known_ppc);
  real HAT_known_sd_ppc = sd(HAT_known_ppc);
  int<lower=0, upper=1> HAT_known_mean_gte = (HAT_known_mean_ppc >= HAT_known_mean);
  int<lower=0, upper=1> HAT_known_sd_gte = (HAT_known_sd_ppc >= HAT_known_sd);

  // known ground
  for(k in 1:n_obs_known){
    HAT_known_ppc[k] = normal_rng(mu_obs, sigma_obs);
  }

  //unknown fall
  vector<offset=mu_alt_fall, multiplier=sigma_alt_fall>[n_obs_unknown_fall] log_real_alt_fall_ppc;
  vector[n_obs_unknown_fall] HAT_unknown_fall_ppc;

  for(f in 1:n_obs_unknown_fall){
    log_real_alt_fall_ppc[f] = normal_rng(mu_alt_fall, sigma_alt_fall);
  }

  //unknown flight fall
  for(g in 1:sample_size_int_fall){ // number of presumed flight locations
    HAT_unknown_fall_ppc[g] = normal_rng(exp(log_real_alt_fall_ppc[g]) + mu_obs, sigma_obs);
  }

  //unknown ground fall
  for(h in (sample_size_int_fall + 1):n_obs_unknown_fall){
    HAT_unknown_fall_ppc[h] = normal_rng(mu_obs, sigma_obs);
  }
  
  //unknown spring
  vector<offset=mu_alt_spring, multiplier=sigma_alt_spring>[n_obs_unknown_spring] log_real_alt_spring_ppc;
  vector[n_obs_unknown_spring] HAT_unknown_spring_ppc;

  for(f in 1:n_obs_unknown_spring){
    log_real_alt_spring_ppc[f] = normal_rng(mu_alt_spring, sigma_alt_spring);
  }

  //unknown flight spring
  for(g in 1:sample_size_int_spring){ // number of presumed flight locations
    HAT_unknown_spring_ppc[g] = normal_rng(exp(log_real_alt_spring_ppc[g]) + mu_obs, sigma_obs);
  }

  //unknown ground spring
  for(h in (sample_size_int_spring + 1):n_obs_unknown_spring){
    HAT_unknown_spring_ppc[h] = normal_rng(mu_obs, sigma_obs);
  }

  //ppc stats
  real HAT_unknown_mean_fall_ppc = mean(HAT_unknown_fall_ppc);
  real HAT_unknown_sd_fall_ppc = sd(HAT_unknown_fall_ppc);
  int<lower=0, upper=1> HAT_unknown_mean_fall_gte = (HAT_unknown_mean_fall_ppc >= HAT_unknown_fall_mean);
  int<lower=0, upper=1> HAT_unknown_sd_fall_gte = (HAT_unknown_sd_fall_ppc >= HAT_unknown_fall_sd);
  
  real HAT_unknown_mean_spring_ppc = mean(HAT_unknown_spring_ppc);
  real HAT_unknown_sd_spring_ppc = sd(HAT_unknown_spring_ppc);
  int<lower=0, upper=1> HAT_unknown_mean_spring_gte = (HAT_unknown_mean_spring_ppc >= HAT_unknown_spring_mean);
  int<lower=0, upper=1> HAT_unknown_sd_spring_gte = (HAT_unknown_sd_spring_ppc >= HAT_unknown_spring_sd);
  
}
