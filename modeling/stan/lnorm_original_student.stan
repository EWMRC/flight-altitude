functions {
  // function to convert real to int
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
  real HAT_known_mean = mean(HAT_known);
  real HAT_known_sd = sd(HAT_known);
  real HAT_unknown_mean = mean(HAT_unknown);
  real HAT_unknown_sd = sd(HAT_unknown);
}

parameters {
  real<lower=0> nu_obs;
  real mu_obs;
  real<lower=0> sigma_obs;
  real<lower=0, upper=1> flight_prior;
  
  real mu_alt;
  real<lower=0> sigma_alt;
  vector<offset=mu_alt, multiplier=sigma_alt>[n_obs_unknown] log_real_alt;
}

transformed parameters {
  vector<lower=0>[n_obs_unknown] real_alt = exp(log_real_alt);
  
  real unknown_q[n_obs_unknown, 2];
  
  for(i in 1:n_obs_unknown){ 
    // Marginalized discrete parameter (https://mc-stan.org/docs/stan-users-guide/latent-discrete.html)
    // Measuring the likelihood that a given location is in a ground or flight state
    unknown_q[i, 1] = student_t_lpdf(HAT_unknown[i] | nu_obs, mu_obs, sigma_obs) + log(1 - flight_prior);//+ log(0.67);
    unknown_q[i, 2] = student_t_lpdf(HAT_unknown[i] | nu_obs, real_alt[i] + mu_obs, sigma_obs) + log(flight_prior);//+ log(0.33);
  }
}

model {
  // likelihood of known locations
  target += student_t_lpdf(HAT_known | nu_obs, mu_obs, sigma_obs); 
  
  // likelihood of unknown locations
  for (i in 1:n_obs_unknown){ 
    target += log_sum_exp(unknown_q[i, 1], unknown_q[i, 2]);
  }
  
  //describing the altitude distribution https://discourse.mc-stan.org/t/gamma-regression-in-stan-vs-frequentist-approach/16274/3
  log_real_alt ~ normal(mu_alt, sigma_alt);
  
  //priors
  nu_obs ~ normal(0, 30) T[0,];
  mu_obs ~ normal(0, 1); 
  sigma_obs ~ normal(0, 1) T[0,]; //truncation shouldn't be necessary, but including it anyway
  flight_prior ~ beta(2, 2); //peak at 0.5, with a mild slope towards 0 and 1
  mu_alt ~ normal(0, 1);
  sigma_alt ~ normal(0, 1) T[0,];
}

generated quantities {
  //calculating probability that any given (unknown) location was recorded in flight, and total num of flight locations
  real p_flight[n_obs_unknown];
  real sample_size;

  for (i in 1:n_obs_unknown){ //probability of state 2 (flight state)
     p_flight[i] = exp(unknown_q[i, 2] - log_sum_exp(unknown_q[i, 1], unknown_q[i, 2]));
  }

  sample_size = sum(p_flight);
  int sample_size_int = bin_search(round(sample_size), 0, n_obs_unknown); //integer version of flight sample size

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

  //unknown
  vector<offset=mu_alt, multiplier=sigma_alt>[n_obs_unknown] log_real_alt_ppc;
  vector[n_obs_unknown] HAT_unknown_ppc;
  
  //generate potential vales of real_alt at random
  for(f in 1:n_obs_unknown){
    log_real_alt_ppc[f] = normal_rng(mu_alt, sigma_alt);
  }
  
  //unknown flight
  for(g in 1:sample_size_int){ // number of presumed flight locations
    HAT_unknown_ppc[g] = normal_rng(exp(log_real_alt_ppc[g]) + mu_obs, sigma_obs);
  }

  //unknown ground
  for(h in (sample_size_int + 1):n_obs_unknown){
    HAT_unknown_ppc[h] = normal_rng(mu_obs, sigma_obs);
  }

  //ppc stats
  real HAT_unknown_mean_ppc = mean(HAT_unknown_ppc);
  real HAT_unknown_sd_ppc = sd(HAT_unknown_ppc);
  int<lower=0, upper=1> HAT_unknown_mean_gte = (HAT_unknown_mean_ppc >= HAT_unknown_mean);
  int<lower=0, upper=1> HAT_unknown_sd_gte = (HAT_unknown_sd_ppc >= HAT_unknown_sd);
}
