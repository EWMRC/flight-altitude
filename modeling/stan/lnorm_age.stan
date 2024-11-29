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
  int<lower=0> n_obs_unknown_adult;
  int<lower=0> n_obs_unknown_juv;
  vector[n_obs_unknown_adult] HAT_unknown_adult;
  vector[n_obs_unknown_juv] HAT_unknown_juv;

}

transformed data { // exclusively for posterior predictive checks
  real HAT_known_mean = mean(HAT_known);
  real HAT_known_sd = sd(HAT_known);
  real HAT_unknown_adult_mean = mean(HAT_unknown_adult);
  real HAT_unknown_adult_sd = sd(HAT_unknown_adult);
  real HAT_unknown_juv_mean = mean(HAT_unknown_juv);
  real HAT_unknown_juv_sd = sd(HAT_unknown_juv);
}

parameters {
  //observation parameters
  real<lower=0> nu_obs;
  real mu_obs;
  real<lower=0> sigma_obs;
  real<lower=0, upper=1> flight_prior_adult;
  real<lower=0, upper=1> flight_prior_juv;
  //process parameters (adult)
  real mu_alt_adult;
  real<lower=0> sigma_alt_adult;
  vector<offset=mu_alt_adult, multiplier=sigma_alt_adult>[n_obs_unknown_adult] log_real_alt_adult;
  //process parameters (juv)
  real mu_alt_juv;
  real<lower=0> sigma_alt_juv;
  vector<offset=mu_alt_juv, multiplier=sigma_alt_juv>[n_obs_unknown_juv] log_real_alt_juv;
}

transformed parameters {
  vector<lower=0>[n_obs_unknown_adult] real_alt_adult = exp(log_real_alt_adult);
  vector<lower=0>[n_obs_unknown_juv] real_alt_juv = exp(log_real_alt_juv);
  
  //calculating likelihood of unknown states through marginalization
  real unknown_q_adult[n_obs_unknown_adult, 2];
  real unknown_q_juv[n_obs_unknown_juv, 2];
  
  //adult
  for(i in 1:n_obs_unknown_adult){
    unknown_q_adult[i, 1] = student_t_lpdf(HAT_unknown_adult[i] | nu_obs, mu_obs, sigma_obs) + log(1 - flight_prior_adult);
    unknown_q_adult[i, 2] = student_t_lpdf(HAT_unknown_adult[i] | nu_obs, real_alt_adult[i] + mu_obs, sigma_obs) + log(flight_prior_adult);
  }
  
  //juv
  for(i in 1:n_obs_unknown_juv){
    unknown_q_juv[i, 1] = student_t_lpdf(HAT_unknown_juv[i]| nu_obs, mu_obs, sigma_obs) + log(1 - flight_prior_juv);
    unknown_q_juv[i, 2] = student_t_lpdf(HAT_unknown_juv[i]| nu_obs, real_alt_juv[i] + mu_obs, sigma_obs) + log(flight_prior_juv);
  }
}

model {
  // likelihood of known locations
  target += student_t_lpdf(HAT_known | nu_obs, mu_obs, sigma_obs); 
  
  // likelihood of unknown locations
  for (i in 1:n_obs_unknown_adult){ 
    target += log_sum_exp(unknown_q_adult[i, 1], unknown_q_adult[i, 2]);
  }
  
  for (i in 1:n_obs_unknown_juv){ 
    target += log_sum_exp(unknown_q_juv[i, 1], unknown_q_juv[i, 2]);
  }
  
  //describing the altitude distribution https://discourse.mc-stan.org/t/gamma-regression-in-stan-vs-frequentist-approach/16274/3
  log_real_alt_adult ~ normal(mu_alt_adult, sigma_alt_adult);
  log_real_alt_juv ~ normal(mu_alt_juv, sigma_alt_juv);
  
  //priors
  nu_obs ~ gamma(2, 0.1); //Juárez and Steel (2010) (Model-based clustering of non-Gaussian panel data based on skew-t distributions. Journal of Business & Economic Statistics 28, 52–66.)
  mu_obs ~ normal(0, 1); 
  sigma_obs ~ normal(0, 1) T[0,]; //truncation shouldn't be necessary, but including it anyway
  flight_prior_adult ~ beta(2, 2); //peak at 0.5, with a mild slope towards 0 and 1
  flight_prior_juv ~ beta(2, 2); //peak at 0.5, with a mild slope towards 0 and 1
  mu_alt_adult ~ normal(0, 1);
  mu_alt_juv ~ normal(0, 1);
  sigma_alt_adult ~ normal(0, 1) T[0,];
  sigma_alt_juv ~ normal(0, 1) T[0,];
}

generated quantities {
  real p_flight_adult[n_obs_unknown_adult];
  real p_flight_juv[n_obs_unknown_juv];
  real sample_size_adult;
  real sample_size_juv;

  for (i in 1:n_obs_unknown_adult){ //probability of state 2 (flight state)
    p_flight_adult[i] = exp(unknown_q_adult[i, 2] - log_sum_exp(unknown_q_adult[i, 1], unknown_q_adult[i, 2]));
  }

  for (i in 1:n_obs_unknown_juv){ //probability of state 2 (flight state)
    p_flight_juv[i] = exp(unknown_q_juv[i, 2] - log_sum_exp(unknown_q_juv[i, 1], unknown_q_juv[i, 2]));
  }

  sample_size_adult = sum(p_flight_adult);
  sample_size_juv = sum(p_flight_juv);
  int sample_size_int_adult = bin_search(round(sample_size_adult), 0, n_obs_unknown_adult);
  int sample_size_int_juv = bin_search(round(sample_size_juv), 0, n_obs_unknown_juv);

  // posterior predictive checks following Meng 1994
  real HAT_known_ppc[n_obs_known];
  real HAT_known_mean_ppc = mean(HAT_known_ppc);
  real HAT_known_sd_ppc = sd(HAT_known_ppc);
  int<lower=0, upper=1> HAT_known_mean_gte = (HAT_known_mean_ppc >= HAT_known_mean);
  int<lower=0, upper=1> HAT_known_sd_gte = (HAT_known_sd_ppc >= HAT_known_sd);

  // known ground
  for(k in 1:n_obs_known){
    HAT_known_ppc[k] = student_t_rng(nu_obs, mu_obs, sigma_obs);
  }

  //unknown adult
  vector<offset=mu_alt_adult, multiplier=sigma_alt_adult>[n_obs_unknown_adult] log_real_alt_adult_ppc;
  vector[n_obs_unknown_adult] HAT_unknown_adult_ppc;

  for(f in 1:n_obs_unknown_adult){
    log_real_alt_adult_ppc[f] = normal_rng(mu_alt_adult, sigma_alt_adult);
  }

  //unknown flight adult
  for(g in 1:sample_size_int_adult){ // number of presumed flight locations
    HAT_unknown_adult_ppc[g] = student_t_rng(nu_obs, exp(log_real_alt_adult_ppc[g]) + mu_obs, sigma_obs);
  }

  //unknown ground adult
  for(h in (sample_size_int_adult + 1):n_obs_unknown_adult){
    HAT_unknown_adult_ppc[h] = student_t_rng(nu_obs, mu_obs, sigma_obs);
  }
  
  //unknown juv
  vector<offset=mu_alt_juv, multiplier=sigma_alt_juv>[n_obs_unknown_juv] log_real_alt_juv_ppc;
  vector[n_obs_unknown_juv] HAT_unknown_juv_ppc;

  for(f in 1:n_obs_unknown_juv){
    log_real_alt_juv_ppc[f] = normal_rng(mu_alt_juv, sigma_alt_juv);
  }

  //unknown flight juv
  for(g in 1:sample_size_int_juv){ // number of presumed flight locations
    HAT_unknown_juv_ppc[g] = student_t_rng(nu_obs, exp(log_real_alt_juv_ppc[g]) + mu_obs, sigma_obs);
  }

  //unknown ground juv
  for(h in (sample_size_int_juv + 1):n_obs_unknown_juv){
    HAT_unknown_juv_ppc[h] = student_t_rng(nu_obs, mu_obs, sigma_obs);
  }

  //ppc stats
  real HAT_unknown_mean_adult_ppc = mean(HAT_unknown_adult_ppc);
  real HAT_unknown_sd_adult_ppc = sd(HAT_unknown_adult_ppc);
  int<lower=0, upper=1> HAT_unknown_mean_adult_gte = (HAT_unknown_mean_adult_ppc >= HAT_unknown_adult_mean);
  int<lower=0, upper=1> HAT_unknown_sd_adult_gte = (HAT_unknown_sd_adult_ppc >= HAT_unknown_adult_sd);
  
  real HAT_unknown_mean_juv_ppc = mean(HAT_unknown_juv_ppc);
  real HAT_unknown_sd_juv_ppc = sd(HAT_unknown_juv_ppc);
  int<lower=0, upper=1> HAT_unknown_mean_juv_gte = (HAT_unknown_mean_juv_ppc >= HAT_unknown_juv_mean);
  int<lower=0, upper=1> HAT_unknown_sd_juv_gte = (HAT_unknown_sd_juv_ppc >= HAT_unknown_juv_sd);
  
}
