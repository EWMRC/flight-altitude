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
  real HAT_known_mean = mean(HAT_known);
  real HAT_known_sd = sd(HAT_known);
  real HAT_unknown_male_mean = mean(HAT_unknown_male);
  real HAT_unknown_male_sd = sd(HAT_unknown_male);
  real HAT_unknown_female_mean = mean(HAT_unknown_female);
  real HAT_unknown_female_sd = sd(HAT_unknown_female);
}

parameters {
  //observation parameters
  real<lower=0> nu_obs;
  real mu_obs;
  real<lower=0> sigma_obs;
  real<lower=0, upper=1> flight_prior_male;
  real<lower=0, upper=1> flight_prior_female;
  //process parameters (male)
  real mu_alt_male;
  real<lower=0> sigma_alt_male;
  vector<offset=mu_alt_male, multiplier=sigma_alt_male>[n_obs_unknown_male] log_real_alt_male;
  //process parameters (female)
  real mu_alt_female;
  real<lower=0> sigma_alt_female;
  vector<offset=mu_alt_female, multiplier=sigma_alt_female>[n_obs_unknown_female] log_real_alt_female;
}

transformed parameters {
  vector<lower=0>[n_obs_unknown_male] real_alt_male = exp(log_real_alt_male);
  vector<lower=0>[n_obs_unknown_female] real_alt_female = exp(log_real_alt_female);
  
  //calculating likelihood of unknown states through marginalization
  real unknown_q_male[n_obs_unknown_male, 2];
  real unknown_q_female[n_obs_unknown_female, 2];
  
  //male
  for(i in 1:n_obs_unknown_male){
    unknown_q_male[i, 1] = student_t_lpdf(HAT_unknown_male[i] | nu_obs, mu_obs, sigma_obs) + log(1 - flight_prior_male);
    unknown_q_male[i, 2] = student_t_lpdf(HAT_unknown_male[i] | nu_obs, real_alt_male[i] + mu_obs, sigma_obs) + log(flight_prior_male);
  }
  
  //female
  for(i in 1:n_obs_unknown_female){
    unknown_q_female[i, 1] = student_t_lpdf(HAT_unknown_female[i]| nu_obs, mu_obs, sigma_obs) + log(1 - flight_prior_female);
    unknown_q_female[i, 2] = student_t_lpdf(HAT_unknown_female[i]| nu_obs, real_alt_female[i] + mu_obs, sigma_obs) + log(flight_prior_female);
  }
}

model {
  // likelihood of known locations
  target += student_t_lpdf(HAT_known | nu_obs, mu_obs, sigma_obs); 
  
  // likelihood of unknown locations
  for (i in 1:n_obs_unknown_male){ 
    target += log_sum_exp(unknown_q_male[i, 1], unknown_q_male[i, 2]);
  }
  
  for (i in 1:n_obs_unknown_female){ 
    target += log_sum_exp(unknown_q_female[i, 1], unknown_q_female[i, 2]);
  }
  
  //describing the altitude distribution https://discourse.mc-stan.org/t/gamma-regression-in-stan-vs-frequentist-approach/16274/3
  log_real_alt_male ~ normal(mu_alt_male, sigma_alt_male);
  log_real_alt_female ~ normal(mu_alt_female, sigma_alt_female);
  
  //priors
  nu_obs ~ gamma(2, 0.1); //Juárez and Steel (2010) (Model-based clustering of non-Gaussian panel data based on skew-t distributions. Journal of Business & Economic Statistics 28, 52–66.)
  mu_obs ~ normal(0, 1); 
  sigma_obs ~ normal(0, 1) T[0,]; //truncation shouldn't be necessary, but including it anyway
  flight_prior_male ~ beta(2, 2); //peak at 0.5, with a mild slope towards 0 and 1
  flight_prior_female ~ beta(2, 2); //peak at 0.5, with a mild slope towards 0 and 1
  mu_alt_male ~ normal(0, 1);
  mu_alt_female ~ normal(0, 1);
  sigma_alt_male ~ normal(0, 1) T[0,];
  sigma_alt_female ~ normal(0, 1) T[0,];
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
  int sample_size_int_male = bin_search(round(sample_size_male), 0, n_obs_unknown_male);
  int sample_size_int_female = bin_search(round(sample_size_female), 0, n_obs_unknown_female);

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

  //unknown male
  vector<offset=mu_alt_male, multiplier=sigma_alt_male>[n_obs_unknown_male] log_real_alt_male_ppc;
  vector[n_obs_unknown_male] HAT_unknown_male_ppc;

  for(f in 1:n_obs_unknown_male){
    log_real_alt_male_ppc[f] = normal_rng(mu_alt_male, sigma_alt_male);
  }

  //unknown flight male
  for(g in 1:sample_size_int_male){ // number of presumed flight locations
    HAT_unknown_male_ppc[g] = student_t_rng(nu_obs, exp(log_real_alt_male_ppc[g]) + mu_obs, sigma_obs);
  }

  //unknown ground male
  for(h in (sample_size_int_male + 1):n_obs_unknown_male){
    HAT_unknown_male_ppc[h] = student_t_rng(nu_obs, mu_obs, sigma_obs);
  }
  
  //unknown female
  vector<offset=mu_alt_female, multiplier=sigma_alt_female>[n_obs_unknown_female] log_real_alt_female_ppc;
  vector[n_obs_unknown_female] HAT_unknown_female_ppc;

  for(f in 1:n_obs_unknown_female){
    log_real_alt_female_ppc[f] = normal_rng(mu_alt_female, sigma_alt_female);
  }

  //unknown flight female
  for(g in 1:sample_size_int_female){ // number of presumed flight locations
    HAT_unknown_female_ppc[g] = student_t_rng(nu_obs, exp(log_real_alt_female_ppc[g]) + mu_obs, sigma_obs);
  }

  //unknown ground female
  for(h in (sample_size_int_female + 1):n_obs_unknown_female){
    HAT_unknown_female_ppc[h] = student_t_rng(nu_obs, mu_obs, sigma_obs);
  }

  //ppc stats
  real HAT_unknown_mean_male_ppc = mean(HAT_unknown_male_ppc);
  real HAT_unknown_sd_male_ppc = sd(HAT_unknown_male_ppc);
  int<lower=0, upper=1> HAT_unknown_mean_male_gte = (HAT_unknown_mean_male_ppc >= HAT_unknown_male_mean);
  int<lower=0, upper=1> HAT_unknown_sd_male_gte = (HAT_unknown_sd_male_ppc >= HAT_unknown_male_sd);
  
  real HAT_unknown_mean_female_ppc = mean(HAT_unknown_female_ppc);
  real HAT_unknown_sd_female_ppc = sd(HAT_unknown_female_ppc);
  int<lower=0, upper=1> HAT_unknown_mean_female_gte = (HAT_unknown_mean_female_ppc >= HAT_unknown_female_mean);
  int<lower=0, upper=1> HAT_unknown_sd_female_gte = (HAT_unknown_sd_female_ppc >= HAT_unknown_female_sd);
  
}
