// described in https://www.youtube.com/watch?v=KOIudAB6vJ0&ab_channel=BenLambert

data {
  int K;
  int X[K];
}

transformed data {
  int n[4];
  for(i in 1:4)
    n[i] = 4 + i; // n is number of coin flips (discrete). Possible values are 5:8
}

parameters {
  real<lower=0, upper = 1> theta; //theta is p of the coin (continuous)
}

transformed parameters {
  vector[4] lq; //unnormalized log probability (q means unnormalized, p means normalized)
  for(i in 1:4)
    lq[i] = binomial_lpmf(X| n[i], theta) + log(0.25);
  // calculate the log prob mass function for every possible value of n...
}

model{
  target += log_sum_exp(lq);
  // ... and then sum the lpmf values together
  theta ~ uniform(0, 1);
}

generated quantities {
  vector[4] pState;
  pState = exp(lq - log_sum_exp(lq)); // calculates the probability of each value of n
}
